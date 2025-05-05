use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::{Arc, RwLock}
};

use log::{debug, error, trace, warn};
use serde::de::DeserializeOwned;
use serde_json::Value;

use crate::{
    core::{functions::{pretty_print::PrettyPrintFunction, ModFunction, ModifyFunction}, processors::plugin}, ParserError, WorldInfoError, WorldInfoNode
};

// --- PluginBridge Trait ---
pub trait PluginBridge: Clone + Debug {
    type PluginId: Copy + DeserializeOwned + Debug;
    fn invoke_plugin(
        &self,
        plugin_id: Self::PluginId,
        properties: serde_json::Value,
    ) -> Result<String, WorldInfoError>;
}

pub struct WorldInfoRegistry<P: PluginBridge + 'static> {
    processor_factories: RwLock<HashMap<String, Box<dyn WorldInfoProcessorFactory<P>>>>,
    variables: RwLock<HashMap<String, serde_json::Value>>,
    activation_stack: RwLock<HashSet<String>>,
    functions: RwLock<HashMap<String, Arc<dyn ModFunction<P>>>>,
    plugin_bridge: Arc<P>,
}

#[derive(Clone)]
pub struct ScopedRegistry<'a, P: PluginBridge + 'static> {
    inner: &'a WorldInfoRegistry<P>,
    allowed_scopes: &'a [String],
    scope_id: String,
}

impl<'a, P: PluginBridge + 'static> ScopedRegistry<'a, P> {
    // This method remains the same conceptually
    pub fn instantiate_processor(&self, name: &str, props: &serde_json::Value) -> Option<Box<dyn WorldInfoNode>> {
        self.inner.instantiate_processor(name, props)
    }

    // Expose plugin bridge access
    pub fn plugin_bridge(&self) -> &P {
        self.inner.plugin_bridge()
    }

    // Expose scope_id if needed by functions/processors
    pub fn scope_id(&self) -> &str {
        &self.scope_id
    }
}

// --- Resolver Traits  ---
pub trait VariableResolver {
    fn get_variable(
        &self,
        name: &str,
        loop_context: Option<&HashMap<String, Value>>,
    ) -> Result<Value, ParserError>;
    fn insert_variable(&self, name: &str, value: Value) -> Result<(), ParserError>;
    fn update_variable(&self, name: &str, value: Value) -> Result<(), ParserError>;
    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError>;
}

pub trait ActivationResolver {
    fn push_activation(&self, id: &str) -> Result<(), ParserError>;
}

pub trait FunctionResolver {
    fn call_function(&self, name: &str, args: Vec<Value>) -> Result<Value, ParserError>;
}

// --- ScopedRegistry Implementations ---

impl<'a, P: PluginBridge + Debug> VariableResolver for ScopedRegistry<'a, P> {
    fn get_variable(
        &self,
        full_name: &str,
        loop_context: Option<&HashMap<String, Value>>,
    ) -> Result<Value, ParserError> {
        trace!(
            "ScopedRegistry::get_variable: full_name='{}', scope_id='{}', loop_context is {}",
            full_name,
            self.scope_id,
            if loop_context.is_some() { "Some" } else { "None" }
        );

        // --- Loop Context Check ---
        if let Some(context) = loop_context {
            if !full_name.contains(':') {
                if let Some(value) = context.get(full_name) {
                    debug!("Found variable '{}' in loop context (unscoped)", full_name);
                    return Ok(value.clone());
                }
                trace!("Variable '{}' not found in loop context (unscoped), proceeding to registry lookup.", full_name);
            } else {
                 match self.resolve_scope(full_name) {
                    Ok((ref scope, ref name)) if scope == &self.scope_id => {
                        if let Some(value) = context.get(name) {
                            debug!("Found variable '{}:{}' (resolved to local) in loop context", scope, name);
                            return Ok(value.clone());
                        }
                        trace!("Variable '{}:{}' (resolved to local) not found in loop context, proceeding to registry lookup.", scope, name);
                    }
                    Ok((scope, name)) => {
                        trace!("Variable '{}:{}' resolved to non-local scope, ignoring loop context.", scope, name);
                    }
                    Err(e) => {
                        warn!("Error resolving scope for '{}' during loop context check: {:?}. Proceeding to registry lookup.", full_name, e);
                    }
                }
            }
        } else {
            trace!("No loop context provided for variable lookup '{}'.", full_name);
        }
        // --- End Loop Context Check ---


        let (scope, name) = self.resolve_scope(full_name)?;
        trace!(
            "Registry lookup: Resolved scope='{}', name='{}'",
            scope,
            name
        );

        // Permission check
        if !self.allowed_scopes.contains(&scope) && scope != "global" { 
            error!(
                "Permission denied for scope '{}' trying to access variable '{}' from scope '{}'",
                scope, name, self.scope_id
            );
            return Err(ParserError::InsufficientPermissions(format!(
                "Scope '{}' has no read access to scope `{}`",
                self.scope_id, scope
            )));
        }

        let key = format!("{}:{}", scope, name);
        trace!("Looking up key '{}' in inner variables", key);

        let variables = self.inner.variables.read().map_err(|_| {
            error!("Failed to acquire read lock on variables");
            ParserError::Internal("Variable lock poisoned".to_string())
        })?;

        variables.get(&key).cloned().ok_or_else(|| {
            warn!(
                "Variable '{}' not found in registry (key: '{}')",
                full_name, key
            );
            ParserError::UndefinedVariable(full_name.to_string())
        })
    }

    fn insert_variable(&self, full_name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        let key = format!("{}:{}", scope, name);
        trace!("ScopedRegistry::insert_variable: key='{}'", key);

        // Permission check for insertion
        if scope != "global" && !self.allowed_scopes.contains(&scope) {
            error!(
                "Permission denied for scope '{}' trying to insert variable '{}' into scope '{}'",
                self.scope_id, name, scope
            );
            return Err(ParserError::InsufficientPermissions(format!(
                "Scope '{}' has no write access to scope `{}`",
                self.scope_id, scope
            )));
        }

        let mut variables = self.inner.variables.write().map_err(|_| {
            error!("Failed to acquire write lock on variables for insert");
            ParserError::Internal("Variable lock poisoned".to_string())
        })?;

        if variables.contains_key(&key) {
            error!("Variable '{}' already exists in scope '{}'", name, scope);
            Err(ParserError::VariableAlreadyExists(key))
        } else {
            debug!("Inserting variable '{}' into scope '{}'", name, scope);
            variables.insert(key, value);
            Ok(())
        }
    }

    fn update_variable(&self, full_name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        trace!(
            "ScopedRegistry::update_variable: scope='{}', name='{}'",
            scope,
            name
        );

        // Permission check for update
        if scope != "global" && !self.allowed_scopes.contains(&scope) {
            error!(
                "Permission denied for scope '{}' trying to update variable '{}' in scope '{}'",
                 self.scope_id, name, scope
            );
            return Err(ParserError::InsufficientPermissions(format!(
                "Scope '{}' has no write access to scope `{}`",
                self.scope_id, scope
            )));
        }

        let key = format!("{}:{}", scope, name);

        
        let mut variables = self.inner.variables.write().map_err(|_| {
            error!("Failed to acquire write lock on variables for update");
            ParserError::Internal("Variable lock poisoned".to_string())
        })?;

        if let Some(slot) = variables.get_mut(&key) {
            debug!("Updating variable '{}' in scope '{}'", name, scope);
            *slot = value;
            Ok(())
        } else {
            error!(
                "Attempted to update undefined variable '{}' in scope '{}' (key: '{}')",
                name, scope, key
            );
            Err(ParserError::UndefinedVariable(key))
        }
    }

    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError> {
        if let Some((scope, name_part)) = name.split_once(':') {
            match scope {
                // Treat "entry" and "local" explicitly as the current scope_id
                "entry" | "local" => Ok((self.scope_id.clone(), name_part.to_string())),
                // Other scopes are passed through as is
                _ => Ok((scope.to_string(), name_part.to_string())),
            }
        } else {
            // If no colon, assume it's meant to be local within this scope
            trace!(
                "Resolving unscoped name '{}' as local to scope '{}'",
                name,
                self.scope_id
            );
            Ok((self.scope_id.clone(), name.to_string()))
        }
    }
}

impl<'a, P: PluginBridge + Debug> ActivationResolver for ScopedRegistry<'a, P> {
    fn push_activation(&self, id: &str) -> Result<(), ParserError> {
        trace!("ScopedRegistry::push_activation: id='{}'", id);
        self.inner
            .activation_stack
            .write()
            .map_err(|_| {
                error!("Failed to acquire write lock on activation stack");
                ParserError::Internal("Activation stack lock poisoned".to_string())
            })?
            .insert(id.to_string()); // Assuming duplicates are okay or handled by HashSet nature
        Ok(())
    }
}

// --- FunctionResolver Implementation for ScopedRegistry ---
impl<'a, P: PluginBridge + Debug> FunctionResolver for ScopedRegistry<'a, P> {
    fn call_function(&self, name: &str, args: Vec<Value>) -> Result<Value, ParserError> {
        trace!("ScopedRegistry::call_function: name='{}'", name);

        // 1. Look up the function in the inner registry
        let function = { // Scope the read lock
            let functions = self.inner.functions.read().map_err(|_| {
                error!("Failed to acquire read lock on functions");
                ParserError::Internal("Function lock poisoned".to_string())
            })?;
            functions.get(name).cloned() // Clone the Arc
        };

        if let Some(func) = function {
            // 2. Validate arguments (optional here, could be done by caller)
            if let Err(validation_err) = func.validate_args(&args) {
                error!("Argument validation failed for function '{}': {}", name, validation_err);
                return Err(ParserError::FunctionArgsError(validation_err));
            }

            // 3. Call the function, passing the current ScopedRegistry (&self)
            // This works because func.call now takes &ScopedRegistry
            debug!("Calling function '{}'", name);
            func.call(args, self) // Pass self (which is &ScopedRegistry)
        } else {
            error!("Attempted to call undefined function '{}'", name);
            Err(ParserError::UndefinedFunction(name.to_string()))
        }
    }
}


// --- WorldInfoRegistry Methods ---
impl<P: PluginBridge + 'static> WorldInfoRegistry<P> {
    pub fn new(plugin_bridge: Arc<P>) -> Self {
        Self {
            processor_factories: RwLock::new(HashMap::new()),
            plugin_bridge,
            variables: RwLock::new(HashMap::new()),
            activation_stack: RwLock::new(HashSet::new()),
            functions: RwLock::new(load_functions()),
        }
    }

    // --- Processor Registration ---
    pub fn register_processor(&self, name: &str, factory: Box<dyn WorldInfoProcessorFactory<P>>) {
        self.processor_factories.write().unwrap().insert(name.to_string(), factory);
    }

    pub fn register_plugin_processor(&self, author: &str, name: &str) {
        self.register_processor(format!("weaver.plugin.{}.{}", author, name).as_str(), Box::new(plugin::PluginProcessorFactory));
    }

    pub fn instantiate_processor(&self, name: &str, props: &serde_json::Value) -> Option<Box<dyn WorldInfoNode>> {
        let factories = self.processor_factories.read().unwrap();
        factories.get(name).map(|factory| factory.create(props, &self.plugin_bridge))
    }
    // --- End Processor ---


    pub fn plugin_bridge(&self) -> &P {
        &self.plugin_bridge
    }

    /// Registers a function implementation.
    pub fn register_function(&self, function: Arc<dyn ModFunction<P>>) {
        let (name, _) = function.signature();
        debug!("Registering function: {}", name);
        self.functions.write().unwrap().insert(name, function);
    }

    /// Used for initial variable registration (Bypasses scope checks).
    /// Needs write access.
    pub fn register_variable(&self, var_key: String, value: serde_json::Value) {
        debug!("Registering initial variable: {}", var_key);
        // Use write lock here
        self.variables.write().unwrap().insert(var_key, value);
    }

    /// Creates a ScopedRegistry for a specific context.
    pub fn scoped_registry<'a>(
        &'a self, // Now takes &self
        allowed_scopes: &'a [String],
        scope_id: String,
    ) -> ScopedRegistry<'a, P> {
        ScopedRegistry {
            inner: self, // Pass the immutable reference
            allowed_scopes,
            scope_id,
        }
    }

    /// Returns a copy of the activation stack, clearing it.
    /// Needs write access.
    pub fn drain_activation_stack(&self) -> HashSet<String> {
        // Use write lock to clear
        let mut stack_guard = self.activation_stack.write().unwrap();
        let stack = stack_guard.drain().collect(); // Drain consumes the items
        trace!("Drained activation stack: {:?}", stack);
        stack
    }

    // --- Direct Variable Access ---
    pub fn get_global_variable(&self, name: &str) -> Result<Value, ParserError> {
        let key = format!("global:{}", name);
        self.variables.read().unwrap()
        .get(&key)
        .cloned()
        .ok_or_else(|| ParserError::UndefinedVariable(key))
    }

    pub fn set_global_variable(&self, name: &str, value: Value) -> Result<(), ParserError> {
        let key = format!("global:{}", name);
        self.variables.write().unwrap().insert(key, value);
        Ok(())
    }
}


pub trait WorldInfoProcessorFactory<P: PluginBridge + 'static>: Send + Sync + 'static {
    fn create(&self, properties: &serde_json::Value, bridge: &Arc<P>) -> Box<dyn WorldInfoNode>;
}

fn load_functions<P: PluginBridge + 'static>() -> HashMap<String, Arc<dyn ModFunction<P>>> {
    let mut functions = HashMap::new();
    functions.insert("modify".to_string(), Arc::new(ModifyFunction::new()) as Arc<dyn ModFunction<P>>);
    functions.insert("pretty_print".to_string(), Arc::new(PrettyPrintFunction::new()) as Arc<dyn ModFunction<P>>);
    functions
}