use std::{collections::HashMap, sync::{Arc, RwLock}};
use log::{debug, error, trace, warn};
use serde::de::DeserializeOwned;
use serde_json::Value;
use crate::{ParserError, WorldInfoNode, WorldInfoProcessor};
use std::fmt::Debug;

pub struct WorldInfoRegistry<P: PluginBridge + 'static> {
    processor_factories: RwLock<HashMap<String, Box<dyn WorldInfoProcessorFactory<P>>>>,
    plugin_bridge: Arc<P>,
    variables: HashMap<String, serde_json::Value>
}

pub struct ScopedRegistry<'a, P: PluginBridge + 'static> {
    inner: &'a mut WorldInfoRegistry<P>,
    allowed_scopes: &'a [String],
    scope_id: String
}

impl <'a, P: PluginBridge + 'static> ScopedRegistry<'a, P> {
    pub fn instantiate_processor(&self, name: &str, props: &serde_json::Value) -> Option<Box<dyn WorldInfoNode>> {
        self.inner.instantiate_processor(name, props)
    }
}

pub trait VariableResolver {
    /// Gets a variable's value, potentially checking a loop context first.
    fn get_variable(&self, name: &str, loop_context: Option<&HashMap<String, Value>>) -> Result<Value, ParserError>;
    /// Inserts a new variable. Loop context is not relevant here.
    fn insert_variable(&mut self, name: &str, value: Value) -> Result<(), ParserError>;
    /// Updates an existing variable. Loop context is not relevant here.
    fn update_variable(&mut self, name: &str, value: Value) -> Result<(), ParserError>;
    /// Resolves implicit scopes (e.g., `entry:foo` -> `<scope_id>:foo`).
    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError>;
}

impl<'a, P: PluginBridge> VariableResolver for ScopedRegistry<'a, P> {
    fn get_variable(&self, full_name: &str, loop_context: Option<&HashMap<String, Value>>) -> Result<Value, ParserError> {
        trace!("ScopedRegistry::get_variable: full_name='{}', loop_context is {}", full_name, if loop_context.is_some() { "Some" } else { "None" });

        // Check loop context first for unscoped or potentially local-scoped variables
        if let Some(context) = loop_context {
            if !full_name.contains(':') { // Unscoped name check
                if let Some(value) = context.get(full_name) {
                    debug!("Found variable '{}' in loop context (unscoped)", full_name);
                    return Ok(value.clone());
                }
                // If not in loop context, fall through to default scope resolution below
                trace!("Variable '{}' not found in loop context (unscoped), proceeding to registry lookup.", full_name);
            } else {
                // Scoped name check: See if it resolves to local scope and exists in context
                match self.resolve_scope(full_name) {
                    Ok((ref scope, ref name)) if scope == &self.scope_id => {
                        // Resolved scope matches the current local scope_id
                        if let Some(value) = context.get(name) {
                            debug!("Found variable '{}:{}' (resolved to local) in loop context", scope, name);
                            return Ok(value.clone());
                        }
                        trace!("Variable '{}:{}' (resolved to local) not found in loop context, proceeding to registry lookup.", scope, name);
                    }
                    Ok((scope, name)) => {
                        // Resolved to a non-local scope, ignore loop context
                        trace!("Variable '{}:{}' resolved to non-local scope, ignoring loop context.", scope, name);
                    }
                    Err(_) => {
                        // If resolve_scope fails (e.g., invalid format), fall through to registry lookup which will also likely fail
                        trace!("Could not resolve scope for '{}', proceeding to registry lookup.", full_name);
                    }
                }
            }
        } else {
            trace!("No loop context provided for variable lookup '{}'.", full_name);
        }

        // Proceed with normal registry lookup if not found in loop context or context not applicable
        let (scope, name) = self.resolve_scope(full_name)?;
        trace!("Registry lookup: Resolved scope='{}', name='{}'", scope, name);
        if !self.allowed_scopes.contains(&scope) {
            error!("Permission denied for scope '{}' trying to access variable '{}'", scope, name);
            return Err(ParserError::InsufficientPermissions(
                format!("No access to scope `{}`", scope)
            ));
        }
        let key = format!("{}:{}", scope, name);
        trace!("Looking up key '{}' in inner variables", key);
        self.inner.variables // Assuming inner is the WorldInfoRegistry
            .get(&key)
            .cloned()
            .ok_or_else(|| {
                warn!("Variable '{}' not found in registry (key: '{}')", name, key);
                ParserError::UndefinedVariable(name) // Use the resolved name part
            })
    }

    // insert_variable, update_variable, resolve_scope remain the same as provided in the prompt
    // ... (rest of ScopedRegistry impl VariableResolver) ...
     fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError> {
        let (scope, name_part) = name.split_once(':').ok_or_else(|| {
            // If no colon, assume it's meant to be local within this scope
            trace!("Resolving unscoped name '{}' as local to scope '{}'", name, self.scope_id);
            (self.scope_id.clone(), name.to_string())
        })
        .map_err(|_| ParserError::Evaluation(format!("Invalid variable name: {}", name)))?;

        match scope {
            // Treat "entry" and "local" explicitly as the current scope_id
            "entry" | "local" => Ok((self.scope_id.clone(), name_part.to_string())),
            // Other scopes are passed through as is
            _ => Ok((scope.to_string(), name_part.to_string()))
        }
    }

    fn insert_variable(&mut self, full_name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        let key = format!("{}:{}", scope, name);
        trace!("ScopedRegistry::insert_variable: key='{}'", key);

        match scope.as_str() {
            "global" => {
                if self.inner.variables.contains_key(&key) {
                    Err(ParserError::VariableAlreadyExists(name))
                } else {
                    self.inner.variables.insert(key, value); // Directly access inner registry
                    Ok(())
                }
            }
            _ => {
                // Check if the resolved scope is allowed for insertion
                if !self.allowed_scopes.contains(&scope) {
                    error!("Permission denied to insert into scope '{}' for variable '{}'", scope, name);
                    Err(ParserError::InsufficientPermissions(
                        format!("No permission to insert into scope `{}`", scope)
                    ))
                } else if self.inner.variables.contains_key(&key) {
                    error!("Variable '{}' already exists in scope '{}'", name, scope);
                    Err(ParserError::VariableAlreadyExists(name))
                } else {
                    debug!("Inserting variable '{}' into scope '{}'", name, scope);
                    self.inner.variables.insert(key, value);
                    Ok(())
                }
            }
        }
    }

    fn update_variable(&mut self, full_name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        trace!("ScopedRegistry::update_variable: scope='{}', name='{}'", scope, name);

        // Allow updating global scope, but check permissions for others
        if scope != "global" && !self.allowed_scopes.contains(&scope) {
            error!("Permission denied to update variable in scope '{}' for variable '{}'", scope, name);
            return Err(ParserError::InsufficientPermissions(
                format!("No permission to update in scope `{}`", scope)
            ));
        }

        let key = format!("{}:{}", scope, name);
        if let Some(slot) = self.inner.variables.get_mut(&key) {
            debug!("Updating variable '{}' in scope '{}'", name, scope);
            *slot = value;
            Ok(())
        } else {
            error!("Attempted to update undefined variable '{}' in scope '{}'", name, scope);
            Err(ParserError::UndefinedVariable(name))
        }
    }
}

impl<P: PluginBridge + 'static> VariableResolver for WorldInfoRegistry<P> {
    // WorldInfoRegistry ignores loop context as it represents the global state
    fn get_variable(&self, name: &str, _loop_context: Option<&HashMap<String, Value>>) -> Result<Value, ParserError> {
        trace!("WorldInfoRegistry::get_variable: name='{}', loop_context ignored", name);
        let (scope, name_part) = self.resolve_scope(name)?; // Use name_part to avoid shadowing
        let key = format!("{}:{}", scope, name_part);
        trace!("WorldInfoRegistry lookup: key='{}'", key);
        self.variables
            .get(&key)
            .cloned()
            .ok_or_else(|| {
                warn!("WorldInfoRegistry: Variable '{}' (key: '{}') not found.", name_part, key);
                ParserError::UndefinedVariable(name_part)
            })
    }

    // insert_variable, update_variable, resolve_scope remain the same as provided
    // ... (rest of WorldInfoRegistry impl VariableResolver) ...

     fn insert_variable(&mut self, name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name_part) = self.resolve_scope(name)?;
        let key = format!("{}:{}", scope, name_part);
        trace!("WorldInfoRegistry::insert_variable: key='{}'", key);

        if self.variables.contains_key(&key) {
            error!("WorldInfoRegistry: Variable '{}' already exists in scope '{}'", name_part, scope);
            Err(ParserError::VariableAlreadyExists(name_part))
        } else {
            debug!("WorldInfoRegistry: Inserting variable '{}' into scope '{}'", name_part, scope);
            self.variables.insert(key, value);
            Ok(())
        }
    }

    fn update_variable(&mut self, name: &str, value: Value) -> Result<(), ParserError> {
        let (scope, name_part) = self.resolve_scope(name)?;
        let key = format!("{}:{}", scope, name_part);
        trace!("WorldInfoRegistry::update_variable: key='{}'", key);

        if let Some(slot) = self.variables.get_mut(&key) {
            debug!("WorldInfoRegistry: Updating variable '{}' in scope '{}'", name_part, scope);
            *slot = value;
            Ok(())
        } else {
            error!("WorldInfoRegistry: Attempted to update undefined variable '{}' in scope '{}'", name_part, scope);
            Err(ParserError::UndefinedVariable(name_part))
        }
    }

     fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError> {
        // WorldInfoRegistry can only resolve global scope directly
        let (scope, name_part) = name.split_once(':').ok_or_else(|| {
            error!("WorldInfoRegistry::resolve_scope: Unscoped variable access attempted: '{}'. Only 'global:' scope allowed.", name);
            ParserError::InsufficientPermissions(format!("WorldInfoRegistry cannot access unscoped variable: {}. Use 'global:' prefix.", name))
        })?;

        match scope {
            "global" => Ok(("global".to_string(), name_part.to_string())),
            _ => {
                error!("WorldInfoRegistry::resolve_scope: Non-global scope access attempted: '{}:{}'. Only 'global:' scope allowed.", scope, name_part);
                Err(ParserError::InsufficientPermissions(format!("WorldInfoRegistry cannot access scope '{}'. Use 'global:' prefix.", scope)))
            }
        }
    }
}

impl<P: PluginBridge + 'static> WorldInfoRegistry<P> {
    pub fn new(plugin_bridge: Arc<P>) -> Self {
        Self {
            processor_factories: RwLock::new(HashMap::new()),
            plugin_bridge,
            variables: HashMap::new()
        }
    }

    pub fn register_processor(&self, name: &str, factory: Box<dyn WorldInfoProcessorFactory<P>>) {
        self.processor_factories.write().unwrap().insert(name.to_string(), factory);
    }

    pub fn register_plugin_processor(&self, author: &str, name: &str) {
        self.register_processor(format!("weaver.plugin.{}.{}", author, name).as_str(), Box::new(plugin::PluginProcessorFactory));
    }

    pub fn instantiate_processor(&self, name: &str, props: &serde_json::Value) -> Option<Box<dyn WorldInfoNode>> {
        let registry = self.processor_factories.read().unwrap();
        registry.get(name).map(|factory| factory.create(props, &self.plugin_bridge) as Box<dyn WorldInfoNode>)
    }

    pub fn plugin_bridge(&self) -> &P {
        &self.plugin_bridge
    }

    /// Used for initial variable registration
    /// 
    /// This method bypasses scope checks
    pub fn register_variable(&mut self, var: String, value: serde_json::Value) {
        self.variables.insert(var, value);
    }

    pub(crate) fn scoped_registry<'a>(&'a mut self, allowed_scopes: &'a [String], scope_id: String) -> ScopedRegistry<'a, P> {
        ScopedRegistry { inner: self, allowed_scopes, scope_id }
    }
    
}

// Define the factory trait
pub trait WorldInfoProcessorFactory<P: PluginBridge + 'static>: Send + Sync + 'static {
    fn create(&self, properties: &serde_json::Value, bridge: &Arc<P>) -> Box<dyn WorldInfoProcessor>;
}

pub trait PluginBridge: Clone + Debug {
    type PluginId: Copy + DeserializeOwned + Debug;
    fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) -> Result<String, crate::WorldInfoError>;
}

// Declare submodules
pub mod wildcard;
pub mod rng;
pub mod plugin;

// Re-export processors and factories
pub use wildcard::{WildcardProcessor, WildcardProcessorFactory};
pub use rng::{RngProcessor, RngProcessorFactory};