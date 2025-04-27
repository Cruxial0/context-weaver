use std::{collections::HashMap, sync::{Arc, RwLock}};
use serde::de::DeserializeOwned;
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
    fn get_variable(&self, name: &str) -> Result<serde_json::Value, ParserError>;
    fn insert_variable(&mut self, name: &str, value: serde_json::Value) -> Result<(), ParserError>;
    fn update_variable(&mut self, name: &str, value: serde_json::Value) -> Result<(), ParserError>;

    /// Resolves implicit scopes
    /// 
    /// ## Example
    /// 
    /// `entry:foo` -> `<scope_id>:foo`
    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError>;
}

impl<'a, P: PluginBridge> VariableResolver for ScopedRegistry<'a, P> {
    fn get_variable(&self, full_name: &str) -> Result<serde_json::Value, ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        if !self.allowed_scopes.contains(&scope) {
            // 1) scope not allowed ⇒ permission error
            return Err(ParserError::InsufficientPermissions(
                format!("No access to scope `{}`", scope)
            ));
        }
        let key = format!("{}:{}", scope, name);
        self.inner.variables
            .get(&key)
            .cloned()
            .ok_or_else(|| ParserError::UndefinedVariable(name))
    }
    
    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError> {
        let (scope, name) = name.split_once(':').ok_or(ParserError::UndefinedVariable(name.to_string()))?;

        match scope {
            "entry" | "local" => Ok((self.scope_id.clone(), name.to_string())),
            _ => Ok((scope.to_string(), name.to_string()))
        }
    }
    
    fn insert_variable(&mut self, full_name: &str, value: serde_json::Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        let key = format!("{}:{}", scope, name);

        match scope.as_str() {
            "global" => {
                // bypass the resolver‐checks and insert raw key
                if self.inner.variables.contains_key(&key) {
                    Err(ParserError::VariableAlreadyExists(name))
                } else {
                    self.inner.register_variable(key, value);
                    Ok(())
                }
            }
            _ => {
                if !self.allowed_scopes.contains(&scope) {
                    Err(ParserError::InsufficientPermissions(
                        format!("No access to scope `{}`", scope)
                    ))
                } else if self.inner.variables.contains_key(&key) {
                    Err(ParserError::VariableAlreadyExists(name))
                } else {
                    self.inner.variables.insert(key, value);
                    Ok(())
                }
            }
        }
    }
    
    fn update_variable(&mut self, full_name: &str, value: serde_json::Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(full_name)?;
        if !self.allowed_scopes.contains(&scope) && scope != "global" {
            return Err(ParserError::InsufficientPermissions(
                format!("No access to scope `{}`", scope)
            ));
        }

        let key = format!("{}:{}", scope, name);
        if let Some(slot) = self.inner.variables.get_mut(&key) {
            *slot = value;
            Ok(())
        } else {
            Err(ParserError::UndefinedVariable(name))
        }
    }
}

impl<P: PluginBridge + 'static> VariableResolver for WorldInfoRegistry<P>{
    fn get_variable(&self, name: &str) -> Result<serde_json::Value, ParserError> {
        let (scope, name) = self.resolve_scope(name)?;
        let key = format!("{}:{}", scope, name);
        if let Some(val) = self.variables.get(&key){
            return Ok(val.clone());
        }
        Err(ParserError::UndefinedVariable(name.to_string()))
    }

    fn insert_variable(&mut self, name: &str, value: serde_json::Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(name)?;
        let key = format!("{}:{}", scope, name);

        if let Some(_) = self.variables.get_mut(&key) {
            Err(ParserError::VariableAlreadyExists(name.to_string()))
        } else {
            self.variables.insert(key, value);
            Ok(())
        }
    }

    fn update_variable(&mut self, name: &str, value: serde_json::Value) -> Result<(), ParserError> {
        let (scope, name) = self.resolve_scope(name)?;
        let key = format!("{}:{}", scope, name);
        if let Some(val) = self.variables.get_mut(&key) {
            *val = value;
            Ok(())
        } else {
            Err(ParserError::UndefinedVariable(name.to_string()))
        }
    }

    fn resolve_scope(&self, name: &str) -> Result<(String, String), ParserError> {
        let (scope, name) = name.split_once(':').ok_or(ParserError::UndefinedVariable(name.to_string()))?;

        match scope {
            "global" => Ok(("global".to_string(), name.to_string())),
            _ => Err(ParserError::InsufficientPermissions(format!("WorldInfo cannot access scoped variable: {}", name)))
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