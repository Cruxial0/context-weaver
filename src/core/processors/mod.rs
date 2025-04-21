use std::{collections::HashMap, sync::{Arc, RwLock}};
use serde::de::DeserializeOwned;
use crate::{WorldInfoNode, WorldInfoProcessor};
use std::fmt::Debug;

pub struct ProcessorRegistry<P: PluginBridge + 'static> {
    registry: RwLock<HashMap<String, Box<dyn WorldInfoProcessorFactory<P>>>>,
    plugin_bridge: Arc<P>,
}

impl<P: PluginBridge + 'static> ProcessorRegistry<P> {
    pub fn new(plugin_bridge: Arc<P>) -> Self {
        Self {
            registry: RwLock::new(HashMap::new()),
            plugin_bridge,
        }
    }

    pub fn register_processor(&self, name: &str, factory: Box<dyn WorldInfoProcessorFactory<P>>) {
        self.registry.write().unwrap().insert(name.to_string(), factory);
    }

    pub fn register_plugin_processor(&self, author: &str, name: &str) {
        self.register_processor(format!("weaver.plugin.{}.{}", author, name).as_str(), Box::new(plugin::PluginProcessorFactory));
    }

    pub fn instantiate_processor(&self, name: &str, props: &serde_json::Value) -> Option<Box<dyn WorldInfoNode>> {
        let registry = self.registry.read().unwrap();
        registry.get(name).map(|factory| factory.create(props, &self.plugin_bridge) as Box<dyn WorldInfoNode>)
    }

    pub fn plugin_bridge(&self) -> &P {
        &self.plugin_bridge
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