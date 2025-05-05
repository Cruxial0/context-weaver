use std::sync::Arc;

use crate::{registry::{PluginBridge, WorldInfoProcessorFactory}, WorldInfoNode, WorldInfoProcessor};

/// Processor for loading plugins
/// 
/// Plugin Processors contain a reference to the ID of a plugin, aswell as the name of a callback function expected to return a string
/// 
/// Plugin Processors will have an identifier following this format: `weaver.plugin.<plugin_author>.<plugin_name>`

#[derive(Clone, Debug)]
pub struct PluginProcessor<'a, P: PluginBridge + 'a> {
    plugin_id: P::PluginId,
    plugin_name: String,
    plugin_author: String,
    plugin_data: serde_json::Value,
    plugin_bridge: Arc<P>,
    _marker: std::marker::PhantomData<&'a ()>
}

impl<'a, P: PluginBridge + 'a> WorldInfoNode for PluginProcessor<'a, P> {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        self.process()
    }

    fn name(&self) -> String {
        format!("weaver.plugin.{}.{}", self.plugin_author, self.plugin_name)
    }

    fn cloned(&self) -> Box<dyn WorldInfoNode + 'a> {
        Box::new(Clone::clone(self))
    }
}

impl<'a, P: PluginBridge> WorldInfoProcessor for PluginProcessor<'a, P> {
    fn process(&self) -> Result<String, crate::WorldInfoError> {
        self.plugin_bridge.invoke_plugin(self.plugin_id, self.plugin_data.clone())
    }
}

pub struct PluginProcessorFactory;

impl<P: PluginBridge + 'static> WorldInfoProcessorFactory<P> for PluginProcessorFactory {
    fn create(&self, properties: &serde_json::Value, bridge: &Arc<P>) -> Box<dyn WorldInfoNode> {
        // parse out your plugin_id, name, author, props…
        let plugin_id     = serde_json::from_value::<P::PluginId>(properties["plugin_id"].clone()).unwrap();
        let plugin_name   = properties["plugin_name"].as_str().unwrap().to_string();
        let plugin_author = properties["plugin_author"].as_str().unwrap().to_string();
        let inner_props   = properties["plugin_data"].clone();

        // cheaply clone the Arc handle
        let bridge_handle = Arc::clone(bridge);

        Box::new(PluginProcessor {
            plugin_id,
            plugin_name,
            plugin_author,
            plugin_data: inner_props,
            plugin_bridge: bridge_handle,
            _marker: std::marker::PhantomData
        })
    }
}