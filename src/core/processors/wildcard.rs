use std::sync::Arc;

use rand::Rng;

use crate::{WorldInfoNode, WorldInfoProcessor};

use super::{PluginBridge, WorldInfoProcessorFactory};

#[derive(Debug, Clone)]
pub struct WildcardProcessor {
    options: Vec<String>,
}

impl WorldInfoNode for WildcardProcessor {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        self.process()
    }
    
    fn name(&self) -> String {
        "weaver.core.wildcard".to_string()
    }

    fn cloned(&self) -> Box<dyn WorldInfoNode> {
        Box::new(self.to_owned())
    }
}

impl WorldInfoProcessor for WildcardProcessor {
    fn process(&self) -> Result<String, crate::WorldInfoError> {
        let length = self.options.len();

        // empty options does not need to fail
        if length == 0 {
            return Ok(String::from("").into());
        }

        let mut rng = rand::rng();
        let index = rng.random_range(0..length);

        Ok(self.options[index].clone().into())
    }
}

pub struct WildcardProcessorFactory;

impl<P: PluginBridge + 'static> WorldInfoProcessorFactory<P> for WildcardProcessorFactory {
    fn create(&self, properties: &serde_json::Value, _bridge: &Arc<P>) -> Box<dyn WorldInfoProcessor> {
        println!("Creating wildcard");
        let raw_items= properties["items"].as_array().unwrap();
        let mut items = Vec::new();

        for item in raw_items {
            if let serde_json::Value::String(s) = item {
                items.push(s.to_string());
            }
        }

        Box::new(WildcardProcessor { options: items })
    }
}