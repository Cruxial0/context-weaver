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
        let raw_items: Vec<crate::WorldInfoType> = properties["items"].as_array().unwrap().iter().map(|x| x.into()).collect();
        let mut items = Vec::new();

        for item in raw_items {
            match item {
                crate::WorldInfoType::String(s) => items.push(s),
                _ => continue
            }
        }

        Box::new(WildcardProcessor { options: items })
    }
}