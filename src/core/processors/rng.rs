use std::sync::Arc;

use rand::Rng;

use crate::{registry::{PluginBridge, WorldInfoProcessorFactory}, WorldInfoNode, WorldInfoProcessor};

#[derive(Debug, Clone)]
pub struct RngProcessor {
    min: serde_json::Number,
    max: serde_json::Number,
    decimals: bool
}

impl WorldInfoNode for RngProcessor {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        self.process()
    }

    fn name(&self) -> String {
        "weaver.core.rng".to_string()
    }

    fn cloned(&self) -> Box<dyn WorldInfoNode> {
        Box::new(self.to_owned())
    }
}

impl WorldInfoProcessor for RngProcessor {
    fn process(&self) -> Result<String, crate::WorldInfoError> {
        let mut rng = rand::rng();

        if self.decimals {
            Ok(rng.random_range(self.min.as_f64().unwrap()..self.max.as_f64().unwrap()).to_string())
        } else {
            let min = self.min.as_i64().unwrap();
            let max = self.max.as_i64().unwrap();
            Ok(rng.random_range(min..max).to_string())
        }
    }
}

pub struct RngProcessorFactory;

impl<P: PluginBridge + 'static> WorldInfoProcessorFactory<P> for RngProcessorFactory {
    fn create(&self, properties: &serde_json::Value, _bridge: &Arc<P>) -> Box<dyn WorldInfoNode> {
        log::trace!("Creating rng processor");
        let raw_min = properties["min"].clone();
        let raw_max = properties["max"].clone();
        let raw_decimal = properties["decimals"].clone();

        let min = match raw_min {
            serde_json::Value::Number(number) => number,
            serde_json::Value::String(s) => parse_number(&s).unwrap(),
            _ => 0.into()
        };

        let max = match raw_max {
            serde_json::Value::Number(number) => number,
            serde_json::Value::String(s) => parse_number(&s).unwrap(),
            _ => 100.into()
        };

        let decimals = match raw_decimal {
            serde_json::Value::Bool(b) => b,
            _ => false
        };

        Box::new(RngProcessor { min, max, decimals })
    }
}

fn parse_number(s: &str) -> Option<serde_json::Number> {
    if let Ok(i) = s.parse::<i64>() {
        Some(serde_json::Number::from(i))
    } else if let Ok(u) = s.parse::<u64>() {
        Some(serde_json::Number::from(u))
    } else if let Ok(f) = s.parse::<f64>() {
        Some(serde_json::Number::from_f64(f).unwrap())
    } else {
        None
    }
}