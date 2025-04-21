use std::sync::Arc;

use rand::Rng;

use crate::{types::Number, WorldInfoNode, WorldInfoProcessor};

use super::{PluginBridge, WorldInfoProcessorFactory};

#[derive(Debug, Clone)]
pub struct RngProcessor {
    min: Number,
    max: Number,
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
            Ok(rng.random_range(self.min.as_f64()..self.max.as_f64()).to_string())
        } else {
            let min = self.min.as_f64().round() as i64;
            let max = self.max.as_f64().round() as i64;
            Ok(rng.random_range(min..max).to_string())
        }
    }
}

pub struct RngProcessorFactory;

impl<P: PluginBridge + 'static> WorldInfoProcessorFactory<P> for RngProcessorFactory {
    fn create(&self, properties: &serde_json::Value, _bridge: &Arc<P>) -> Box<dyn WorldInfoProcessor> {
        let raw_min: crate::WorldInfoType = properties["min"].clone().into();
        let raw_max: crate::WorldInfoType = properties["max"].clone().into();
        let raw_decimal: crate::WorldInfoType = properties["decimal"].clone().into();

        let min = match raw_min {
            crate::WorldInfoType::Number(n) => n,
            crate::WorldInfoType::String(s) => parse_number(&s).unwrap_or(Number::Int(0)),
            _ => Number::Int(0),
        };

        let max = match raw_max {
            crate::WorldInfoType::Number(n) => n,
            crate::WorldInfoType::String(s) => parse_number(&s).unwrap_or(Number::Int(100)),
            _ => Number::Int(100),
        };

        let decimals = match raw_decimal {
            crate::WorldInfoType::Boolean(b) => b,
            _ => false,
        };

        Box::new(RngProcessor { min, max, decimals })
    }
}

fn parse_number(s: &str) -> Option<Number> {
    if let Ok(i) = s.parse::<i64>() {
        Some(Number::Int(i))
    } else if let Ok(u) = s.parse::<u64>() {
        Some(Number::UInt(u))
    } else if let Ok(f) = s.parse::<f64>() {
        Some(Number::Float(f))
    } else {
        None
    }
}