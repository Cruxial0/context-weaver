use serde_json::Value;

use crate::{registry::{PluginBridge, ScopedRegistry}, ParserError};

use super::{ModFunction, ParamSignature, ParamType};

pub struct PrettyPrintFunction<P: PluginBridge> {
    _marker: std::marker::PhantomData<P>
}

impl<P : PluginBridge> PrettyPrintFunction<P> {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData
        }
    }
}

impl<P : PluginBridge> ModFunction<P> for PrettyPrintFunction<P> {
    fn call(&self, args: Vec<Value>, _: &ScopedRegistry<P>) -> Result<Value, ParserError> {
        let value = &self.signature().1[0];
        let input = args[0].clone();
        let concrete = value.param_type.concrete_type_for(&input);
        if concrete.is_none() {
            return Err(ParserError::TypeMismatch(format!("Expected {}, got {}", value.param_type, input)));
        }

        let pretty_output = serde_json::to_string_pretty(&input).unwrap();

        Ok(Value::String(pretty_output))
    }

    fn signature(&self) -> (String, Vec<ParamSignature>) {
        let input_param = ParamSignature {
            param_type: ParamType::Any(vec![ParamType::Array, ParamType::Object]),
            name: "input".to_string(),
            description: "The input to pretty print".to_string(),
        };

        ("pretty_print".to_string(), vec![input_param])
    }
}