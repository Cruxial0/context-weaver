use serde_json::Value;

use crate::{registry::{PluginBridge, VariableResolver}, ParserError};

use super::{ModFunction, ParamSignature, ParamType};

#[derive(Default)]
pub struct VariableSetFunction<P: PluginBridge> {
    _marker: std::marker::PhantomData<P>,
}

impl<P : PluginBridge> VariableSetFunction<P> {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData
        }
    }
}

impl<P: PluginBridge> ModFunction<P> for VariableSetFunction<P> {
    fn call(&self, args: Vec<serde_json::Value>, registry: &crate::registry::ScopedRegistry<P>) -> Result<serde_json::Value, crate::ParserError> {
        let variable = args[0].as_str().unwrap().to_string().replace("{{", "").replace("}}", "");
        let value = args[1].clone();
        match registry.update_variable(&variable, value.clone()) {
            Ok(_) => return Ok(Value::Null),
            Err(e) => {
                match e {
                    ParserError::UndefinedVariable(_) => (),
                    _ => return Err(e)
                }
            }
        }
        
        match registry.insert_variable(&variable, value) {
            Ok(_) => Ok(Value::Null),
            Err(e) => Err(e)
        }
    }

    fn signature(&self) -> (String, Vec<ParamSignature>) {
        let variable_param = ParamSignature { 
            param_type: ParamType::String, 
            name: "variable".to_string(), 
            description: "The variable to set".to_string(), 
        };

        let value_param = ParamSignature { 
            param_type: ParamType::All, 
            name: "value".to_string(), 
            description: "The value to set".to_string(), 
        };

        let persistent_param = ParamSignature { 
            param_type: ParamType::Optional(Some(Box::new(ParamType::Boolean))), 
            name: "persistent".to_string(), 
            description: "Whether the variable should be persistent (optional)".to_string(), 
        };

        ("set".to_string(), vec![variable_param, value_param, persistent_param])
    }
    
}