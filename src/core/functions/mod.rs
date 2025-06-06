use log::trace;
use serde_json::Value;
use std::fmt;
use std::fmt::Debug;

use crate::registry::{PluginBridge, ScopedRegistry};
use crate::ParserError;

pub mod modify;
pub mod pretty_print;
pub mod set;

pub use modify::ModifyFunction;
pub use pretty_print::PrettyPrintFunction;
pub use set::VariableSetFunction;

#[derive(Debug, Clone, PartialEq)]
pub enum ParamType {
    String,
    Number,
    Boolean,
    Array,
    Object,
    Null,
    All,
    Any(Vec<ParamType>),
    Optional(Option<Box<ParamType>>),
}

impl ParamType {
    /// Returns the most concrete type that matches the value.
    /// If this is a ParamType::Any, it will return the first concrete type that matches.
    /// Otherwise, it will return self if it matches, or None if it doesn't match.
    pub fn concrete_type_for(&self, value: &Value) -> Option<ParamType> {
        // If this is already a concrete type and it matches, return it
        match self {
            ParamType::String if value.is_string() => Some(ParamType::String),
            ParamType::Number if value.is_number() => Some(ParamType::Number),
            ParamType::Boolean if value.is_boolean() => Some(ParamType::Boolean),
            ParamType::Array if value.is_array() => Some(ParamType::Array),
            ParamType::Object if value.is_object() => Some(ParamType::Object),
            ParamType::Null if value.is_null() => Some(ParamType::Null),
            ParamType::All => Some(ParamType::All),
            
            // If this is Any, find the first concrete type that matches
            ParamType::Any(types) => {
                // First check concrete types
                for typ in types {
                    match typ {
                        ParamType::String if value.is_string() => return Some(ParamType::String),
                        ParamType::Number if value.is_number() => return Some(ParamType::Number),
                        ParamType::Boolean if value.is_boolean() => return Some(ParamType::Boolean),
                        ParamType::Array if value.is_array() => return Some(ParamType::Array),
                        ParamType::Object if value.is_object() => return Some(ParamType::Object),
                        ParamType::Null if value.is_null() => return Some(ParamType::Null),
                        ParamType::All => return Some(ParamType::All),
                        
                        // Recursively check nested Any types
                        ParamType::Any(_) => {
                            if let Some(concrete) = typ.concrete_type_for(value) {
                                return Some(concrete);
                            }
                        }
                        
                        _ => {}
                    }
                }
                
                // No matching type found
                None
            },

            // If this is Optional, check the inner type
            ParamType::Optional(inner) => {
                match inner {
                    Some(i) => i.concrete_type_for(value),
                    None => Some(ParamType::All),
                }
            }
            
            // This is a concrete type but it doesn't match
            _ => None,
        }
    }
    
    // Keep your existing matches function
    pub fn matches(&self, value: &Value) -> bool {
        trace!("ParamType::matches({:?}, {:?})", self, value);
        match self {
            ParamType::String => value.is_string(),
            ParamType::Number => value.is_number(),
            ParamType::Boolean => value.is_boolean(),
            ParamType::Array => value.is_array(),
            ParamType::Object => value.is_object(),
            ParamType::Null => value.is_null(),
            ParamType::All => true,
            ParamType::Any(types) => types.iter().any(|t| t.matches(value)),
            ParamType::Optional(inner) => {
                match inner {
                    Some(i) => i.matches(value),
                    None => true,
                }
            }
        }
    }

    pub fn is_optional(&self) -> bool {
        match self {
            ParamType::Optional(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for ParamType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParamType::String => write!(f, "String"),
            ParamType::Number => write!(f, "Number"),
            ParamType::Boolean => write!(f, "Boolean"),
            ParamType::Array => write!(f, "Array"),
            ParamType::Object => write!(f, "Object"),
            ParamType::Null => write!(f, "Null"),
            ParamType::All => write!(f, "Any"),
            ParamType::Any(types) => write!(f, "Any({})", types.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(", ")),
            ParamType::Optional(inner) => {
                match inner {
                    Some(i) => write!(f, "Optional({})", i),
                    None => write!(f, "Optional(None)"),
                }
            }
        }
    }
}

pub struct ParamSignature {
    pub param_type: ParamType,
    pub name: String,
    pub description: String,
}

pub trait ModFunction<P : PluginBridge + Debug> {
    fn call(&self, args: Vec<Value>, registry: &ScopedRegistry<P>) -> Result<Value, ParserError>;
    
    fn signature(&self) -> (String, Vec<ParamSignature>);
    
    fn validate_args(&self, args: &[Value]) -> Result<(), String> {
        trace!("ModFunction::validate_args({:?})", args);
        let (_, params) = self.signature();
        
        let min_args = params.iter().filter(|p| !p.param_type.is_optional()).count();
        if args.len() < min_args || args.len() > params.len() {
            return Err(format!("Expected at least {} arguments", min_args));
        }
        
        for (i, (arg, param)) in args.iter().zip(params.iter()).enumerate() {
            if !param.param_type.matches(arg) {
                return Err(format!(
                    "Argument {} ('{}') expected to be of type {}, got {:?}",
                    i, param.name, param.param_type, arg
                ));
            }
        }
        
        Ok(())
    }
}