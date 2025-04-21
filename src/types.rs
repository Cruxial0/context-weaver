use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Clone, Debug)]
pub enum WorldInfoType {
    String(String),
    Boolean(bool),
    Number(Number),
    List(Vec<WorldInfoType>),
    Map(HashMap<String, WorldInfoType>),
    Null
}

#[derive(Clone, Debug)]
pub enum Number {
    Int(i64),
    UInt(u64),
    Float(f64),
}

impl Number {
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Number::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Number::UInt(u) => Some(*u),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            Number::Int(i) => *i as f64,
            Number::UInt(u) => *u as f64,
            Number::Float(f) => *f,
        }
    }
}

// Macro to generate TryFrom and TryInto impls
macro_rules! impl_world_info_type_conversion {
    ($variant:ident, $type:ty, $error_msg:expr) => {
        impl TryInto<$type> for WorldInfoType {
            type Error = String;

            fn try_into(self) -> Result<$type, Self::Error> {
                if let WorldInfoType::$variant(value) = self {
                    Ok(value)
                } else {
                    Err($error_msg.to_string())
                }
            }
        }

        impl Into<WorldInfoType> for $type {
            fn into(self) -> WorldInfoType {
                WorldInfoType::$variant(self)
            }
        }
    };
}

macro_rules! impl_number_type_conversion {
    ($variant:ident, $type:ty) => {
        impl Into<Number> for $type {
            fn into(self) -> Number {
                Number::$variant(self)
            }
        }
    }
} 

macro_rules! impl_world_info_number_type_conversion {
    ($variant:ident, $type:ty) => {
        impl Into<WorldInfoType> for $type {
            fn into(self) -> WorldInfoType {
                WorldInfoType::Number(Number::$variant(self))
            }
        }
    }
} 

// Use marcos to implement the TryFrom and TryInto impls
impl_world_info_type_conversion!(String, String, "Not a string");
impl_world_info_type_conversion!(Boolean, bool, "Not a boolean");
impl_world_info_type_conversion!(List, Vec<WorldInfoType>, "Not a list");
impl_world_info_type_conversion!(Map, HashMap<String, WorldInfoType>, "Not a map");
impl_number_type_conversion!(Int, i64);
impl_number_type_conversion!(UInt, u64);
impl_number_type_conversion!(Float, f64);
impl_world_info_number_type_conversion!(Int, i64);
impl_world_info_number_type_conversion!(UInt, u64);
impl_world_info_number_type_conversion!(Float, f64);

impl Into<WorldInfoType> for serde_json::Value {
    fn into(self) -> WorldInfoType {
        match self {
            serde_json::Value::Null => WorldInfoType::Null,
            serde_json::Value::Bool(b) => WorldInfoType::Boolean(b),
            serde_json::Value::Number(number) => WorldInfoType::Number(number.into()),
            serde_json::Value::String(s) => WorldInfoType::String(s),
            serde_json::Value::Array(values) => WorldInfoType::List(values.into_iter().map(|x| x.into()).collect()),
            serde_json::Value::Object(map) => WorldInfoType::Map(map.into_iter().map(|(k, v)| (k, v.into())).collect()),
        }
    }
}

impl Into<WorldInfoType> for &serde_json::Value {
    fn into(self) -> WorldInfoType {
        match self {
            serde_json::Value::Null => WorldInfoType::Null,
            serde_json::Value::Bool(b) => WorldInfoType::Boolean(b.clone()),
            serde_json::Value::Number(number) => WorldInfoType::Number(number.clone().into()),
            serde_json::Value::String(s) => WorldInfoType::String(s.clone()),
            serde_json::Value::Array(values) => WorldInfoType::List(values.into_iter().map(|x| x.into()).collect()),
            serde_json::Value::Object(map) => WorldInfoType::Map(map.into_iter().map(|(k, v)| (k.clone(), v.clone().into())).collect()),
        }
    }
}

impl Into<Number> for serde_json::Number {
    fn into(self) -> Number {
        if self.is_f64() { 
            Number::Float(self.as_f64().unwrap())
        } else if self.is_u64() {
            Number::UInt(self.as_u64().unwrap())
        } else {
            Number::Int(self.as_i64().unwrap())
        }
    }
}