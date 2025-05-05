use std::collections::HashMap;

use serde_json::{Map, Number, Value};

use crate::{registry::{PluginBridge, ScopedRegistry, VariableResolver}, ParserError};

use super::{ModFunction, ParamSignature, ParamType};


// Helper function to check if a Number is an integer (has no fractional part)
fn is_integer(num: &Number) -> bool {
    if num.is_u64() || num.is_i64() {
        return true;
    }
    
    // Check if the f64 value is effectively an integer
    if let Some(f) = num.as_f64() {
        return f.fract() == 0.0 && f.abs() <= i64::MAX as f64;
    }
    
    false
}

// Helper function to create a Number from an operation result
fn create_number_from_result(a: Number, b: Number, result: f64) -> Option<Number> {
    // If both inputs are integers and the result has no fractional part, return as integer
    if is_integer(&a) && is_integer(&b) && result.fract() == 0.0 {
        // Check range to determine if we need u64, i64, or have to fall back to f64
        if result >= 0.0 && result <= u64::MAX as f64 {
            return Some(Number::from(result as u64));
        } else if result >= i64::MIN as f64 && result <= i64::MAX as f64 {
            return Some(Number::from(result as i64));
        }
    }
    
    // Otherwise, fall back to floating point
    Number::from_f64(result)
}

pub struct ModifyFunction<P: PluginBridge> {
    _marker: std::marker::PhantomData<P>
}

impl<P: PluginBridge> ModifyFunction<P> {
    pub fn new() -> ModifyFunction<P> {
        ModifyFunction {
            _marker: std::marker::PhantomData
        }
    }

    fn try_get_numbers(&self, variable: Value, value: Value) -> Result<(Number, Number), ParserError> {
        if let (Value::Number(a), Value::Number(b)) = (variable.clone(), value.clone()) {
            Ok((a, b))
        } else {
            Err(ParserError::TypeMismatch(format!("Expected number, got {} and {}", variable, value)))
        }
    }

    fn try_get_strings(&self, variable: Value, value: Value) -> Result<(String, String), ParserError> {
        if let (Value::String(a), Value::String(b)) = (variable.clone(), value.clone()) {
            Ok((a, b))
        } else {
            Err(ParserError::TypeMismatch(format!("Expected string, got {} and {}", variable, value)))
        }
    }

    fn try_get_arrays(&self, variable: Value, value: Value) -> Result<(Vec<Value>, Vec<Value>), ParserError> {
        if let (Value::Array(a), Value::Array(b)) = (variable.clone(), value.clone()) {
            Ok((a, b))
        } else {
            Err(ParserError::TypeMismatch(format!("Expected array, got {} and {}", variable, value)))
        }
    }

    fn try_get_array_value(&self, variable: Value, value: Value) -> Result<(Vec<Value>, Value), ParserError> {
        if let (Value::Array(a), b) = (variable.clone(), value.clone()) {
            Ok((a, b))
        } else {
            Err(ParserError::TypeMismatch(format!("Expected array, got {} and {}", variable, value)))
        }
    }

    fn try_get_objects(&self, variable: Value, value: Value) -> Result<(Map<String, Value>, Map<String, Value>), ParserError> {
        if let (Value::Object(a), Value::Object(b)) = (variable.clone(), value.clone()) {
            Ok((a, b))
        } else {
            Err(ParserError::TypeMismatch(format!("Expected object, got {} and {}", variable, value)))
        }
    }

    fn add_number(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_numbers(variable, value)?;
        
        // Convert both to f64 for the calculation
        let a_f64 = a.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        let b_f64 = b.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        
        let sum = a_f64 + b_f64;
        
        // Convert back to the appropriate Number type
        create_number_from_result(a, b, sum)
            .map(Value::Number)
            .ok_or(ParserError::TypeError("Invalid number".to_string()))
    }
    
    fn sub_number(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_numbers(variable, value)?;
        
        // Convert both to f64 for the calculation
        let a_f64 = a.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        let b_f64 = b.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        
        let diff = a_f64 - b_f64;
        
        // Convert back to the appropriate Number type
        create_number_from_result(a, b, diff)
            .map(Value::Number)
            .ok_or(ParserError::TypeError("Invalid number".to_string()))
    }
    
    fn mul_number(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_numbers(variable, value)?;
        
        // Convert both to f64 for the calculation
        let a_f64 = a.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        let b_f64 = b.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        
        let product = a_f64 * b_f64;
        
        // Convert back to the appropriate Number type
        create_number_from_result(a, b, product)
            .map(Value::Number)
            .ok_or(ParserError::TypeError("Invalid number".to_string()))
    }
    
    fn div_number(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_numbers(variable, value)?;
        
        // Convert both to f64 for the calculation
        let a_f64 = a.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        let b_f64 = b.as_f64().ok_or(ParserError::TypeError("Invalid number".to_string()))?;
        
        if b_f64 == 0.0 {
            return Err(ParserError::Evaluation("Division by zero".to_string()));
        }
        
        let quotient = a_f64 / b_f64;
        
        // Convert back to the appropriate Number type
        create_number_from_result(a, b, quotient)
            .map(Value::Number)
            .ok_or(ParserError::TypeError("Invalid number".to_string()))
    }

    fn add_string(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_strings(variable, value.clone())?;
        Ok(Value::String(a + &b))
    }

    fn sub_string(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (a, b) = self.try_get_strings(variable, value.clone())?;
        Ok(Value::String(a.replace(&b, "")))
    }

    fn extend_array(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_arrays(variable, value.clone())?;
        a.extend(b);
        Ok(Value::Array(a))
    }

    fn insert_array(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_array_value(variable, value.clone())?;
        a.insert(a.len(), b);
        Ok(Value::Array(a))
    }

    fn remove_array(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_array_value(variable, value.clone())?;
        a.retain(|v| v != &b);
        Ok(Value::Array(a))
    }

    fn insert_object(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_objects(variable, value.clone())?;
        if b.len() != 1 {
            return Err(ParserError::TypeMismatch(format!("Expected object, got {}", value)));
        }
        a.extend(b);
        Ok(Value::Object(a))
    }

    fn remove_object(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_objects(variable, value.clone())?;
        a.retain(|k, _| !b.contains_key(k));
        Ok(Value::Object(a))
    }

    fn merge_object(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_objects(variable, value.clone())?;
        a.extend(b);
        Ok(Value::Object(a))
    }

    fn modify_object(&self, variable: Value, value: Value) -> Result<Value, ParserError> {
        let (mut a, b) = self.try_get_objects(variable, value.clone())?;
        if b.len() != 1 {
            return Err(ParserError::TypeMismatch(format!("Expected object, got {}", value)));
        }
        for (k, v) in b {
            let val = a.get_mut(&k).ok_or(ParserError::TypeMismatch(format!("Expected object, got {}", value)))?;
            *val = v;
        }
        Ok(Value::Object(a))
    }
}

impl<P: PluginBridge> ModFunction<P> for ModifyFunction<P> {
    fn call(&self, args: Vec<Value>, registry: &ScopedRegistry<P>) -> Result<Value, ParserError>{
        let value = &self.signature().1[2];

        let var = args[0].as_str().unwrap().to_string().replace("{{", "").replace("}}", "");
        let op = args[1].as_str().unwrap();
        let val = args[2].clone();

        let variable = registry.get_variable(&var, None)?;

        let concrete = value.param_type.concrete_type_for(&val);
        if concrete.is_none() {
            return Err(ParserError::TypeMismatch(format!("Expected {}, got {}", value.param_type, val)));
        }

        let new_value = match (op, concrete.unwrap()) {
            ("add", ParamType::Number) => self.add_number(variable, val),
            ("sub", ParamType::Number) => self.sub_number(variable, val),
            ("div", ParamType::Number) => self.div_number(variable, val),
            ("mul", ParamType::Number) => self.mul_number(variable, val),

            ("add", ParamType::String) => self.add_string(variable, val),
            ("sub", ParamType::String) => self.sub_string(variable, val),

            ("extend", ParamType::All) => self.extend_array(variable, val),
            ("insert", ParamType::All) => self.insert_array(variable, val),
            ("remove", ParamType::All) => self.remove_array(variable, val),

            ("insert", ParamType::Object) => self.insert_object(variable, val),
            ("remove", ParamType::Object) => self.remove_object(variable, val),
            ("merge", ParamType::Object) => self.merge_object(variable, val),
            ("modify", ParamType::Object) => self.modify_object(variable, val),

            _ => Err(ParserError::UnsupportedOperation(format!("Operation {} not supported for type {}", op, value.param_type)))
        }?;
        
        registry.update_variable(&var, new_value)?;

        Ok(Value::Null)
    }

    fn signature(&self) -> (String, Vec<ParamSignature>) {
        let variable_signature = ParamSignature {
            param_type: ParamType::String,
            name: "variable".to_string(),
            description: "The variable to modify".to_string(),
        };

        let operation_signature = ParamSignature {
            param_type: ParamType::String,
            name: "operation".to_string(),
            description: "The operation to perform on the variable".to_string(),
        };

        let value_signature = ParamSignature {
            param_type: ParamType::Any(vec![ParamType::String, ParamType::Number, ParamType::Boolean, ParamType::Array, ParamType::Object]),
            name: "value".to_string(),
            description: "The value to set the variable to".to_string(),
        };

        ("modify".to_string(), vec![variable_signature, operation_signature, value_signature])
    }
}