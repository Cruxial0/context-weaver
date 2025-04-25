use crate::WorldInfoNode;

#[derive(Debug, Clone)]
pub struct VariableNode {
    value: serde_json::Value
}

impl VariableNode {
    pub fn new(value: serde_json::Value) -> Self {
        Self { value }
    }
}

impl WorldInfoNode for VariableNode {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        match self.value {
            serde_json::Value::Null => Ok("".to_string()),
            _ => Ok(self.value.to_string())
        }
    }

    fn name(&self) -> String {
        "variable".to_string()
    }

    fn cloned(&self) -> Box<dyn WorldInfoNode + '_> {
        Box::new(self.to_owned())
    }
}