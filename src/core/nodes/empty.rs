use crate::WorldInfoNode;

#[derive(Debug, Clone)]
pub struct EmptyNode;

impl WorldInfoNode for EmptyNode {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        Ok(String::new())
    }

    fn name(&self) -> String {
        "empty".to_string()
    }

    fn cloned(&self) -> Box<dyn WorldInfoNode + '_> {
        Box::new(self.to_owned())
    }
}