use crate::WorldInfoNode;

#[derive(Debug, Clone)]
pub struct TextNode {
    pub(crate) content: String,
}

impl WorldInfoNode for TextNode {
    fn content(&self) -> Result<String, crate::WorldInfoError> {
        Ok(self.content.clone())
    }
    
    fn name(&self) -> String {
        "text".to_string()
    }
    
    fn cloned(&self) -> Box<dyn WorldInfoNode> {
        Box::new(self.to_owned())
    }
}