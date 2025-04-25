use core::processors::{PluginBridge, WorldInfoRegistry};
use std::fmt::Debug;

use parser::parse_entry_content;

pub mod core;
pub mod types;
pub mod errors;
mod parser;
mod tests;

pub use types::WorldInfoType;
pub use errors::{ParserError, WorldInfoError};



pub struct WorldInfo<P: PluginBridge + 'static> {
    name: String,
    entries: Vec<WorldInfoEntry>,
    /// List of allowed processors
    permitted_processors: Vec<String>,
    processor_registry: Box<WorldInfoRegistry<P>>,
    error_stack: Vec<WorldInfoError>,
}

impl<P: PluginBridge> WorldInfo<P>{
    pub fn evaluate(&mut self) -> Result<String, &Vec<WorldInfoError>> {
        let mut result = String::new();
        let mut failed = false;
        for entry in &mut self.entries {
            match entry.parse(&self.processor_registry) {
                Ok(_) => (),
                Err(e) => {
                    self.error_stack.push(e);
                    failed = true;
                }
            }

            if failed {
                break;
            }
            
            match entry.evalute() {
                Ok(content) => result.push_str(&content),
                Err(e) => {
                    failed = true;
                    self.error_stack.push(e)
                },
            }
        }
        
        if failed {
            Err(&self.error_stack)
        } else {
            Ok(result)
        }
    }
}

pub trait WorldInfoFactory<P: PluginBridge> {
    /// Creates a new empty world info
    fn new(registry: Box<WorldInfoRegistry<P>>) -> Self;
    /// Sets the name of the world info
    fn set_name(&mut self, name: &str) -> &mut Self;
    /// Inserts a world info entry
    fn insert_entry(&mut self, entry: WorldInfoEntry) -> &mut Self;
    /// Sets the list of permitted processors
    fn set_permitted_processors(&mut self, processors: Vec<String>) -> &mut Self;
    /// Builds the world info
    fn build(&self) -> &Self;

    /// Inserts a list of world info entries
    fn insert_entries(&mut self, entries: Vec<WorldInfoEntry>) -> &mut Self {
        for entry in entries {
            self.insert_entry(entry);
        }
        self
    }
}

impl<P: PluginBridge> WorldInfoFactory<P> for WorldInfo<P> {
    fn new(registry: Box<WorldInfoRegistry<P>>) -> Self {
        WorldInfo {
            name: String::new(),
            entries: Vec::new(),
            permitted_processors: Vec::new(),
            processor_registry: registry,
            error_stack: Vec::new()
        }
    }
    fn set_name(&mut self, name: &str) -> &mut Self {
        self.name = name.to_string();
        self
    }
    fn insert_entry(&mut self, entry: WorldInfoEntry) -> &mut Self {
        self.entries.push(entry);
        self
    }
    fn set_permitted_processors(&mut self, processors: Vec<String>) -> &mut Self {
        self.permitted_processors = processors;
        self
    }
    fn build(&self) -> &WorldInfo::<P> {
        self
    }
}

pub struct WorldInfoEntry {
    name: String,
    id: u32,
    order: u32,
    nodes: Vec<Box<dyn WorldInfoNode>>,
    text: String
}


impl WorldInfoEntry {
    pub fn new(name: String, id: u32, order: u32) -> Self {
        Self { name, id, order, nodes: Vec::new(), text: String::new() }
    }

    pub fn evalute(&self) -> Result<String, crate::WorldInfoError> {
        let mut result = String::new();
        for node in &self.nodes {
            match node.content() {
                Ok(content) => result.push_str(&content),
                Err(err) => return Err(err)
            }
        }
        Ok(result.into())
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn order(&self) -> u32 {
        self.order
    }
}

pub trait EntryFactory {
    fn create(name: &str, id: u32, order: u32) -> WorldInfoEntry;
    fn set_text(&mut self, text: &str) -> &mut WorldInfoEntry;
    fn parse<P: PluginBridge>(&mut self, registry: &WorldInfoRegistry<P>) -> Result<&mut WorldInfoEntry, WorldInfoError>;
}

impl EntryFactory for WorldInfoEntry {
    fn create(name: &str, id: u32, order: u32) -> WorldInfoEntry {
        WorldInfoEntry::new(name.to_string(), id, order)
    }
    fn parse<P: PluginBridge>(&mut self, registry: &WorldInfoRegistry<P>) -> Result<&mut WorldInfoEntry, WorldInfoError> {
        println!("Parsing node: {:?}", self.text);
        match parse_entry_content(&self.text, registry) {
            Ok(nodes) => self.nodes = nodes,
            Err(e) => return Err(WorldInfoError::ParserError(e)),
        }
        Ok(self)
    }
    
    fn set_text(&mut self, text: &str) -> &mut WorldInfoEntry {
        self.text = text.to_string();
        self
    }
}

pub trait WorldInfoNode: Debug {
    fn content(&self) -> Result<String, crate::WorldInfoError>;
    fn name(&self) -> String;
    fn cloned(&self) -> Box<dyn WorldInfoNode + '_>;
}

pub trait WorldInfoProcessor: WorldInfoNode {
    fn process(&self) -> Result<String, crate::WorldInfoError>;
}