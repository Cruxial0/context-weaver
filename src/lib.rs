use core::processors::{PluginBridge, ScopedRegistry, WorldInfoRegistry};
use std::fmt::Debug;

use log::debug;
use parser::{evaluate_nodes, parse_activation_condition, parse_entry_content, AstNode};

pub mod core;
pub mod errors;
pub mod id;
mod parser;
mod tests;

pub use errors::{ParserError, WorldInfoError};

pub struct WorldInfo<P: PluginBridge + 'static> {
    name: String,
    entries: Vec<WorldInfoEntry>,
    /// List of allowed processors
    permitted_processors: Vec<String>,
    processor_registry: Box<WorldInfoRegistry<P>>,
    error_stack: Vec<WorldInfoError>,
    id_generator: id::IdGenerator,

    // Stateful data
    activation_stack: Vec<String>
}

impl<P: PluginBridge> WorldInfo<P>{
    pub fn evaluate(&mut self, context: String) -> Result<String, &Vec<WorldInfoError>> {
        let mut result = String::new();
        self.reset();

        // pre-parse entries
        self.parse_entries();

        let mut failed = self.error_stack.len() > 0;
        let activated_entries = self.filter_activated_entries(&context);

        for entry in self.entries.iter_mut().filter(|e| activated_entries.contains(&e.id())) {
            let scopes = &entry.scopes.clone();
            let id = entry.id();
            let scoped_registry = &self.processor_registry.scoped_registry(scopes, id);
            
            if failed {
                break;
            }
            
            match entry.evaluate(scoped_registry) {
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

    fn parse_entries(&mut self) {
        for entry in &mut self.entries {
            match entry.parse::<P>() {
                Ok(_) => (),
                Err(e) => self.error_stack.push(e),
            }
        }
    }

    fn filter_activated_entries(&mut self, context: &String) -> Vec<String> {
        let mut activated_entries: Vec<String> = Vec::new();
        for entry in &mut self.entries {
            let scopes = &entry.scopes.clone();
            let scoped_registry = &self.processor_registry.scoped_registry(scopes, entry.id());

            match entry.determine_activation_status(&scoped_registry, context) {
                Ok(active) => {
                    if active {
                        activated_entries.push(entry.id());
                    }
                },
                Err(e) => self.error_stack.push(e),
            }
        }
        activated_entries
    }
    
    fn reset(&mut self) {
        self.error_stack.clear();
        self.activation_stack.clear();
    }
}

pub trait WorldInfoFactory<P: PluginBridge> {
    /// Creates a new empty world info
    fn new(registry: Box<WorldInfoRegistry<P>>) -> Self;
    /// Sets the name of the world info
    fn set_name(&mut self, name: &str) -> &mut Self;
    /// Creates a new world info entry with a random id and inserts it
    fn new_entry(&mut self, name: &str, order: u32) -> &mut WorldInfoEntry;
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
            error_stack: Vec::new(),
            id_generator: id::IdGenerator::new(6),
            activation_stack: Vec::new()
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
    
    fn new_entry(&mut self, name: &str, order: u32) -> &mut WorldInfoEntry {
        let id = self.id_generator.generate_unique(&self.entries.iter().map(|e| e.id()).collect());
        let entry = WorldInfoEntry::create(name, id.clone(), order);
        self.insert_entry(entry);
        
        self.entries.last_mut().unwrap()
    }
}

pub struct WorldInfoEntry {
    name: String,
    id: String,
    order: u32,
    scopes: Vec<String>,
    activation_conditions: Vec<String>,
    text: String,
    enabled: bool,
    constant: bool,

    // Stateful data
    raw_nodes: Vec<AstNode>,
    nodes: Vec<Box<dyn WorldInfoNode>>,
    content_updated: bool
}

impl WorldInfoEntry {
    pub fn new(name: String, id: String, order: u32) -> Self {
        Self 
        { 
            name, 
            id: id.clone(), 
            order, 
            nodes: Vec::new(), 
            text: String::new(),
            enabled: true,
            constant: false,
            scopes: vec!["global".to_string(), id],
            activation_conditions: Vec::new(),
            content_updated: true,
            raw_nodes: Vec::new()
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn id(&self) -> String {
        self.id.clone()
    }

    pub fn order(&self) -> u32 {
        self.order
    }
}

pub trait EntryFactory {
    fn create(name: &str, id: String, order: u32) -> WorldInfoEntry;
    fn set_text(&mut self, text: &str) -> &mut WorldInfoEntry;
    fn set_conditions(&mut self, conditions: Vec<String>) -> &mut WorldInfoEntry;
    fn parse<P: PluginBridge>(&mut self) -> Result<&mut WorldInfoEntry, WorldInfoError>;
    fn evaluate<P: PluginBridge>(&mut self, registry: &ScopedRegistry<P>) -> Result<String, WorldInfoError>;
    fn determine_activation_status<P: PluginBridge>(&mut self, registry: &ScopedRegistry<P>, context: &String) -> Result<bool, WorldInfoError>;
    fn set_enabled(&mut self, enabled: bool) -> &mut WorldInfoEntry;
    fn set_constant(&mut self, constant: bool) -> &mut WorldInfoEntry;
}

impl EntryFactory for WorldInfoEntry {
    fn create(name: &str, id: String, order: u32) -> WorldInfoEntry {
        WorldInfoEntry::new(name.to_string(), id, order)
    }
    fn parse<P: PluginBridge>(&mut self) -> Result<&mut WorldInfoEntry, WorldInfoError> {
        log::debug!("Parsing node: {:?}", self.text);
        // Re-parse if needed
        if self.content_updated {
            match parse_entry_content::<P>(&self.text) {
                Ok(ast_nodes) => self.raw_nodes = ast_nodes,
                Err(e) => return Err(WorldInfoError::ParserError(e)),
            }
        }
        
        self.content_updated = false;

        Ok(self)
    }
    
    fn set_text(&mut self, text: &str) -> &mut WorldInfoEntry {
        self.text = text.to_string();
        self.content_updated = true;
        self
    }
    
    fn evaluate<P: PluginBridge>(&mut self, registry: &ScopedRegistry<P>) -> Result<String, WorldInfoError> {
        match evaluate_nodes(&self.raw_nodes, registry, &self.id, None) {
            Ok(eval_nodes) => self.nodes = eval_nodes,
            Err(e) => return Err(WorldInfoError::ParserError(e)),
        };

        let mut result = String::new();
        for node in &self.nodes {
            result.push_str(&node.content()?);
        }
        Ok(result)
    }
    
    fn determine_activation_status<P: PluginBridge>(&mut self, registry: &ScopedRegistry<P>, context: &String) -> Result<bool, WorldInfoError> {
        debug!("Activation conditions: {:?}, constant: {}", self.activation_conditions, self.constant);
        match (self.enabled, self.constant, self.activation_conditions.len() > 0) {
            (true, true, false) => return Ok(true), // Constant entries without additional conditions are active
            (true, false, false) => return Ok(false), // Enabled entries without additional conditions are never active
            (false, ..) => return Ok(false), // Disabled entries are never active
            _ => (),
        }

        let mut results: Vec<bool> = Vec::new();
        for condition in &self.activation_conditions {
            if parse_activation_condition(condition, &context, registry, &self.id)
                .map_err(|e| WorldInfoError::ParserError(e))? 
            {
                results.push(true);
            }
            else {
                results.push(false);
            }
        }

        Ok(results.iter().all(|b| *b) && self.enabled)
    }
    
    fn set_conditions(&mut self, conditions: Vec<String>) -> &mut WorldInfoEntry {
        self.activation_conditions = conditions;
        self
    }
    
    fn set_enabled(&mut self, enabled: bool) -> &mut WorldInfoEntry {
        self.enabled = enabled;
        self
    }
    
    fn set_constant(&mut self, constant: bool) -> &mut WorldInfoEntry {
        self.constant = constant;
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