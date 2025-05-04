use std::{collections::{HashMap, HashSet}, fmt::Debug};

use log::{debug, error, trace};
use parser::{evaluate_nodes, parse_activation_condition, parse_entry_content, AstNode};

pub mod core;
pub mod errors;
pub mod id;
mod parser;
mod tests;
mod registry;

pub use errors::{ParserError, WorldInfoError};
use registry::{PluginBridge, ScopedRegistry, WorldInfoRegistry};

pub enum ContextNode {
    TextChunk(String),
    InsertionPoint(String),
}

pub struct Context(Vec<ContextNode>);

impl Context {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, node: ContextNode) {
        self.0.push(node);
    }

    pub fn text(&self) -> String {
        let mut result = String::new();
        for node in &self.0 {
            if let ContextNode::TextChunk(text) = node {
                result.push_str(text);
            }
        }
        result
    }
}

pub struct WorldInfoSettings {
    /// List of allowed processors
    pub permitted_processors: Vec<String>,
    pub recursion_limit: u32,
    pub id_length: usize,
    pub seperate_context_nodes: bool
}

impl Default for WorldInfoSettings {
    fn default() -> Self {
        Self { 
            permitted_processors: vec!["*".to_string()], 
            recursion_limit: 10, 
            id_length: 6, 
            seperate_context_nodes: true
        }
    }
}

pub struct WorldInfo<P: PluginBridge + 'static> {
    name: String,
    entries: Vec<WorldInfoEntry>,
    processor_registry: Box<WorldInfoRegistry<P>>,
    error_stack: Vec<WorldInfoError>,
    id_generator: id::IdGenerator,
    settings: WorldInfoSettings,

    // Stateful data
    trigger_stack: HashSet<String>,
    evaluated_entries: HashMap<String, String>
}

impl<P: PluginBridge> WorldInfo<P>{
    pub fn evaluate(&mut self, context: Context) -> Result<String, &Vec<WorldInfoError>> {
        self.reset();

        // pre-parse entries
        self.parse_entries();

        if self.error_stack.len() > 0 {
            return Err(&self.error_stack);
        }

        let mut failed = self.error_stack.len() > 0;
        let activated_entries = self.filter_activated_entries(&context.text());

        for entry in self.entries.iter_mut().filter(|e| activated_entries.contains(&e.id())) {
            debug!("-- Evaluating entry {} --", entry.id());
            let scopes = &entry.scopes.clone();
            let id = entry.id();
            let mut scoped_registry = self.processor_registry.scoped_registry(scopes, id.clone());
        
            
            match entry.evaluate(&mut scoped_registry) {
                Ok(content) => {
                    if !self.evaluated_entries.contains_key(&id) {
                        self.evaluated_entries.insert(id, content);
                    }
                },
                Err(e) => {
                    failed = true;
                    self.error_stack.push(e);
                },
            }
        }
        
        if failed {
            Err(&self.error_stack)
        } else {
            self.trigger_stack = self.processor_registry.drain_activation_stack();
            self.evaluate_activation_stack(&mut 0);
            Ok(self.format_output(context))
        }
    }

    fn format_output(&self, context: Context) -> String {
        let mut result = String::new();
        for chunk in context.0 {
            match chunk {
                ContextNode::TextChunk(text) => result.push_str(&text),
                ContextNode::InsertionPoint(id) => {
                    for entry in self.entries_from_insertion(id).iter().map(|e| self.evaluated_entries.get(&e.id()).unwrap()) {
                        if self.settings.seperate_context_nodes {
                            result.push_str("\n");
                        }
                        result.push_str(&entry);
                    }
                }
            }
        }
        result
    }

    fn entries_from_insertion(&self, insertion_id: String) -> Vec<&WorldInfoEntry> {
        let mut result = self.entries
            .iter()
            .filter(|e| self.evaluated_entries.contains_key(&e.id()))
            .filter(|e| e.insertion_point == Some(insertion_id.clone()))
            .into_iter()
            .collect::<Vec<_>>();

        result.sort_by(|a, b| a.order.cmp(&b.order));

        result
    }

    fn evaluate_activation_stack(&mut self, depth: &mut u32) {
        debug!("Evaluating activation stack (depth: {})", depth);
        let mut failed = false;
        let mut activated = self.filter_activated_entries(&self.get_evaluated_context());
        activated.retain(|id| !self.trigger_stack.contains(id) && !self.evaluated_entries.contains_key(id));
        
        let mut recurse_stack: HashSet<String> = self.trigger_stack.clone();
        recurse_stack.extend(activated);

        if recurse_stack.len() == 0 {
            return;
        }

        trace!("Recursively evaluating {} entries", recurse_stack.len());
        
        for entry in self.entries.iter_mut().filter(|e| recurse_stack.contains(&e.id()))
        {
            let scopes = &entry.scopes.clone();
            let id = entry.id();
            let mut scoped_registry = self.processor_registry.scoped_registry(scopes, id.clone());
            
            if failed {
                break;
            }
            
            match entry.evaluate(&mut scoped_registry) {
                Ok(content) => {
                    trace!("Evaluated entry {} (depth: {}) -> {}", id, depth, content);
                    if !self.evaluated_entries.contains_key(&id) {
                        self.evaluated_entries.insert(id, content);
                    }
                },
                Err(e) => {
                    failed = true;
                    error!("Failed to evaluate entry {} (depth: {}): {}", id, depth, e);
                    self.error_stack.push(e);
                },
            }
        }

        self.trigger_stack = self.processor_registry.drain_activation_stack();
        
        if failed {
            return;
        }
        *depth += 1;
        
        if *depth < self.settings.recursion_limit && self.trigger_stack.len() > 0 {
            self.evaluate_activation_stack(depth);
        }
    }

    fn parse_entries(&mut self) {
        debug!("-- Parsing entries --");
        for entry in &mut self.entries {
            trace!("-- Parsing entry {} --", entry.id());
            match entry.parse::<P>() {
                Ok(_) => (),
                Err(e) => {
                    self.error_stack.push(e); 
                    break;
                },
            }
        }
    }

    fn filter_activated_entries(&mut self, context: &String) -> Vec<String> {
        let mut activated_entries: Vec<String> = Vec::new();
        for entry in &mut self.entries {
            let scopes = &entry.scopes.clone();
            let mut scoped_registry = self.processor_registry.scoped_registry(scopes, entry.id());

            match entry.determine_activation_status(&mut scoped_registry, context) {
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
    
    fn get_evaluated_context(&self) -> String {
        let mut result = String::new();
        for entry in &self.entries {
            if self.evaluated_entries.contains_key(&entry.id()) {
                result.push_str(&(self.evaluated_entries[&entry.id()].clone() + "\n"));
            }
        }
        result
    }

    fn reset(&mut self) {
        self.error_stack.clear();
        self.trigger_stack.clear();
        self.evaluated_entries.clear();
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
            settings: Default::default(),
            processor_registry: registry,
            error_stack: Vec::new(),
            id_generator: id::IdGenerator::new(6),
            trigger_stack: HashSet::new(),
            evaluated_entries: HashMap::new()
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
        self.settings.permitted_processors = processors;
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
    insertion_point: Option<String>,

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
            insertion_point: None,
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
    fn evaluate<P: PluginBridge>(&mut self, registry: &mut ScopedRegistry<P>) -> Result<String, WorldInfoError>;
    fn determine_activation_status<P: PluginBridge>(&mut self, registry: &mut ScopedRegistry<P>, context: &String) -> Result<bool, WorldInfoError>;
    fn set_enabled(&mut self, enabled: bool) -> &mut WorldInfoEntry;
    fn set_constant(&mut self, constant: bool) -> &mut WorldInfoEntry;
    fn set_insertion_point(&mut self, insertion_point: String) -> &mut WorldInfoEntry;
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
    
    fn evaluate<P: PluginBridge>(&mut self, registry: &mut ScopedRegistry<P>) -> Result<String, WorldInfoError> {
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
    
    fn determine_activation_status<P: PluginBridge>(&mut self, registry: &mut ScopedRegistry<P>, context: &String) -> Result<bool, WorldInfoError> {
        trace!("Activation conditions: {:?}, constant: {}", self.activation_conditions, self.constant);
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
    
    fn set_insertion_point(&mut self, insertion_point: String) -> &mut WorldInfoEntry {
        self.insertion_point = Some(insertion_point);
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