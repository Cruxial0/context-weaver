// src/parser.rs
use crate::core::processors::{PluginBridge, ProcessorRegistry};
use crate::errors::ParserError;
// Assuming you have TextNode and the WorldInfoNode/Processor traits defined elsewhere
use crate::{TextNode, WorldInfoNode};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use serde_json::{json, Map, Value};
use std::fmt::Debug;

// Derive the parser using the grammar file
#[derive(Parser)]
#[grammar = "parser.pest"]
struct WorldInfoParser;

// --- Abstract Syntax Tree (AST) ---
// Represents the parsed structure before resolution

#[derive(Debug, Clone)]
enum AstNode {
    Text(String),
    Processor {
        name: String,
        // Properties are kept as AST nodes until resolution
        properties: Vec<(String, AstNode)>
    },
    Trigger {
        id: String,
        raw_tag: String,
    },
    // Represents nested values within properties during parsing
    NestedValue(Value), // Stores partially parsed JSON-like values (String, Number, Bool, Null)
    NestedArray(Vec<AstNode>), // Represents parsed arrays
    NestedObject(Vec<(String, AstNode)>), // Represents parsed objects (pseudo-JSON)
}

// --- Public Entry Point ---

/// Parses the raw input string containing text and special tags (@[...] and <trigger...>)
/// into a vector of resolved WorldInfoNode objects.
///
/// Handles nested processor tags recursively, including those found within string literals.
/// Uses a pseudo-JSON format for properties (unquoted keys).
///
/// # Arguments
/// * `raw` - The input string to parse.
/// * `registry` - The processor registry used to instantiate processors.
///
/// # Returns
/// A `Result` containing either the vector of resolved nodes or a `ParserError`.
pub fn parse_entry_content<P: PluginBridge + Debug>(
    raw: &str,
    registry: &ProcessorRegistry<P>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    // 1. Parse the raw string into an AST using pest
    println!("Parsing input: {:?}", raw);
    let pairs = WorldInfoParser::parse(Rule::input, raw).map_err(ParserError::PestParse)?;
    println!("Initial pairs: {:?}", pairs);
    let ast = build_ast(pairs)?;
    println!("Built AST: {:?}", ast);

    // 2. Resolve the AST into WorldInfoNodes
    let resolved = resolve_ast_nodes(&ast, registry)?;
    println!("Resolved Nodes: {:?}", resolved.iter().map(|n| n.name()).collect::<Vec<_>>());
    Ok(resolved)
}

// --- AST Building ---

/// Builds the initial Abstract Syntax Tree (AST) from pest parse pairs.
fn build_ast(mut pairs: Pairs<Rule>) -> Result<Vec<AstNode>, ParserError> {
    let mut nodes = Vec::new();
    if let Some(input_pair) = pairs.next() {
        if input_pair.as_rule() == Rule::input {
            for pair in input_pair.into_inner() {
                match pair.as_rule() {
                    Rule::text => nodes.push(AstNode::Text(pair.as_str().to_string())),
                    Rule::processor_tag => nodes.push(parse_processor_node(pair)?),
                    Rule::trigger_tag => nodes.push(parse_trigger_node(pair)?),
                    Rule::EOI | Rule::WHITESPACE | Rule::COMMENT => (), // Ignore
                    rule => {
                        eprintln!("Unexpected rule inside input: {:?} ({:?})", rule, pair.as_str());
                        nodes.push(AstNode::Text(pair.as_str().to_string())); // Fallback
                    }
                }
            }
            if pairs.next().is_some() { eprintln!("Warning: Unexpected extra pairs after processing Rule::input"); }
        } else {
            return Err(ParserError::Processing(format!("Expected Rule::input, found {:?}", input_pair.as_rule())));
        }
    }
    Ok(nodes)
}


/// Parses a pest pair representing a processor tag into an AstNode::Processor.
fn parse_processor_node(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    let raw_tag = pair.as_str().to_string();
    let mut inner = pair.into_inner(); // processor_start, processor_name, properties?

    let _start_pair = inner.next().filter(|p| p.as_rule() == Rule::processor_start)
        .ok_or_else(|| ParserError::Processing(format!("Processor tag {:?} missing start '@['", raw_tag)))?;

    let name_pair = inner.next().filter(|p| p.as_rule() == Rule::processor_name)
        .ok_or_else(|| ParserError::Processing(format!("Expected processor_name after '@[' in tag {:?}", raw_tag)))?;
    let name = name_pair.as_str().to_string();

    let properties = match inner.peek() {
        Some(props_pair) if props_pair.as_rule() == Rule::properties => {
            parse_properties(inner.next().unwrap())?
        }
        _ => Vec::new(),
    };

    Ok(AstNode::Processor { name, properties })
}


/// Parses a pest pair representing a trigger tag into an AstNode::Trigger.
fn parse_trigger_node(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    let raw_tag = pair.as_str().to_string();
    let mut id = None;
    let mut inner = pair.into_inner(); // trigger_start, trigger_attributes?

    let _start_pair = inner.next().filter(|p| p.as_rule() == Rule::trigger_start)
        .ok_or_else(|| ParserError::Processing(format!("Trigger tag {:?} missing start '<trigger'", raw_tag)))?;

    if let Some(attributes_pair) = inner.peek() {
        if attributes_pair.as_rule() == Rule::trigger_attributes {
            let consumed_attributes_pair = inner.next().unwrap();
            for attr_pair in consumed_attributes_pair.into_inner() {
                if attr_pair.as_rule() == Rule::trigger_attribute {
                    let mut attr_inner = attr_pair.clone().into_inner(); // key, value
                    let key = attr_inner.next().ok_or_else(|| ParserError::Processing(format!("Missing key in trigger attribute: {:?}", attr_pair.as_str())))?.as_str();
                    let value_pair = attr_inner.next().ok_or_else(|| ParserError::Processing(format!("Missing value in trigger attribute: {:?}", attr_pair.as_str())))?;
                    let value = value_pair.as_str().trim_matches(|c| c == '"' || c == '\'');
                    if key == "id" {
                        id = Some(value.to_string());
                        break;
                    }
                }
            }
        }
    }

    let trigger_id = id.ok_or_else(|| ParserError::MissingTriggerId(raw_tag.clone()))?;
    if inner.next().is_some() { eprintln!("Warning: Unexpected extra pairs inside trigger tag: {:?}", raw_tag); }
    Ok(AstNode::Trigger { id: trigger_id, raw_tag })
}


/// Parses a pest pair representing pseudo-JSON properties into a vector of key-value AstNode pairs.
fn parse_properties(pair: Pair<Rule>) -> Result<Vec<(String, AstNode)>, ParserError> {
    let mut props = Vec::new();
    // properties = { property ~ ("," ~ property)* }
    for prop_pair in pair.into_inner() {
        if prop_pair.as_rule() == Rule::property {
            // property = { property_key ~ ":" ~ property_value }
            let mut inner = prop_pair.clone().into_inner(); // property_key, actual_value_rule

            let key_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing key pair in property rule: {:?}", prop_pair.as_str())))?;
            if key_pair.as_rule() != Rule::property_key { return Err(ParserError::Processing(format!("Expected property_key, found {:?} in property rule: {:?}", key_pair.as_rule(), prop_pair.as_str()))); }
            let key = key_pair.as_str().to_string();

            // The pair yielded by inner.next() corresponds to the rule matched *by* the silent `property_value` rule
            let actual_value_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing value pair after key '{}' in property rule: {:?}", key, prop_pair.as_str())))?;

            // Parse the actual value rule using parse_property_value
            let value_node = parse_property_value(actual_value_pair)?;
            props.push((key, value_node));

            if inner.next().is_some() { eprintln!("Warning: Unexpected extra pairs inside property rule: {:?}", prop_pair.as_str()); }
        } else if !matches!(prop_pair.as_rule(), Rule::WHITESPACE | Rule::COMMENT) {
             eprintln!("Unexpected rule within properties container: {:?}", prop_pair.as_rule());
             return Err(ParserError::InvalidRule(prop_pair.as_rule()));
        }
    }
    Ok(props)
}


/// Parses a pest pair representing a property value into an AstNode.
fn parse_property_value(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    match pair.as_rule() {
        Rule::processor_tag => parse_processor_node(pair),
        Rule::string => {
            let raw_str = pair.as_str();
            let inner_str = &raw_str[1..raw_str.len() - 1];
            // Basic unescaping
            let mut unescaped = String::with_capacity(inner_str.len());
            let mut chars = inner_str.chars();
            while let Some(c) = chars.next() {
                if c == '\\' {
                    match chars.next() {
                        Some('\\') => unescaped.push('\\'), Some('"') => unescaped.push('"'),
                        Some('n') => unescaped.push('\n'), Some('r') => unescaped.push('\r'),
                        Some('t') => unescaped.push('\t'),
                        Some(other) => { unescaped.push('\\'); unescaped.push(other); }
                        None => { unescaped.push('\\'); break; }
                    }
                } else { unescaped.push(c); }
            }
            Ok(AstNode::NestedValue(Value::String(unescaped)))
        }
        Rule::number => {
            let num_val: Value = serde_json::from_str(pair.as_str()).map_err(ParserError::Json)?;
            if num_val.is_number() { Ok(AstNode::NestedValue(num_val)) }
            else { Err(ParserError::Processing(format!("Parsed value is not a number: {}", pair.as_str()))) }
        }
        Rule::boolean => Ok(AstNode::NestedValue(json!(pair.as_str() == "true"))),
        Rule::null => Ok(AstNode::NestedValue(Value::Null)),

        // Handle generic object rule (contains optional 'properties')
        Rule::object => {
            // object = { "{" ~ properties? ~ "}" }
            match pair.into_inner().next() { // Check if properties rule exists inside {}
                Some(props_pair) if props_pair.as_rule() == Rule::properties => {
                    let props = parse_properties(props_pair)?;
                    Ok(AstNode::NestedObject(props))
                }
                Some(other) => Err(ParserError::Processing(format!("Unexpected rule {:?} inside object", other.as_rule()))),
                None => Ok(AstNode::NestedObject(Vec::new())), // Empty object {}
            }
        }
         // Handle generic array rule (contains elements matched by property_value)
         // ***** CORRECTED LOGIC HERE *****
        Rule::array => {
            // array = { "[" ~ (property_value ~ ("," ~ property_value)*)? ~ "]" }
            // As observed, pair.into_inner() on the array pair yields the *actual* value pairs
            // (e.g., string, number) directly, not the silent property_value pairs.
            let items = pair.into_inner() // Gets actual value pairs (string, number, etc.)
                 .filter(|p| !matches!(p.as_rule(), Rule::WHITESPACE | Rule::COMMENT)) // Skip noise
                 .map(|item_pair| { // item_pair is the actual value pair (e.g., string)
                     // Directly parse this item pair using parse_property_value
                     // No need to call item_pair.into_inner().next()
                     parse_property_value(item_pair)
                 })
                 .collect::<Result<Vec<_>, _>>()?;
            Ok(AstNode::NestedArray(items))
        }

        // This might occur if grammar allows text directly as a value
        Rule::text => Ok(AstNode::Text(pair.as_str().to_string())),

        // Any other rule appearing here is unexpected for a value
        r => {
            eprintln!("Unexpected rule type encountered during value parsing: {:?}", r);
            Err(ParserError::InvalidRule(r))
        }
    }
}


// --- AST Resolution --- (No changes needed from previous version)

/// Resolves a list of AST nodes into final WorldInfoNode objects.
fn resolve_ast_nodes<P: PluginBridge + Debug>(
    nodes: &[AstNode],
    registry: &ProcessorRegistry<P>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    let mut resolved_nodes = Vec::new();
    for node in nodes {
        println!("Resolving node: {:?}", node);
        match resolve_single_node(node, registry) {
            Ok(resolved_node_list) => resolved_nodes.extend(resolved_node_list),
            Err(e) => {
                 eprintln!("Resolution failed for node {:?}: {}", node, e);
                 return Err(e);
            }
        }
    }
    Ok(resolved_nodes)
}

/// Resolves a single AST node.
fn resolve_single_node<P: PluginBridge + Debug>(
    node: &AstNode,
    registry: &ProcessorRegistry<P>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    match node {
        AstNode::Text(content) => {
            println!("Resolving Text node, content: {:?}", content);
            if content.contains("@[") || content.contains("<trigger") {
                 println!("Text node contains potential tags, re-parsing...");
                 parse_entry_content(content, registry) // Re-parse content
            } else {
                 Ok(vec![ Box::new(TextNode { content: content.clone() }) as Box<dyn WorldInfoNode> ])
            }
        },
        AstNode::Trigger { id, .. } => {
            Ok(vec![ Box::new(TextNode { content: format!("<trigger id=\"{}\">", id) }) as Box<dyn WorldInfoNode> ])
        }
        AstNode::Processor { name, properties, .. } => {
            println!("Resolving Processor: {} with properties AST: {:?}", name, properties);
            let resolved_props = resolve_properties(properties, registry)?; // Calls updated resolve_property_value
             println!("Resolved properties for {}: {:?}", name, resolved_props);
            match registry.instantiate_processor(name, &resolved_props) {
                Some(processor) => {
                    println!("Successfully instantiated processor: {}", name);
                    Ok(vec![processor as Box<dyn WorldInfoNode>])
                }
                None => {
                    eprintln!("Failed to instantiate processor: {}", name);
                    Err(ParserError::ProcessorInstantiation( name.clone(),
                        format!("Processor not found or instantiation failed (props: {:?})", resolved_props),
                    ))
                },
            }
        }
        AstNode::NestedValue(_) | AstNode::NestedArray(_) | AstNode::NestedObject(_) => {
             Err(ParserError::Processing(format!("Unexpected nested AST node type during final resolution: {:?}", node)))
        }
    }
}

/// Recursively resolves AST nodes within properties into a serde_json::Value.
fn resolve_properties<P: PluginBridge + Debug>(
    properties: &[(String, AstNode)],
    registry: &ProcessorRegistry<P>,
) -> Result<Value, ParserError> {
    let mut map = Map::new();
    for (key, value_node) in properties {
         println!("Resolving property key: {}, value AST: {:?}", key, value_node);
        let resolved_value = resolve_property_value(value_node, registry)?;
         println!("Resolved property key: {}, resolved value: {:?}", key, resolved_value);
        map.insert(key.clone(), resolved_value);
    }
    Ok(Value::Object(map))
}

/// Resolves a single property value AST node into a serde_json::Value.
/// Handles recursive evaluation of tags within string literals.
fn resolve_property_value<P: PluginBridge + Debug>(
    node: &AstNode,
    registry: &ProcessorRegistry<P>,
) -> Result<Value, ParserError> {
    match node {
        AstNode::Processor { .. } => {
            let resolved_nodes = resolve_single_node(node, registry)?;
            let mut combined_content = String::new();
            for res_node in resolved_nodes {
                combined_content.push_str(&res_node.content().map_err(|e| ParserError::ProcessorExecution(format!("{}", e)))?);
            }
             println!("Resolved nested processor to string: {:?}", combined_content);
            Ok(Value::String(combined_content))
        }
        AstNode::NestedValue(Value::String(s)) => {
            println!("Resolving string literal: {:?}", s);
            if s.contains("@[") || s.contains("<trigger") {
                println!("String literal contains tags, re-parsing and evaluating: {:?}", s);
                let inner_resolved_nodes = parse_entry_content(s, registry)?;
                let mut combined_content = String::new();
                 for res_node in inner_resolved_nodes {
                    combined_content.push_str(&res_node.content().map_err(|e| ParserError::ProcessorExecution(format!("{}", e)))?);
                }
                println!("Re-parsed string evaluated to: {:?}", combined_content);
                Ok(Value::String(combined_content))
            } else { Ok(Value::String(s.clone())) }
        }
        AstNode::NestedValue(v @ Value::Number(_)) => Ok(v.clone()),
        AstNode::NestedValue(v @ Value::Bool(_)) => Ok(v.clone()),
        AstNode::NestedValue(v @ Value::Null) => Ok(v.clone()),
        AstNode::NestedArray(items) => {
            let resolved_items = items.iter().map(|item| resolve_property_value(item, registry)).collect::<Result<Vec<_>, _>>()?;
            Ok(Value::Array(resolved_items))
        }
        AstNode::NestedObject(props) => {
            resolve_properties(props, registry) // Resolve nested object's properties
        }
        AstNode::Text(t) => Err(ParserError::Processing(format!("Unexpected Text node as property value: '{}'", t))),
        AstNode::Trigger { raw_tag, .. } => Err(ParserError::Processing(format!("Unexpected Trigger node as property value: '{}'", raw_tag))),
        AstNode::NestedValue(other) => Err(ParserError::Processing(format!("Unexpected NestedValue type: {:?}", other))),
    }
}
