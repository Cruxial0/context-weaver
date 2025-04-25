// src/parser.rs
use crate::core::processors::{PluginBridge, WorldInfoRegistry};
use crate::errors::ParserError;
use crate::{TextNode, WorldInfoNode}; // Placeholder imports
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use serde_json::{json, Map, Value};
use std::fmt::Debug;
use std::iter::Peekable; // Needed for peeking in the main loop

// Derive the parser using the grammar file
#[derive(Parser)]
#[grammar = "parser.pest"] // Use the v5 grammar
struct WorldInfoParser;

// --- Abstract Syntax Tree (AST) ---
// Represents the parsed structure before resolution

#[derive(Debug, Clone)]
pub enum AstNode {
    Text(String),
    Processor {
        name: String,
        properties: Vec<(String, AstNode)>,
        raw_tag: String, // Store the original tag for context/debugging
    },
    Trigger {
        id: String,
        raw_tag: String,
    },
    Variable {
        scope: String,
        name: String,
        raw_tag: String, // Store the original tag {{scope:name}}
    },
    MacroIf {
        condition: Box<AstNode>, // The expression to evaluate
        then_branch: Vec<AstNode>, // Nodes inside the 'if' block
        else_branch: Option<Vec<AstNode>>, // Nodes inside the optional 'else' block
        raw_tag: String, // Store the original starting tag {# if ... #}
    },
    MacroForeach {
        item_variable: String,   // The loop variable name (e.g., "item")
        collection: Box<AstNode>, // The expression for the collection (e.g., a variable)
        body: Vec<AstNode>,      // Nodes inside the loop body
        raw_tag: String, // Store the original starting tag {# foreach ... #}
    },
    // Represents nested values within properties during parsing
    NestedValue(Value), // Stores parsed JSON-like values (String, Number, Bool, Null)
    NestedArray(Vec<AstNode>), // Represents parsed arrays within properties
    NestedObject(Vec<(String, AstNode)>), // Represents parsed objects (pseudo-JSON) within properties
}

// --- Public Entry Point ---

/// Parses the raw input string containing text and special tags (@[...], <trigger...>, {{...}}, {#...#})
/// into a vector of resolved WorldInfoNode objects.
///
/// Handles nested tags recursively.
///
/// # Arguments
/// * `raw` - The input string to parse.
/// * `registry` - The processor registry used to instantiate processors.
///
/// # Returns
/// A `Result` containing either the vector of resolved nodes or a `ParserError`.
pub fn parse_entry_content<P: PluginBridge + Debug>(
    raw: &str,
    registry: &WorldInfoRegistry<P>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    // 1. Parse the raw string into an AST using pest
    println!("Parsing input: {:?}", raw);
    let pairs = WorldInfoParser::parse(Rule::input, raw)
        .map_err(|e| ParserError::PestParse(e))?;
    println!("Initial pairs: {:?}", pairs);

    let input_pair = pairs.peek().ok_or_else(|| ParserError::Processing("Empty parse result".to_string()))?;
    if input_pair.as_rule() != Rule::input {
         return Err(ParserError::Processing(format!("Expected Rule::input, found {:?}", input_pair.as_rule())));
    }

    // Build AST from the children of the 'input' rule
    // Clone the inner pairs to make the iterator mutable
    let mut inner_pairs = input_pair.clone().into_inner().peekable();
    let ast = build_ast_from_pairs::<P>(&mut inner_pairs)?; // Pass mutable iterator
    println!("Built AST: {:?}", ast);

    // 2. Resolve the AST into WorldInfoNodes
    let resolved = resolve_ast_nodes(&ast, registry)?;
    println!("Resolved Nodes (may be partial): {:?}", resolved.iter().map(|n| n.name()).collect::<Vec<_>>());
    Ok(resolved)
}

// --- AST Building ---

/// Builds the AST from a *peekable iterator* of pest pairs.
/// Consumes pairs from the iterator as it builds nodes.
fn build_ast_from_pairs<'i, P: PluginBridge + Debug>(
    pairs: &mut Peekable<Pairs<'i, Rule>>,
) -> Result<Vec<AstNode>, ParserError> {
    let mut nodes = Vec::new();

    while let Some(pair) = pairs.peek() {
        // Clone the pair to inspect its rule without consuming it yet
        let current_pair = pair.clone();
        match current_pair.as_rule() {
            // --- Direct Atomic Nodes ---
            Rule::text => {
                let text_pair = pairs.next().unwrap(); // Consume the pair
                nodes.push(AstNode::Text(text_pair.as_str().to_string()));
            }

            // --- Top-Level Content Tags (as defined in the `input` rule) ---
            Rule::processor_tag_content => {
                let content_pair = pairs.next().unwrap(); // Consume the pair
                nodes.push(parse_processor_content::<P>(content_pair)?);
            }
            Rule::trigger_tag_content => {
                let content_pair = pairs.next().unwrap(); // Consume the pair
                nodes.push(parse_trigger_content::<P>(content_pair)?);
            }
             Rule::variable_tag_content => {
                let content_pair = pairs.next().unwrap(); // Consume the pair
                nodes.push(parse_variable_content::<P>(content_pair)?);
            }

            // --- Macro Handling (Macros are non-atomic containers) ---
            Rule::macro_tag => {
                let macro_container_pair = pairs.next().unwrap(); // Consume the 'macro_tag' pair
                // Get the specific macro type (if or foreach) from the inner rule
                let macro_pair = macro_container_pair.clone().into_inner().next()
                     .ok_or_else(|| ParserError::Processing(format!("Empty 'macro_tag' pair: {:?}", macro_container_pair.as_str())))?;
                match macro_pair.as_rule() {
                    Rule::macro_if => nodes.push(parse_macro_if::<P>(macro_pair)?),
                    Rule::macro_foreach => nodes.push(parse_macro_foreach::<P>(macro_pair)?),
                    r => return Err(ParserError::InvalidRule(r)), // Should not happen if grammar is correct
                }
            }

            // --- End of Input ---
            Rule::EOI => {
                pairs.next(); // Consume EOI
                break; // Stop parsing
            }

            // --- Ignore Whitespace (Should be handled by `_` in grammar, but belt-and-suspenders) ---
            Rule::WHITESPACE => {
                pairs.next(); // Consume whitespace
            }

            // --- Unexpected Rule ---
            r => {
                 // Consume the unexpected pair to avoid infinite loop
                 let unexpected_pair = pairs.next().unwrap();
                 eprintln!("Unexpected rule during AST building: {:?} ({:?})", r, unexpected_pair.as_str());
                 // Return specific error for unexpected content rules if they appear here
                 // (This indicates a logic error elsewhere, as they should be handled above)
                 if matches!(r, Rule::processor_tag_content | Rule::trigger_tag_content | Rule::variable_tag_content) {
                    eprintln!("Error: Content rule {:?} encountered unexpectedly in main loop.", r);
                 }
                 return Err(ParserError::InvalidRule(r));
            }
        }
    }
    Ok(nodes)
}

/// Parses a processor_tag_content pair.
fn parse_processor_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    if pair.as_rule() != Rule::processor_tag_content {
        return Err(ParserError::Processing(format!("Expected processor_tag_content, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string(); // Capture the full tag
    let mut inner = pair.clone().into_inner(); // processor_start, processor_name, [properties], processor_end

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_start in content".to_string()))?;
    let name_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_name in content".to_string()))?;
    if name_pair.as_rule() != Rule::processor_name { return Err(ParserError::Processing(format!("Expected processor_name, got {:?}", name_pair.as_rule()))); }
    let name = name_pair.as_str().to_string();

    // Check for optional properties rule
    let properties = if let Some(props_pair) = inner.peek() {
        // Check if the next rule is 'properties'
        if props_pair.as_rule() == Rule::properties {
            let consumed_props_pair = inner.next().unwrap(); // Consume properties
            parse_properties(consumed_props_pair)?
        } else {
            // No properties rule found (might be just processor_end next)
            Vec::new()
        }
    } else {
        // No more inner pairs after name, means no properties
        Vec::new()
    };

    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_end in content".to_string()))?;

    Ok(AstNode::Processor { name, properties, raw_tag: raw_tag_string })
}


/// Parses a trigger_tag_content pair.
fn parse_trigger_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    if pair.as_rule() != Rule::trigger_tag_content {
        return Err(ParserError::Processing(format!("Expected trigger_tag_content, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string(); // Capture the full tag
    let mut inner = pair.clone().into_inner(); // trigger_start, [trigger_attributes], trigger_end

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected trigger_start in content".to_string()))?;

    let mut trigger_id: Option<String> = None;

    // Check for optional attributes
    if let Some(attrs_pair) = inner.peek() {
        if attrs_pair.as_rule() == Rule::trigger_attributes {
            let consumed_attrs = inner.next().unwrap(); // Consume attributes
            for attr_pair in consumed_attrs.clone().into_inner() {
                 if attr_pair.as_rule() == Rule::trigger_attribute {
                     let mut attr_inner = attr_pair.clone().into_inner(); // trigger_key, trigger_value
                     let key_pair = attr_inner.next().ok_or_else(|| ParserError::Processing("Trigger attribute missing key".to_string()))?;
                     let value_pair = attr_inner.next().ok_or_else(|| ParserError::Processing("Trigger attribute missing value".to_string()))?;

                     if key_pair.as_str() == "id" {
                        // trigger_value is silent `_`, so we get the inner quoted_string
                        let quoted_string_pair = value_pair.into_inner().next()
                            .ok_or_else(|| ParserError::Processing("Trigger id value missing quoted_string".to_string()))?;
                         if quoted_string_pair.as_rule() == Rule::quoted_string {
                            let content = quoted_string_pair.clone().into_inner()
                                .find(|p| p.as_rule() == Rule::string_content)
                                .map(|p| p.as_str()).unwrap_or("");
                            trigger_id = Some(unescape_string(content)?);
                         } else {
                             return Err(ParserError::Processing(format!("Invalid trigger id value type: expected quoted_string, got {:?}", quoted_string_pair.as_rule())));
                         }
                     }
                 }
            }
        }
    }

    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected trigger_end in content".to_string()))?;

    let id = trigger_id.ok_or_else(|| ParserError::MissingTriggerId(raw_tag_string.clone()))?;

    Ok(AstNode::Trigger { id, raw_tag: raw_tag_string })
}

/// Parses a variable_tag_content pair.
fn parse_variable_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
     if pair.as_rule() != Rule::variable_tag_content {
        return Err(ParserError::Processing(format!("Expected variable_tag_content, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string(); // Capture the full tag
    let mut inner = pair.clone().into_inner(); // variable_start, scope, variable_separator, name, variable_end

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected variable_start in content".to_string()))?;

    let scope_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected scope in variable content".to_string()))?;
    if scope_pair.as_rule() != Rule::scope { return Err(ParserError::Processing(format!("Expected scope, got {:?}", scope_pair.as_rule()))); }

    let _sep_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected separator in variable content".to_string()))?;

    let name_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected name in variable content".to_string()))?;
    if name_pair.as_rule() != Rule::name { return Err(ParserError::Processing(format!("Expected name, got {:?}", name_pair.as_rule()))); }

    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected variable_end in content".to_string()))?;

    let scope = scope_pair.as_str().to_string();
    let name = name_pair.as_str().to_string();

    Ok(AstNode::Variable { scope, name, raw_tag })
}


// --- Macro, Property, Literal Parsing Functions ---

/// Parses a pest pair representing a macro if tag into an AstNode::MacroIf.
fn parse_macro_if<P: PluginBridge + Debug>(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    // pair is macro_if rule
    let mut inner = pair.clone().into_inner(); // macro_if_start, expression, macro_tag_end, inner_nodes (then), optional macro_else, macro_endif

    let start_tag_pair = inner.next().ok_or_else(|| ParserError::Processing("If macro missing start tag".to_string()))?;
    let start_tag_str = start_tag_pair.as_str();

    let condition_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing condition: {}", start_tag_str)))?;
    let condition_str = condition_pair.as_str();
    let condition = parse_expression(condition_pair)?;

    let tag_end_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing end '#}}': {}", start_tag_str)))?;
    let tag_end_str = tag_end_pair.as_str();
    let full_raw_start_tag = format!("{}{}{}", start_tag_str, condition_str, tag_end_str);


    // Parse the 'then' branch nodes
    let then_nodes_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing 'then' branch content: {}", full_raw_start_tag)))?;
    if then_nodes_pair.as_rule() != Rule::inner_nodes { return Err(ParserError::Processing(format!("Expected inner_nodes for 'then' branch, found {:?} in {}", then_nodes_pair.as_rule(), full_raw_start_tag))); }
    let mut then_inner_pairs = then_nodes_pair.clone().into_inner().peekable();
    let then_branch = build_ast_from_pairs::<P>(&mut then_inner_pairs)?;

    // Check for optional 'else' branch
    let mut else_branch: Option<Vec<AstNode>> = None;
    if let Some(peek_pair) = inner.peek() {
        if peek_pair.as_rule() == Rule::macro_else {
            let else_pair = inner.next().unwrap(); // Consume macro_else
            let mut else_inner = else_pair.clone().into_inner(); // macro_else_tag, inner_nodes (else)
            let _else_tag = else_inner.next().ok_or_else(|| ParserError::Processing("Else macro missing tag".to_string()))?;
            let else_nodes_pair = else_inner.next().ok_or_else(|| ParserError::Processing("Else macro missing content".to_string()))?;
             if else_nodes_pair.as_rule() != Rule::inner_nodes { return Err(ParserError::Processing(format!("Expected inner_nodes for 'else' branch, found {:?} in {}", else_nodes_pair.as_rule(), full_raw_start_tag))); }
             let mut else_inner_pairs = else_nodes_pair.clone().into_inner().peekable();
            else_branch = Some(build_ast_from_pairs::<P>(&mut else_inner_pairs)?);
        }
    }

    // Ensure endif is present
    let endif_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing endif tag: {}", full_raw_start_tag)))?;
    if endif_pair.as_rule() != Rule::macro_endif { return Err(ParserError::Processing(format!("Expected endif tag, found {:?} in {}", endif_pair.as_rule(), full_raw_start_tag))); }

    Ok(AstNode::MacroIf {
        condition: Box::new(condition),
        then_branch,
        else_branch,
        raw_tag: full_raw_start_tag, // Store the {# if ... #} part
    })
}

/// Parses a pest pair representing a macro foreach tag into an AstNode::MacroForeach.
fn parse_macro_foreach<P: PluginBridge + Debug>(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    // pair is macro_foreach rule
    let mut inner = pair.clone().into_inner(); // macro_foreach_start, identifier (item_var), expression (collection), macro_tag_end, inner_nodes, macro_endforeach

    let start_tag_pair = inner.next().ok_or_else(|| ParserError::Processing("Foreach macro missing start tag".to_string()))?;
    let start_tag_str = start_tag_pair.as_str();

    let item_var_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing item variable: {}", start_tag_str)))?;
     if item_var_pair.as_rule() != Rule::identifier {
        return Err(ParserError::Processing(format!("Expected identifier for item variable in foreach, found {:?}: {}", item_var_pair.as_rule(), start_tag_str)));
    }
    let item_variable = item_var_pair.as_str().trim().to_string(); // Trim whitespace around identifier
    let item_var_str_full = item_var_pair.as_str(); // Includes surrounding whitespace from grammar

    // The grammar structure implies "in" and surrounding whitespace are consumed implicitly between identifier and expression.

    let collection_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing collection after 'in': {}", start_tag_str)))?;
    let collection_str_full = collection_pair.as_str(); // Includes surrounding whitespace
    let collection = parse_expression(collection_pair)?;

    let tag_end_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing end '#}}': {}", start_tag_str)))?;
    let tag_end_str = tag_end_pair.as_str();

    // Reconstruct the full raw start tag accurately using the full spans captured
    let full_raw_start_tag = format!("{}{}{}{}", start_tag_str, item_var_str_full, collection_str_full, tag_end_str);


    // Parse the body nodes
    let body_nodes_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing body content: {}", full_raw_start_tag)))?;
    if body_nodes_pair.as_rule() != Rule::inner_nodes { return Err(ParserError::Processing(format!("Expected inner_nodes for 'foreach' body, found {:?} in {}", body_nodes_pair.as_rule(), full_raw_start_tag))); }
    let mut body_inner_pairs = body_nodes_pair.clone().into_inner().peekable();
    let body = build_ast_from_pairs::<P>(&mut body_inner_pairs)?;

    // Ensure endforeach is present
    let endforeach_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing endforeach tag: {}", full_raw_start_tag)))?;
    if endforeach_pair.as_rule() != Rule::macro_endforeach { return Err(ParserError::Processing(format!("Expected endforeach tag, found {:?} in {}", endforeach_pair.as_rule(), full_raw_start_tag))); }

    Ok(AstNode::MacroForeach {
        item_variable, // Use trimmed variable name
        collection: Box::new(collection),
        body,
        raw_tag: full_raw_start_tag, // Store the {# foreach ... #} part
    })
}


/// Parses a pest pair representing an expression (currently variable or literal).
/// Expects the silent `expression` pair, or potentially the inner content directly.
fn parse_expression(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
     // Check if the pair itself is the content we expect inside an expression
     match pair.as_rule() {
         Rule::variable => return parse_atomic_variable(pair),
         Rule::literal => {
             // Literal is silent `_`, get the actual inner rule (string, number, etc.)
             let literal_pair = pair.clone().into_inner().next()
                 .ok_or_else(|| ParserError::Processing(format!("Empty literal pair: {:?}", pair.as_str())))?;
             return parse_literal(literal_pair);
         }
         // If it's not variable or literal directly, assume it's the `expression` rule itself
         Rule::expression => {
             // Expression is silent `_`, so get the actual inner rule (variable, literal)
             let inner_pair = pair.clone().into_inner().next()
                .ok_or_else(|| ParserError::Processing(format!("Empty expression pair: {:?}", pair.as_str())))?;
            // Recursively call parse_expression on the inner pair to handle variable or literal
            return parse_expression(inner_pair);
         }
         // Handle cases where pest might directly provide number/bool/null within expression context
         Rule::number | Rule::boolean | Rule::null => return parse_literal(pair),
         Rule::string => { // String is silent, contains quoted_string
             let quoted_string_pair = pair.clone().into_inner().next()
                 .ok_or_else(|| ParserError::Processing(format!("Empty string pair in expression: {:?}", pair.as_str())))?;
             return parse_literal(quoted_string_pair); // Pass quoted_string to parse_literal
         }

         // Any other rule here is unexpected within an expression context
         r => Err(ParserError::Processing(format!("Unexpected rule type in expression context: {:?} (from {:?})", r, pair.as_str()))),
     }
}

/// Parses an *atomic* variable pair (e.g., from within an expression or property value).
fn parse_atomic_variable(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    if pair.as_rule() != Rule::variable {
        return Err(ParserError::Processing(format!("Expected atomic variable rule, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    // Since it's atomic, we extract info based on structure defined in grammar
    // {{ scope : name }}
    // We need to manually find the parts within the raw string, as `into_inner` won't work.
    // This is less robust than using non-atomic rules but necessary here.
    let content = raw_tag.trim_start_matches("{{").trim_end_matches("}}");
    let parts: Vec<&str> = content.splitn(2, ':').collect();
    if parts.len() == 2 {
        let scope = parts[0].trim().to_string();
        let name = parts[1].trim().to_string();
        // Basic validation (ensure not empty)
        if scope.is_empty() || name.is_empty() {
             Err(ParserError::Processing(format!("Invalid atomic variable format (empty scope/name): {}", raw_tag)))
        } else {
             Ok(AstNode::Variable { scope, name, raw_tag })
        }
    } else {
        Err(ParserError::Processing(format!("Invalid atomic variable format (missing ':'): {}", raw_tag)))
    }
}

/// Parses an *atomic* processor tag pair (e.g., from within a property value).
fn parse_atomic_processor(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
     if pair.as_rule() != Rule::processor_tag {
        return Err(ParserError::Processing(format!("Expected atomic processor_tag rule, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    // Similar to atomic variable, parse based on expected string structure: @[name(...)] or @[name]
    // This is brittle. Consider making processor_tag non-atomic if complex parsing needed inside.
    let content = raw_tag.trim_start_matches("@[")
                         .trim_end_matches(']');

    let (name_str, props_str_opt) = match content.find('(') {
        Some(paren_idx) => {
            if content.ends_with(')') {
                (&content[..paren_idx], Some(&content[paren_idx+1..content.len()-1]))
            } else {
                // Malformed - opening paren but no closing
                return Err(ParserError::Processing(format!("Malformed atomic processor tag (missing ')'): {}", raw_tag)));
            }
        },
        None => (content, None), // No parentheses found
    };

    let name = name_str.trim().to_string();
    if name.is_empty() {
         return Err(ParserError::Processing(format!("Malformed atomic processor tag (empty name): {}", raw_tag)));
    }

    let properties = match props_str_opt {
         Some(props_str) if !props_str.trim().is_empty() => {
             // Re-parse the properties string using the properties rule
             // This requires a separate parse call, which is inefficient but works for atomic rules.
             let prop_pairs = WorldInfoParser::parse(Rule::properties, props_str)
                 .map_err(|e| ParserError::PestParse(e.with_path(&format!("atomic processor properties: {}", props_str))))?;
             // Assuming parse returns a single `properties` pair
             if let Some(props_pair) = prop_pairs.peek() {
                 parse_properties(props_pair)?
             } else {
                 Vec::new() // Should not happen if parse succeeded
             }
         }
         _ => Vec::new(), // No properties string or empty properties string
    };


    Ok(AstNode::Processor { name, properties, raw_tag })
}


/// Parses a pest pair representing a literal value into an AstNode::NestedValue.
/// Expects the actual literal rule pair (string, number, boolean, null, or quoted_string).
fn parse_literal(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    match pair.as_rule() {
        // Handle quoted_string which might be passed directly if string rule was silent
        Rule::quoted_string => {
            // quoted_string contains optional string_content
            let inner_content = pair.clone().into_inner()
                .find(|p| p.as_rule() == Rule::string_content)
                .map(|p| p.as_str())
                .unwrap_or(""); // Default to empty string if no content (e.g., "")
            let unescaped = unescape_string(inner_content)?;
            Ok(AstNode::NestedValue(Value::String(unescaped)))
        }
        // Handle the silent string rule if it's passed
        Rule::string => {
            let quoted_string_pair = pair.clone().into_inner().next()
                .ok_or_else(|| ParserError::Processing(format!("Empty string literal pair: {:?}", pair.as_str())))?;
            // Recurse with the inner quoted_string pair
            parse_literal(quoted_string_pair)
        }
        Rule::number => {
            // Attempt to parse the number string directly into a serde_json::Number
            let num_str = pair.as_str();
            let num = num_str.parse::<serde_json::Number>().map_err(|e| ParserError::Processing(format!("Failed to parse number '{}': {}", num_str, e)))?;
            Ok(AstNode::NestedValue(Value::Number(num)))
        }
        Rule::boolean => Ok(AstNode::NestedValue(json!(pair.as_str() == "true"))),
        Rule::null => Ok(AstNode::NestedValue(Value::Null)),
        r => Err(ParserError::Processing(format!("Unexpected rule type for literal: {:?} ({:?})", r, pair.as_str()))),
    }
}

/// Helper function to unescape string content based on the `escape` rule.
fn unescape_string(s: &str) -> Result<String, ParserError> {
    let mut unescaped = String::with_capacity(s.len());
    let mut chars = s.chars().peekable(); // Use peekable for unicode escape
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => unescaped.push('"'), Some('\\') => unescaped.push('\\'),
                Some('/') => unescaped.push('/'), Some('b') => unescaped.push('\u{0008}'),
                Some('f') => unescaped.push('\u{000C}'), Some('n') => unescaped.push('\n'),
                Some('r') => unescaped.push('\r'), Some('t') => unescaped.push('\t'),
                Some('u') => {
                    let mut hex_code = String::with_capacity(4);
                    for _ in 0..4 {
                        // Ensure the next char exists before pushing
                        hex_code.push(chars.next().ok_or_else(|| ParserError::Processing("Incomplete unicode escape sequence: missing hex digits".to_string()))?);
                    }
                    // Validate hex digits
                    if hex_code.len() != 4 || !hex_code.chars().all(|c| c.is_ascii_hexdigit()) {
                        return Err(ParserError::Processing(format!("Invalid unicode escape sequence: non-hex characters in \\u{}", hex_code)));
                    }
                    let code_point = u32::from_str_radix(&hex_code, 16).map_err(|_| ParserError::Processing(format!("Invalid unicode escape sequence: failed to parse hex \\u{}", hex_code)))?;
                    unescaped.push(std::char::from_u32(code_point).ok_or_else(|| ParserError::Processing(format!("Invalid unicode code point: {}", code_point)))?);
                }
                Some(other) => {
                    // If it's not a recognized escape, just treat it as literal backslash + char
                    unescaped.push('\\');
                    unescaped.push(other);
                }
                None => { return Err(ParserError::Processing("Dangling escape character at end of string".to_string())); }
            }
        } else {
            unescaped.push(c);
        }
    }
    Ok(unescaped)
}


/// Parses a pest pair representing pseudo-JSON properties into a vector of key-value AstNode pairs.
/// Expects a `properties` rule pair.
fn parse_properties(pair: Pair<Rule>) -> Result<Vec<(String, AstNode)>, ParserError> {
    if pair.as_rule() != Rule::properties {
         return Err(ParserError::Processing(format!("Expected properties rule, got {:?}", pair.as_rule())));
    }
    let mut props = Vec::new();
    // `properties` contains a sequence of `property` pairs
    for prop_pair in pair.clone().into_inner() {
        // Only process actual `property` rules
        if prop_pair.as_rule() == Rule::property {
            let mut inner = prop_pair.clone().into_inner(); // property_key, property_value

            // First inner element MUST be the property_key
            let key_pair_outer = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing property_key pair in property rule: {:?}", prop_pair.as_str())))?;

            // Check the rule is indeed property_key
            if key_pair_outer.as_rule() != Rule::property_key {
                 return Err(ParserError::Processing(format!("Expected property_key rule, got {:?} in {}", key_pair_outer.as_rule(), prop_pair.as_str())));
            }

            // Handle atomic property_key
            let key_str = key_pair_outer.as_str();
            let key = if key_str.starts_with('"') && key_str.ends_with('"') && key_str.len() >= 2 {
                // It matched quoted_string: remove quotes and unescape content
                let inner_content = &key_str[1..key_str.len()-1];
                unescape_string(inner_content)?
            } else {
                // It matched identifier: use the string directly
                key_str.to_string()
            };

            // The next inner element MUST be the rule matched by property_value
            let actual_value_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing value pair after key '{}' in property rule: {:?}", key, prop_pair.as_str())))?;

            // Parse the actual value rule using parse_property_value
            let value_node = parse_property_value(actual_value_pair)?;
            props.push((key, value_node));
        }
    }
    Ok(props)
}


/// Parses a pest pair representing a property value into an AstNode.
/// Expects the actual value pair matched *by* the silent `property_value` rule
/// (e.g., processor_tag, variable, object, array, or one of the literal types like number, boolean).
fn parse_property_value(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    match pair.as_rule() {
        // Handle complex types first
        Rule::processor_tag => parse_atomic_processor(pair),
        Rule::variable => parse_atomic_variable(pair),
        Rule::object => { // object is non-atomic { }
            match pair.clone().into_inner().find(|p| p.as_rule() == Rule::properties) {
                Some(props_pair) => {
                    let props = parse_properties(props_pair)?;
                    Ok(AstNode::NestedObject(props))
                }
                None => Ok(AstNode::NestedObject(Vec::new())), // Empty object {}
            }
        }
        Rule::array => { // array is non-atomic [ ]
            let items = pair.clone().into_inner()
                 .filter(|p| !matches!(p.as_rule(), Rule::WHITESPACE)) // Filter only actual value rules
                 .map(parse_property_value) // Recursively parse each item
                 .collect::<Result<Vec<_>, _>>()?;
            Ok(AstNode::NestedArray(items))
        }

        // Handle literal types directly (as they might be passed directly by Pest)
        Rule::number | Rule::boolean | Rule::null | Rule::string | Rule::quoted_string => {
            // Delegate to parse_literal, which handles these specific types
            parse_literal(pair)
        }
        // *** FIX: Add case for string_content ***
        Rule::string_content => {
            // Treat string_content directly as an unescaped string value
            // This handles the case where it appears directly inside an array
            let content = pair.as_str();
            // Assuming string_content from the grammar doesn't need further unescaping here,
            // as the escapes would have been handled by the (skipped) quoted_string rule.
            // If escapes *can* appear raw in string_content, unescape_string(content)? would be needed.
             Ok(AstNode::NestedValue(Value::String(content.to_string())))
        }

        // Keep the literal rule as a fallback, although less likely to be hit now
        Rule::literal => { // literal is silent '_'
            let literal_pair = pair.clone().into_inner().next()
                .ok_or_else(|| ParserError::Processing(format!("Empty literal pair: {:?}", pair.as_str())))?;
            // Delegate to parse_literal with the inner actual literal rule pair
            parse_literal(literal_pair)
        }

        // Catch unexpected rules
        r => {
            eprintln!("Unexpected rule type encountered during property value parsing: {:?} ({:?})", r, pair.as_str());
            Err(ParserError::InvalidRule(r))
        }
    }
}


// --- AST Resolution ---

/// Resolves a list of AST nodes into final WorldInfoNode objects.
fn resolve_ast_nodes<P: PluginBridge + Debug>(
    nodes: &[AstNode],
    registry: &WorldInfoRegistry<P>,
    // TODO: Add context here: e.g., variable_scopes: &VariableScopes
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    let mut resolved_nodes = Vec::new();
    for node in nodes {
        println!("Resolving node: {:?}", node);
        // Pass context down if needed
        match resolve_single_node(node, registry /*, variable_scopes */) {
            Ok(resolved_node_list) => resolved_nodes.extend(resolved_node_list),
            Err(e) => {
                 eprintln!("Resolution failed for node {:?}: {}", node, e);
                 return Err(e); // Stop resolution on first error
            }
        }
    }
    Ok(resolved_nodes)
}

/// Resolves a single AST node.
/// Returns a Vec because some nodes (like macros or text containing tags) might expand into multiple nodes.
fn resolve_single_node<P: PluginBridge + Debug>(
    node: &AstNode,
    registry: &WorldInfoRegistry<P>,
     // TODO: Add context here: e.g., variable_scopes: &VariableScopes
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    match node {
        AstNode::Text(content) => {
            println!("Resolving Text node, content: {:?}", content);
            // Simple text node, no further parsing needed at this stage
            Ok(vec![ Box::new(TextNode { content: content.clone() }) as Box<dyn WorldInfoNode> ])
        },
        AstNode::Trigger { id, raw_tag } => {
             println!("Resolving Trigger: {}", raw_tag);
            // Represent as text for now. Needs proper handling in evaluation.
            Ok(vec![ Box::new(TextNode { content: format!("<trigger id=\"{}\">", id) }) as Box<dyn WorldInfoNode> ])
        }
        AstNode::Processor { name, properties, .. } => {
            println!("Resolving Processor: {} with properties AST: {:?}", name, properties);
            // Resolve nested AST property values into a single JSON value for the processor
            let resolved_props = resolve_properties_to_json(properties, registry /*, variable_scopes */)?;
             println!("Resolved properties for {}: {:?}", name, resolved_props);
            // Instantiate the processor using the resolved JSON properties
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
        AstNode::Variable { raw_tag, .. } => {
             println!("Resolving Variable: {}", raw_tag);
             // Placeholder: Variable resolution needs access to the current variable scopes/context
             Err(ParserError::Evaluation(format!("Variable evaluation not implemented: {}", raw_tag)))
        }
        AstNode::MacroIf { raw_tag, .. } => {
             println!("Resolving MacroIf: {}", raw_tag);
             // Placeholder: Macro evaluation requires evaluating the condition and resolving branches.
             Err(ParserError::Evaluation(format!("If macro evaluation not implemented: {}", raw_tag)))
        }
        AstNode::MacroForeach { raw_tag, .. } => {
             println!("Resolving MacroForeach: {}", raw_tag);
             // Placeholder: Macro evaluation requires evaluating the collection and iterating.
             Err(ParserError::Evaluation(format!("Foreach macro evaluation not implemented: {}", raw_tag)))
        }
        // These should only exist *within* properties during AST building, not as top-level nodes for resolution.
        AstNode::NestedValue(_) | AstNode::NestedArray(_) | AstNode::NestedObject(_) => {
             Err(ParserError::Processing(format!("Unexpected nested AST node type during final resolution: {:?}", node)))
        }
    }
}

/// Recursively resolves AST nodes within properties into a single serde_json::Value object.
fn resolve_properties_to_json<P: PluginBridge + Debug>(
    properties: &[(String, AstNode)], // Input is Vec<(Key, ValueAstNode)>
    registry: &WorldInfoRegistry<P>,
    // TODO: Add context here: e.g., variable_scopes: &VariableScopes
) -> Result<Value, ParserError> {
    let mut map = Map::new();
    for (key, value_node) in properties {
         println!("Resolving property key: {}, value AST: {:?}", key, value_node);
         // Resolve the AST node for the value into a JSON value
        let resolved_value = resolve_property_value_to_json(value_node, registry /*, variable_scopes */)?;
         println!("Resolved property key: {}, resolved value: {:?}", key, resolved_value);
        map.insert(key.clone(), resolved_value);
    }
    Ok(Value::Object(map)) // Return a JSON object
}

/// Resolves a single property value AST node into a serde_json::Value.
fn resolve_property_value_to_json<P: PluginBridge + Debug>(
    node: &AstNode, // Input is a single AST node representing the value
    registry: &WorldInfoRegistry<P>,
     // TODO: Add context here: e.g., variable_scopes: &VariableScopes
) -> Result<Value, ParserError> {
    match node {
        // If a property value is another processor, variable, macro etc.,
        // resolve it first, then get its content (likely as a string).
        // This assumes nested tags evaluate to strings when used as property values.
        AstNode::Processor { .. } | AstNode::MacroIf { .. } | AstNode::MacroForeach { .. } | AstNode::Text { .. } | AstNode::Trigger { .. } => {
             println!("Resolving complex node within property: {:?}", node);
            let resolved_nodes = resolve_single_node(node, registry /*, variable_scopes */)?;
            let mut combined_content = String::new();
            for res_node in resolved_nodes {
                // Assume WorldInfoNode has a method like `content()` or `evaluate_to_string()`
                match res_node.content() { // Replace `.content()` with the actual method if different
                     Ok(content) => combined_content.push_str(&content),
                     Err(e) => return Err(ParserError::ProcessorExecution(format!("Failed to get content from resolved node within property: {}", e))),
                }
            }
             println!("Resolved nested node to string: {:?}", combined_content);
            Ok(Value::String(combined_content))
        }
        AstNode::Variable { raw_tag, .. } => {
             println!("Resolving Variable within property: {}", raw_tag);
             // Placeholder: Evaluate the variable based on scope/context
             Err(ParserError::Evaluation(format!("Variable evaluation not implemented for property value: {}", raw_tag)))
        }
        // Handle literal values directly - AstNode::NestedValue wraps the serde_json::Value
        AstNode::NestedValue(v) => {
            println!("Resolving literal property value: {:?}", v);
            // Check if a string literal itself contains tags that need evaluation
            if let Value::String(s) = v {
                 if s.contains("@[") || s.contains("<trigger") || s.contains("{{") || s.contains("{#") {
                    println!("String literal contains tags, re-parsing and evaluating: {:?}", s);
                    // Parse the string content as if it were a new input
                    let inner_resolved_nodes = { // Renamed from inner_ast for clarity
                        // Use parse_entry_content which returns Vec<Box<dyn WorldInfoNode>>
                        parse_entry_content(s, registry)?
                    };

                    // Combine their content into a single string
                    let mut combined_content = String::new();
                    for res_node in inner_resolved_nodes { // Iterate over Vec<Box<dyn WorldInfoNode>>
                         match res_node.content() {
                             Ok(content) => combined_content.push_str(&content),
                             Err(e) => return Err(ParserError::ProcessorExecution(format!("Failed to get content from re-parsed string node: {}", e))),
                         }
                    }
                    println!("Re-parsed string evaluated to: {:?}", combined_content);
                    Ok(Value::String(combined_content))
                 } else {
                    // String literal has no tags, use it directly
                    Ok(v.clone())
                 }
            } else {
                // Not a string, just clone the literal value (Number, Bool, Null)
                Ok(v.clone())
            }
        }
        // Handle nested arrays/objects stored in the AST
        AstNode::NestedArray(items) => {
             println!("Resolving array property");
             // Recursively resolve each AST node item in the array to a JSON value
            let resolved_items = items.iter()
                .map(|item_node| resolve_property_value_to_json(item_node, registry /*, variable_scopes */))
                .collect::<Result<Vec<_>, _>>()?; // Collect results into Vec<Value>
            Ok(Value::Array(resolved_items))
        }
        AstNode::NestedObject(props) => {
             println!("Resolving object property");
            // Recursively resolve the nested object's properties Vec<(String, AstNode)> into a JSON Value::Object
            resolve_properties_to_json(props, registry /*, variable_scopes */)
        }
    }
}
