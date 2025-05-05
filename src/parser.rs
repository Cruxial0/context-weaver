// src/parser.rs

use crate::core::nodes::empty::EmptyNode;
// --- Crates ---
use crate::core::nodes::{TextNode, VariableNode};
use crate::errors::ParserError;
use crate::registry::{ActivationResolver, FunctionResolver, PluginBridge, ScopedRegistry, VariableResolver};
use crate::WorldInfoNode;
// Placeholder imports
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use pest::pratt_parser::{Op, PrattParser};
use regex::RegexBuilder;
use serde_json::{json, Map, Value};
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Peekable;
use lazy_static::lazy_static;
// --- Add log crate ---
use log::{debug, error, info, trace, warn}; // Import log macros

// --- AST Definitions (Unchanged) ---
#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Value),
    Variable { scope: String, name: String, _raw_tag: String },
    Processor { name: String, properties: Vec<(String, AstNode)>, raw_tag: String },
    UnaryOperation { operator: UnaryOperator, operand: Box<Expression> },
    BinaryOperation { left: Box<Expression>, operator: BinaryOperator, right: Box<Expression> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Logical
    And, Or,
    // Comparison
    Eq, Neq, Lt, Gt, Lte, Gte,
    // Arithmetic
    Add, Sub, Mul, Div,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Text(String),
    Processor { name: String, properties: Vec<(String, AstNode)>, raw_tag: String },
    ModFunction { name: String, parameters: Vec<AstNode>, raw_tag: String },
    Trigger { id: String, _raw_tag: String },
    Variable { scope: String, name: String, _raw_tag: String },
    MacroIf { condition: Box<Expression>, then_branch: Vec<AstNode>, else_branch: Option<Vec<AstNode>>, raw_tag: String },
    MacroForeach { item_variable: String, collection: Box<Expression>, body: Vec<AstNode>, raw_tag: String },
    IteratorReference { name: String, raw_tag: String },
    NestedValue(Value),
    NestedArray(Vec<AstNode>),
    NestedObject(Vec<(String, AstNode)>),
}

// --- Derive the Pest Parser (Unchanged) ---
#[derive(Parser)]
#[grammar = "parser.pest"]
struct WorldInfoParser;

// --- Pratt Parser Definition (Unchanged) ---
lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::Assoc::*;
        use Rule::*;

        PrattParser::new()
            .op(Op::infix(or_op, Left))
            .op(Op::infix(and_op, Left))
            .op(Op::infix(comp_op, Left))
            .op(Op::infix(add_op, Left) | Op::infix(sub_op, Left))
            .op(Op::infix(mul_op, Left) | Op::infix(div_op, Left))
            .op(Op::prefix(not_op))
    };
}

// --- Expression Parsing Function using PrattParser ---

/// Parses pairs representing an expression using the Pratt parser.
fn parse_expression_pratt(pairs: Pairs<Rule>) -> Result<Expression, ParserError> {
    trace!("Entering parse_expression_pratt");
    PRATT_PARSER
        .map_primary(|primary| {
            trace!("Pratt primary rule: {:?}", primary.as_rule());
            match primary.as_rule() {
            // --- Handle structural rules by recursing ---
            Rule::expression | Rule::logical_or | Rule::logical_and | Rule::comparison |
            Rule::addition_subtraction | Rule::multiplication_division | Rule::unary_prefix | Rule::term => {
                trace!("Pratt descending into structural rule: {:?}", primary.as_rule());
                parse_expression_pratt(primary.into_inner())
            }
            // --- Actual Primary Terms ---
            Rule::literal => {
                match parse_literal(primary.clone())? {
                    AstNode::NestedValue(v) => Ok(Expression::Literal(v)),
                    other => {
                        error!("Expected literal value from literal rule, got {:?}", other);
                        Err(ParserError::Processing(format!("Expected literal value from literal rule, got non-NestedValue AST node")))
                    }
                }
            }
            Rule::variable => {
                match parse_atomic_variable(primary.clone())? {
                    AstNode::Variable { scope, name, _raw_tag } => Ok(Expression::Variable { scope, name, _raw_tag }),
                    other => {
                        error!("Expected variable node from atomic parse, got {:?}", other);
                        Err(ParserError::Processing(format!("Expected variable node from atomic parse, got non-Variable AST node")))
                    }
                }
            }
            Rule::processor => {
                match parse_atomic_processor(primary.clone())? {
                    AstNode::Processor { name, properties, raw_tag } => Ok(Expression::Processor { name, properties, raw_tag }),
                     other => {
                        error!("Expected processor node from atomic parse, got {:?}", other);
                        Err(ParserError::Processing(format!("Expected processor node from atomic parse, got non-Processor AST node")))
                    }
                }
            }
            // Allow direct matching of literal components
            Rule::number | Rule::boolean | Rule::null | Rule::quoted_string => {
                match parse_literal(primary.clone())? {
                    AstNode::NestedValue(v) => Ok(Expression::Literal(v)),
                    other => {
                        error!("Expected literal value from specific literal rule, got {:?}", other);
                        Err(ParserError::Processing(format!("Expected literal value from specific literal rule, got non-NestedValue AST node")))
                    }
                }
            }
            Rule::string => { // Handle silent string rule
                let inner = primary.clone().into_inner().next().ok_or_else(|| ParserError::Processing("Empty string rule".to_string()))?;
                match parse_literal(inner)? {
                    AstNode::NestedValue(v) => Ok(Expression::Literal(v)),
                    other => {
                        error!("Expected literal value from string rule, got {:?}", other);
                        Err(ParserError::Processing(format!("Expected literal value from string rule, got non-NestedValue AST node")))
                    }
                }
            }
            rule => {
                error!("Unexpected primary rule in Pratt parser: {:?} ({})", rule, primary.as_str());
                Err(ParserError::Processing(format!("Unexpected primary rule: {:?}", rule)))
            }
        }})
        .map_prefix(|op, rhs| {
            trace!("Pratt prefix op: {:?}, rhs: {:?}", op.as_rule(), rhs);
            let rhs_expr = rhs?;
            match op.as_rule() {
                Rule::not_op => Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::Not,
                    operand: Box::new(rhs_expr),
                }),
                rule => {
                    error!("Unexpected prefix operator: {:?}", rule);
                    Err(ParserError::Processing(format!("Unexpected prefix operator: {:?}", rule)))
                }
            }
        })
        .map_infix(|lhs, op, rhs| {
            trace!("Pratt infix op: {:?}, lhs: {:?}, rhs: {:?}", op.as_rule(), lhs, rhs);
            let lhs_expr = lhs?;
            let rhs_expr = rhs?;
            let operator = match op.as_rule() {
                Rule::or_op => BinaryOperator::Or,
                Rule::and_op => BinaryOperator::And,
                Rule::comp_op => match op.as_str() {
                    "==" => BinaryOperator::Eq, "!=" => BinaryOperator::Neq,
                    "<" => BinaryOperator::Lt, ">" => BinaryOperator::Gt,
                    "<=" => BinaryOperator::Lte, ">=" => BinaryOperator::Gte,
                    _ => {
                        error!("Unknown comparison operator: {}", op.as_str());
                        return Err(ParserError::Processing(format!("Unknown comparison operator: {}", op.as_str())));
                    }
                },
                Rule::add_op => BinaryOperator::Add, Rule::sub_op => BinaryOperator::Sub,
                Rule::mul_op => BinaryOperator::Mul, Rule::div_op => BinaryOperator::Div,
                rule => {
                    error!("Unexpected infix operator: {:?}", rule);
                    return Err(ParserError::Processing(format!("Unexpected infix operator: {:?}", rule)));
                }
            };
            Ok(Expression::BinaryOperation {
                left: Box::new(lhs_expr),
                operator,
                right: Box::new(rhs_expr),
            })
        })
        .parse(pairs) // Parse the input pairs
}


// --- Public Entry Point ---

/// Parses the raw input string into resolved WorldInfoNode objects.
pub fn parse_entry_content<P: PluginBridge + Debug>(
    raw: &str,
) -> Result<Vec<AstNode>, ParserError> {
    info!("Parsing input (first 50 chars): {:?}", raw.chars().take(50).collect::<String>());
    trace!("Full raw input: {:?}", raw); // Use trace for potentially large input
    let pairs = WorldInfoParser::parse(Rule::input, raw)
        .map_err(|e| {
            error!("Pest parsing failed: {}", e);
            ParserError::PestParse(e)
        })?;
    trace!("Initial Pest pairs: {:?}", pairs); // Trace for potentially verbose output

    let input_pair = pairs.peek().ok_or_else(|| {
        error!("Empty parse result from Pest");
        ParserError::Processing("Empty parse result".to_string())
    })?;
    if input_pair.as_rule() != Rule::input {
        error!("Expected Rule::input from Pest, found {:?}", input_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected Rule::input, found {:?}", input_pair.as_rule())));
    }

    let mut inner_pairs = input_pair.clone().into_inner().peekable();
    let raw_ast = build_ast_from_pairs::<P>(&mut inner_pairs)?;
    debug!("Built AST: {:?}", raw_ast); // Debug level for AST structure
    Ok(optimize_ast_nodes(raw_ast))
}

pub fn evaluate_nodes<P: PluginBridge + Debug>(
    nodes: &[AstNode],
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    let resolved = resolve_ast_nodes(nodes, registry, entry_id, loop_context)?;
    debug!("Resolved Nodes (count: {}): {:?}", resolved.len(), resolved.iter().map(|n| n.name()).collect::<Vec<_>>());
    Ok(resolved)
}

/// Internal function for parsing and evaluating content from string.
/// 
/// Used for recursive evaluation of nested or looping entries
fn parse_and_evaluate<P: PluginBridge + Debug>(
    raw: &str,
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    let nodes = parse_entry_content::<P>(raw)?;
    evaluate_nodes(&nodes, registry, entry_id, loop_context)
}

/// Parses an activation condition string and evaluates it against the context.
///
/// Activation conditions can be:
/// 1. Simple keywords (case-insensitive match against context).
/// 2. Keywords that are variables or processors (evaluated, then matched).
/// 3. Regex patterns (`/pattern/flags`) where the pattern itself can contain evaluated variables/processors.
/// 4. Comparison expressions involving variables, processors, etc.
pub fn parse_activation_condition<P: PluginBridge + Debug>(
    condition_str: &str,
    context: &str, // The evaluated text context to match keywords/regex against
    registry: &mut ScopedRegistry<P>,
    entry_id: &String, // Used for resolving processors/variables within expressions
) -> Result<bool, ParserError> {
    info!("Parsing activation condition: '{}'", condition_str);
    trace!("Context for condition: '{}'", context);

    // 1. Parse the condition string using the specific entry rule
    let mut pairs = WorldInfoParser::parse(Rule::cond_input, condition_str)
        .map_err(|e| {
            error!("Pest parsing failed for activation condition '{}': {}", condition_str, e);
            ParserError::PestParse(e.with_path(&format!("activation condition: {}", condition_str)))
        })?;

    // Expect SOI ~ condition ~ EOI
    let cond_input_pair = pairs.next().ok_or_else(|| {
        error!("Empty parse result for activation condition: {}", condition_str);
        ParserError::Processing(format!("Empty parse result for activation condition: {}", condition_str))
    })?;
    if cond_input_pair.as_rule() != Rule::cond_input {
        error!("Expected Rule::cond_input for activation condition, found {:?}", cond_input_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected Rule::cond_input for activation condition, found {:?}", cond_input_pair.as_rule())));
    }

    // Get the actual condition rule inside cond_input
    let condition_pair = cond_input_pair.into_inner()
        .find(|p| p.as_rule() == Rule::condition)
        .ok_or_else(|| {
            error!("Could not find Rule::condition within Rule::cond_input for: {}", condition_str);
            ParserError::Processing(format!("Could not find Rule::condition within Rule::cond_input for: {}", condition_str))
        })?;

    // 2. Determine the type of condition based on its *first inner* rule
    let inner_condition = condition_pair.clone().into_inner().next().ok_or_else(|| {
        error!("Rule::condition was empty for: {}", condition_str);
        ParserError::Processing(format!("Rule::condition was empty for: {}", condition_str))
    })?;

    match inner_condition.as_rule() {
        // --- Keyword Condition (handles simple, variable, or processor) ---
        Rule::keyword_condition => {
            // The keyword_condition rule itself is non-atomic. Get its inner content.
            let actual_keyword_content = inner_condition.into_inner().next().ok_or_else(|| {
                error!("keyword_condition rule was empty for: {}", condition_str);
                ParserError::Processing(format!("keyword_condition rule was empty for: {}", condition_str))
            })?;

            // Determine if it's a variable, processor, or simple keyword and get its string value
            let keyword_value_str = match actual_keyword_content.as_rule() {
                Rule::variable => {
                    let var_tag = actual_keyword_content.as_str();
                    debug!("Activation Condition Type: Keyword (Variable: '{}')", var_tag);
                    // Parse the variable AST node
                    let var_node = parse_variable_content::<P>(actual_keyword_content)?;
                    // Evaluate the variable node to a string value using existing resolver logic
                    let resolved_val = resolve_property_value_to_json(&var_node, registry, entry_id, None)?;
                    value_to_string_for_keyword(&resolved_val)
                        .ok_or_else(|| ParserError::Evaluation(format!(
                            "Variable '{}' used as keyword did not evaluate to a string or number", var_tag
                        )))?
                }
                Rule::processor => {
                    let proc_tag = actual_keyword_content.as_str();
                    debug!("Activation Condition Type: Keyword (Processor: '{}')", proc_tag);
                    // Parse the processor AST node
                    let proc_node = parse_processor_content::<P>(actual_keyword_content)?;
                    // Evaluate the processor node to a string value using existing resolver logic
                    let resolved_val = resolve_property_value_to_json(&proc_node, registry, entry_id, None)?;
                    value_to_string_for_keyword(&resolved_val)
                        .ok_or_else(|| ParserError::Evaluation(format!(
                            "Processor '{}' used as keyword did not evaluate to a string or number", proc_tag
                        )))?
                }
                Rule::simple_keyword => {
                    let keyword = actual_keyword_content.as_str().trim();
                    debug!("Activation Condition Type: Keyword (Simple: '{}')", keyword);
                    keyword.to_string()
                }
                // This shouldn't happen if the grammar is correct
                rule => {
                    error!("Unexpected rule {:?} found inside keyword_condition for '{}'", rule, condition_str);
                    return Err(ParserError::InvalidRule(rule));
                }
            };

            // Perform the check
            if keyword_value_str.is_empty() {
                warn!("Evaluated keyword condition resulted in an empty string. Treated as false.");
                return Ok(false);
            }
            // Perform case-insensitive search using the evaluated/extracted keyword string
            let result = context.to_lowercase().contains(&keyword_value_str.to_lowercase());
            debug!("Keyword search ('{}') result in context: {}", keyword_value_str, result);
            Ok(result)
        }

        // --- Regex Condition ---
        Rule::regex_condition => {
            debug!("Activation Condition Type: Regex ('{}')", inner_condition.as_str());
            let mut inner_regex_pairs = inner_condition.into_inner(); // Pairs inside regex_condition rule: regex_pattern, regex_flags?

            let pattern_pair = inner_regex_pairs.next().ok_or_else(|| ParserError::Processing("Regex condition missing pattern".to_string()))?;
            if pattern_pair.as_rule() != Rule::regex_pattern {
                error!("Expected regex_pattern, got {:?}", pattern_pair.as_rule());
                return Err(ParserError::Processing(format!("Expected regex_pattern, got {:?}", pattern_pair.as_rule())));
            }

            // --- Evaluate the pattern ---
            let mut evaluated_pattern = String::new();
            trace!("Evaluating regex pattern content: {}", pattern_pair.as_str());
            for inner_pair in pattern_pair.into_inner() { // Iterate through pairs inside regex_pattern: escape | variable | processor | text_in_regex
                 match inner_pair.as_rule() {
                    Rule::variable => {
                        let var_tag = inner_pair.as_str();
                        trace!("  Evaluating variable in regex pattern: {}", var_tag);
                        let var_node = parse_variable_content::<P>(inner_pair)?;
                        let resolved_val = resolve_property_value_to_json(&var_node, registry, entry_id, None)?;
                        let value_str = value_to_string_for_keyword(&resolved_val)
                            .ok_or_else(|| ParserError::Evaluation(format!(
                                "Variable '{}' inside regex pattern did not evaluate to a string or number", var_tag
                            )))?;
                        evaluated_pattern.push_str(&value_str);
                    }
                    Rule::processor => {
                        let proc_tag = inner_pair.as_str();
                        trace!("  Evaluating processor in regex pattern: {}", proc_tag);
                        let proc_node = parse_processor_content::<P>(inner_pair)?;
                        let resolved_val = resolve_property_value_to_json(&proc_node, registry, entry_id, None)?;
                        let value_str = value_to_string_for_keyword(&resolved_val)
                            .ok_or_else(|| ParserError::Evaluation(format!(
                                "Processor '{}' inside regex pattern did not evaluate to a string or number", proc_tag
                            )))?;
                        evaluated_pattern.push_str(&value_str);
                    }
                    Rule::text_in_regex => {
                        trace!("  Appending text in regex pattern: {}", inner_pair.as_str());
                        evaluated_pattern.push_str(inner_pair.as_str());
                    }
                    Rule::escape => {
                        // Handle the escape sequence by unescaping it and appending
                        trace!("  Appending escaped char in regex pattern: {}", inner_pair.as_str());
                        let unescaped_char = unescape_char_for_regex(inner_pair)?;
                        evaluated_pattern.push(unescaped_char);
                    }
                        rule => {
                        error!("Unexpected rule {:?} found inside regex_pattern for '{}'", rule, condition_str);
                        return Err(ParserError::InvalidRule(rule));
                    }
                }
            }
            debug!("Evaluated regex pattern: '{}'", evaluated_pattern);
            // --- End pattern evaluation ---

            let flags_pair = inner_regex_pairs.next(); // Flags are optional
            let flags = flags_pair.map(|p| p.as_str()).unwrap_or("");
            trace!("Regex flags: '{}'", flags);

            // Build regex with flags using the *evaluated* pattern
            let mut builder = RegexBuilder::new(&evaluated_pattern); // Use evaluated_pattern here
            builder.case_insensitive(flags.contains('i'));
            builder.multi_line(flags.contains('m'));
            builder.dot_matches_new_line(flags.contains('s'));
            // Note: 'g' (global) isn't directly set, `is_match` finds *any* match.

            let regex = builder.build().map_err(|e| {
                error!("Invalid regex pattern '{}' (evaluated from '{}', flags '{}'): {}", evaluated_pattern, condition_str, flags, e);
                ParserError::RegexCompilation(evaluated_pattern.to_string(), e.to_string())
            })?;

            let result = regex.is_match(context);
            debug!("Regex match result: {}", result);
            Ok(result)
        }

        // --- Comparison Condition (Expression) ---
        Rule::comparison_condition => {
            debug!("Activation Condition Type: Comparison ('{}')", inner_condition.as_str());
            // The comparison_condition directly contains an expression
            let expression_pairs = inner_condition.into_inner();

            // Parse the expression using the Pratt parser
            let expression_ast = parse_expression_pratt(expression_pairs)?;
            debug!("Parsed comparison expression AST: {:?}", expression_ast);

            // Evaluate the expression (no loop context needed here)
            let evaluation_result = evaluate_expression(&expression_ast, registry, entry_id, None)?;
            debug!("Evaluated comparison expression result: {:?}", evaluation_result);

            // Check truthiness of the result
            let result = is_truthy(&evaluation_result);
            debug!("Comparison expression truthiness: {}", result);
            Ok(result)
        }

        // Should not happen if grammar is correct and condition rule is exhaustive
        rule => {
            error!("Unexpected rule type {:?} found directly inside Rule::condition for '{}'", rule, condition_str);
            Err(ParserError::InvalidRule(rule))
        }
    }
}

/// Helper function to convert a JSON Value to a string suitable for keyword matching
/// or for inserting into a regex pattern.
/// Handles String and Number types. Returns None for others.
fn value_to_string_for_keyword(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        // Decide how to handle other types (e.g., bool, null, array, object)
        // Currently, they won't match as keywords or be inserted into regex patterns.
        _ => None,
    }
}

/// Helper function to unescape a single character from an `escape` rule pair, specifically for regex patterns.
/// This is simpler than `unescape_string` as it only handles one char at a time.
fn unescape_char_for_regex(pair: Pair<Rule>) -> Result<char, ParserError> {
    if pair.as_rule() != Rule::escape {
        return Err(ParserError::Processing(format!("Expected escape rule, got {:?}", pair.as_rule())));
    }
    let mut inner = pair.into_inner(); // Should contain '\' and the escaped char/sequence
    let _backslash = inner.next(); // Consume the backslash itself
    let escaped_part = inner.next().ok_or_else(|| ParserError::Processing("Empty escape sequence".to_string()))?;

    match escaped_part.as_str() {
        "\"" => Ok('"'), "\\" => Ok('\\'), "/" => Ok('/'),
        "b" => Ok('\u{0008}'), "f" => Ok('\u{000C}'), "n" => Ok('\n'),
        "r" => Ok('\r'), "t" => Ok('\t'),
        // Handle unicode escape
        u if u.starts_with('u') => {
            let hex_code = &u[1..]; // Get the 4 hex digits
            if hex_code.len() == 4 && hex_code.chars().all(|c| c.is_ascii_hexdigit()) {
                 let code_point = u32::from_str_radix(hex_code, 16).map_err(|_| {
                     ParserError::Processing(format!("Invalid unicode escape sequence: failed to parse hex \\u{}", hex_code))
                 })?;
                 std::char::from_u32(code_point).ok_or_else(|| {
                     ParserError::Processing(format!("Invalid unicode code point: {}", code_point))
                 })
            } else {
                Err(ParserError::Processing(format!("Invalid unicode escape sequence: \\u{}", hex_code)))
            }
        }
        // If it's none of the known escapes, just return the character after the backslash literally
        other if other.chars().count() == 1 => Ok(other.chars().next().unwrap()),
        _ => Err(ParserError::Processing(format!("Invalid escape sequence: \\{}", escaped_part.as_str()))),
    }
}

/// Helper function to optimize AST nodes
/// 
/// - Combines adjacent Text nodes
fn optimize_ast_nodes(ast_nodes: Vec<AstNode>) -> Vec<AstNode> {
    let mut optimized_nodes = Vec::new();
    let mut current_text = String::new();
    for node in ast_nodes {
        match node {
            AstNode::Text(text) => current_text.push_str(&text),
            _ => {
                if current_text.len() > 0 {
                    optimized_nodes.push(AstNode::Text(current_text));
                    current_text = String::new();
                }
                optimized_nodes.push(node);
            }
        }
    }
    if current_text.len() > 0 {
        optimized_nodes.push(AstNode::Text(current_text));
    }
    optimized_nodes
}

// --- AST Building ---

/// Builds the AST from a *peekable iterator* of pest pairs.
fn build_ast_from_pairs<'i, P: PluginBridge + Debug>(
    pairs: &mut Peekable<Pairs<'i, Rule>>,
) -> Result<Vec<AstNode>, ParserError> {
    trace!("Entering build_ast_from_pairs");
    let mut nodes = Vec::new();

    while let Some(pair) = pairs.peek() {
        let current_pair = pair.clone();
        trace!("AST build loop - current rule: {:?}", current_pair.as_rule());
        match current_pair.as_rule() {
            Rule::text => {
                let text_pair = pairs.next().unwrap();
                trace!("AST build: Text node: {:?}", text_pair.as_str());
                nodes.push(AstNode::Text(text_pair.as_str().to_string()));
            }
            Rule::processor => {
                let content_pair = pairs.next().unwrap();
                trace!("AST build: Parsing processor content: {:?}", content_pair.as_str());
                nodes.push(parse_processor_content::<P>(content_pair)?);
            }
            Rule::mod_function => {
                let content_pair = pairs.next().unwrap();
                trace!("AST build: Parsing mod function content: {:?}", content_pair.as_str());
                nodes.push(parse_mod_function_content::<P>(content_pair)?);
            }
            Rule::trigger => {
                let content_pair = pairs.next().unwrap();
                trace!("AST build: Parsing trigger content: {:?}", content_pair.as_str());
                nodes.push(parse_trigger_content::<P>(content_pair)?);
            }
            Rule::variable => {
                let content_pair = pairs.next().unwrap();
                trace!("AST build: Parsing variable content: {:?}", content_pair.as_str());
                nodes.push(parse_variable_content::<P>(content_pair)?);
            }
            Rule::macro_tag => {
                let macro_container_pair = pairs.next().unwrap();
                trace!("AST build: Parsing macro tag: {:?}", macro_container_pair.as_str());
                let macro_pair = macro_container_pair.clone().into_inner().next()
                    .ok_or_else(|| {
                        error!("Empty 'macro_tag' pair: {:?}", macro_container_pair.as_str());
                        ParserError::Processing(format!("Empty 'macro_tag' pair: {:?}", macro_container_pair.as_str()))
                    })?;
                match macro_pair.as_rule() {
                    Rule::macro_if => nodes.push(parse_macro_if::<P>(macro_pair)?),
                    Rule::macro_foreach => nodes.push(parse_macro_foreach::<P>(macro_pair)?),
                    r => {
                        error!("Invalid rule inside macro_tag: {:?}", r);
                        return Err(ParserError::InvalidRule(r));
                    }
                }
            }
            Rule::iterator_reference => {
                let content_pair = pairs.next().unwrap();
                trace!("AST build: Parsing iterator reference content: {:?}", content_pair.as_str());
                nodes.push(parse_iterator_reference_content::<P>(content_pair)?);
            }
            Rule::EOI => {
                trace!("AST build: Reached EOI");
                pairs.next();
                break;
            }
            Rule::WHITESPACE => {
                trace!("AST build: Skipping WHITESPACE");
                pairs.next();
            }
            r => {
                let unexpected_pair = pairs.next().unwrap();
                // Use warn! here if it's potentially recoverable or just unexpected structure
                // Use error! if it signifies a definite parsing failure
                error!("Unexpected rule during AST building: {:?} ({:?})", r, unexpected_pair.as_str());
                if matches!(r, Rule::processor | Rule::trigger | Rule::variable) {
                    error!("Critical Error: Content rule {:?} encountered unexpectedly in main loop.", r);
                }
                return Err(ParserError::InvalidRule(r));
            }
        }
    }
    trace!("Exiting build_ast_from_pairs");
    Ok(nodes)
}

fn parse_mod_function_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>
) -> Result<AstNode, ParserError> {
    trace!("Parsing mod function content: {:?}", pair.as_str());
    if pair.as_rule() != Rule::mod_function {
        error!("Expected mod_function, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected mod_function, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string();
    let mut inner = pair.clone().into_inner();

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected mod_start in content".to_string()))?;
    let name_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected mod_name in content".to_string()))?;
    if name_pair.as_rule() != Rule::namespace_identifier {
        error!("Expected mod_name, got {:?}", name_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected mod_name, got {:?}", name_pair.as_rule())));
    }
    let name = name_pair.as_str().to_string();
    
    // Get the parameters pair
    let parameters_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected mod_parameters in content".to_string()))?;
    if parameters_pair.as_rule() != Rule::parameters {
        error!("Expected mod_parameters, got {:?}", parameters_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected mod_parameters, got {:?}", parameters_pair.as_rule())));
    }

    // Parse each parameter value
    let mut parameters = Vec::new();
    for param_pair_outer in parameters_pair.into_inner() {
        // Each param_pair should be a property_value according to your grammar
        if param_pair_outer.as_rule() != Rule::parameter_value {
            error!("Expected property_value, got {:?}", param_pair_outer.as_rule());
            return Err(ParserError::Processing(format!("Expected property_value, got {:?}", param_pair_outer.as_rule())));
        }

        let param_pair = param_pair_outer.clone().into_inner().next().ok_or_else(|| ParserError::Processing("Expected parameter in content".to_string()))?;

        let param_node = match param_pair.as_rule() {
            Rule::processor => parse_processor_content::<P>(param_pair)?,
            Rule::variable => parse_variable_content::<P>(param_pair)?,
            Rule::iterator_reference => parse_iterator_reference_content::<P>(param_pair)?,
            Rule::array | Rule::object | Rule::literal | Rule::string_content | Rule::number => parse_property_value(param_pair)?,
            _ => {
                error!("Unexpected parameter type: {:?}", param_pair.as_rule());
                return Err(ParserError::Processing(
                    format!("Unexpected parameter type: {:?}", param_pair.as_rule())
                ));
            }
        };
        parameters.push(param_node);
    }

    // Check for the end tag
    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected mod_end in content".to_string()))?;
    if _end_pair.as_rule() != Rule::processor_end {
        error!("Expected processor_end, got {:?}", _end_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected processor_end, got {:?}", _end_pair.as_rule())));
    }

    // Make sure there's nothing else
    if inner.next().is_some() {
        error!("Unexpected additional content in mod_function");
        return Err(ParserError::Processing("Unexpected additional content in mod_function".to_string()));
    }

    // Return the completed ModFunction node with parameters
    Ok(AstNode::ModFunction { 
        name, 
        parameters, 
        raw_tag:raw_tag_string 
    })
}
/// Parses a processor_tag_content pair.
fn parse_processor_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    trace!("Parsing processor content: {:?}", pair.as_str());
    if pair.as_rule() != Rule::processor {
        error!("Expected processor_tag_content, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected processor_tag_content, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string();
    let mut inner = pair.clone().into_inner();

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_start in content".to_string()))?;
    let name_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_name in content".to_string()))?;
    if name_pair.as_rule() != Rule::namespace_identifier {
        error!("Expected processor_name, got {:?}", name_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected processor_name, got {:?}", name_pair.as_rule())));
    }
    let name = name_pair.as_str().to_string();
    trace!("Processor name: {}", name);

    let properties = if let Some(props_pair) = inner.peek() {
        if props_pair.as_rule() == Rule::properties {
            let consumed_props_pair = inner.next().unwrap();
            trace!("Parsing processor properties: {:?}", consumed_props_pair.as_str());
            parse_properties(consumed_props_pair)?
        } else {
            trace!("No properties rule found for processor {}", name);
            Vec::new()
        }
    } else {
        trace!("No inner pairs after name for processor {}", name);
        Vec::new()
    };
    trace!("Processor properties parsed (count: {})", properties.len());

    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected processor_end in content".to_string()))?;

    Ok(AstNode::Processor { name, properties, raw_tag: raw_tag_string })
}


/// Parses a trigger_tag_content pair.
fn parse_trigger_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    trace!("Parsing trigger content: {:?}", pair.as_str());
    if pair.as_rule() != Rule::trigger {
        error!("Expected trigger rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected trigger rule, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string();
    let mut inner = pair.clone().into_inner(); // Inner pairs of the main 'trigger' rule

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected trigger_start in content".to_string()))?;

    let mut trigger_id: Option<String> = None;

    // Check for optional attributes part
    if let Some(attrs_pair) = inner.peek() {
        // Check if the next pair is trigger_attributes
        if attrs_pair.as_rule() == Rule::trigger_attributes {
            let consumed_attrs = inner.next().unwrap(); // Consume the attributes pair
            trace!("Parsing trigger attributes: {:?}", consumed_attrs.as_str());

            // Iterate through each attribute inside trigger_attributes
            // Filter for trigger_attribute rule specifically to ignore potential whitespace etc.
            for attr_pair in consumed_attrs.clone().into_inner().filter(|p| p.as_rule() == Rule::trigger_attribute) {
                trace!("Parsing trigger_attribute: {:?}", attr_pair.as_str());
                let mut attr_inner = attr_pair.clone().into_inner(); // Inner parts of trigger_attribute: key, value

                let key_pair = attr_inner.next().ok_or_else(|| ParserError::Processing(format!("Trigger attribute missing key in '{}'", attr_pair.as_str())))?;

                // Due to the chain of SILENT rules (trigger_value -> quoted_string),
                // value_pair is expected to be the string_content pair directly.
                let value_pair = attr_inner.next().ok_or_else(|| ParserError::Processing(format!("Trigger attribute missing value in '{}'", attr_pair.as_str())))?;

                trace!("Attribute key: '{}', value rule: {:?}", key_pair.as_str(), value_pair.as_rule());

                // Check the key rule and its content
                if key_pair.as_rule() == Rule::trigger_key && key_pair.as_str() == "id" {
                    // --- Correction Start ---
                    // Now expect the value pair rule to be string_content
                    if value_pair.as_rule() == Rule::string_content {
                        // The value_pair *is* the string_content. It's atomic (@), so get its string value directly.
                        let content = value_pair.as_str();

                        // Still unescape, as escape sequences might be handled differently by Pest vs string literals.
                        // Although the string_content rule itself avoids quotes/backslashes needed for JSON escape,
                        // the escape rule allows \n, \t etc. which might be present.
                        trigger_id = Some(unescape_string(content)?);
                        trace!("Found trigger id: \"{}\"", trigger_id.as_ref().unwrap());
                    } else {
                        // This case would mean the grammar parsing produced something unexpected
                        // for the value part of the id attribute.
                        error!("Invalid trigger id value type: expected Rule::string_content due to nested silent rules, but got {:?} for value pair '{}'", value_pair.as_rule(), value_pair.as_str());
                        return Err(ParserError::Processing(format!("Invalid trigger id value type: expected string_content, got {:?}", value_pair.as_rule())));
                    }
                    // --- Correction End ---
                } else {
                    // Handle cases where the key is not "id" or the key rule itself is wrong
                    warn!("Ignoring attribute with unknown or invalid key '{}' (Rule: {:?})", key_pair.as_str(), key_pair.as_rule());
                }
            }
        } else {
            trace!("No trigger_attributes rule found after trigger_start.");
            // It's valid to have <trigger> with no attributes if grammar allowed, but ours requires id.
            // If attributes are optional but ID required, the check later handles it.
        }
    } else {
        // This means no trigger_attributes and no trigger_end followed trigger_start
        trace!("No inner pairs found after trigger_start (missing attributes and end tag).");
        return Err(ParserError::Processing(format!("Malformed trigger tag '{}': missing attributes or end tag", raw_tag_string)));
    }


    // Ensure trigger_end is present and is the next token
    let end_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Expected trigger_end after attributes for tag: {}", raw_tag_string)))?;
    if end_pair.as_rule() != Rule::trigger_end {
        error!("Expected trigger_end, found {:?} for tag: {}", end_pair.as_rule(), raw_tag_string);
        return Err(ParserError::Processing(format!("Expected trigger_end, found {:?}", end_pair.as_rule())));
    }

    // Check if there's anything unexpected after trigger_end within the main trigger rule
    if inner.next().is_some() {
        // This might happen if the grammar was like trigger = { start ~ attrs? ~ end ~ WHITESPACE* } and there was trailing space
        // For the current grammar, this shouldn't happen if the input is valid.
        warn!("Unexpected content found after trigger_end within trigger rule for tag: {}", raw_tag_string);
    }

    // Ensure the ID attribute was successfully found and parsed
    let id = trigger_id.ok_or_else(|| {
        // This error occurs if the loop finished without finding a valid 'id' attribute key/value pair.
        error!("Trigger tag missing required 'id' attribute or attribute was invalid: {}", raw_tag_string);
        ParserError::MissingTriggerId(raw_tag_string.clone())
    })?;

    Ok(AstNode::Trigger { id, _raw_tag: raw_tag_string })
}

/// Parses a variable_tag_content pair.
fn parse_variable_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    trace!("Parsing variable content: {:?}", pair.as_str());
    if pair.as_rule() != Rule::variable {
        error!("Expected variable_tag_content, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected variable_tag_content, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    let mut inner = pair.clone().into_inner();

    let _start_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected variable_start in content".to_string()))?;

    let scope_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected scope in variable content".to_string()))?;
    if scope_pair.as_rule() != Rule::scope {
        error!("Expected scope, got {:?}", scope_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected scope, got {:?}", scope_pair.as_rule())));
    }

    let _sep_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected separator in variable content".to_string()))?;

    let name_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected name in variable content".to_string()))?;
    if name_pair.as_rule() != Rule::name {
        error!("Expected name, got {:?}", name_pair.as_rule());
        return Err(ParserError::Processing(format!("Expected name, got {:?}", name_pair.as_rule())));
    }

    let _end_pair = inner.next().ok_or_else(|| ParserError::Processing("Expected variable_end in content".to_string()))?;

    let scope = scope_pair.as_str().to_string();
    let name = name_pair.as_str().to_string();
    trace!("Parsed variable: scope='{}', name='{}'", scope, name);

    Ok(AstNode::Variable { scope, name, _raw_tag: raw_tag })
}


// --- Macro, Property, Literal Parsing Functions ---

/// Helper function to trim leading/trailing whitespace from the first/last Text nodes.
fn trim_outer_text_nodes(nodes: &mut Vec<AstNode>) {
    if let Some(AstNode::Text(content)) = nodes.first_mut() {
        let trimmed = content.trim_start();
        if trimmed.len() < content.len() {
            trace!("Trimmed start whitespace from text node");
            *content = trimmed.to_string();
        }
    }
    if let Some(AstNode::Text(content)) = nodes.last_mut() {
        let trimmed = content.trim_end();
        if trimmed.len() < content.len() {
            trace!("Trimmed end whitespace from text node");
            *content = trimmed.to_string();
        }
    }
}

/// Parses a macro if tag.
fn parse_macro_if<P: PluginBridge + Debug>(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing macro if: {:?}", pair.as_str());
    if pair.as_rule() != Rule::macro_if {
        error!("Expected macro_if rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected macro_if rule, got {:?}", pair.as_rule())));
    }
    let mut inner = pair.clone().into_inner();

    let start_tag_pair = inner.next().ok_or_else(|| ParserError::Processing("If macro missing start tag".to_string()))?;
    let start_tag_str = start_tag_pair.as_str();

    let condition_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing condition: {}", start_tag_str)))?;
    let condition_str = condition_pair.as_str();
    trace!("Parsing if condition expression: {}", condition_str);
    let condition = parse_expression_pratt(condition_pair.into_inner())?;

    let tag_end_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing end '#}}': {}", start_tag_str)))?;
    let tag_end_str = tag_end_pair.as_str();
    let full_raw_start_tag = format!("{}{}{}", start_tag_str, condition_str, tag_end_str);
    trace!("Parsed if start tag: {}", full_raw_start_tag);


    // Parse the 'then' branch nodes
    let then_nodes_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing 'then' branch content: {}", full_raw_start_tag)))?;
    if then_nodes_pair.as_rule() != Rule::inner_nodes {
        error!("Expected inner_nodes for 'then' branch, found {:?} in {}", then_nodes_pair.as_rule(), full_raw_start_tag);
        return Err(ParserError::Processing(format!("Expected inner_nodes for 'then' branch, found {:?} in {}", then_nodes_pair.as_rule(), full_raw_start_tag)));
    }
    trace!("Parsing 'then' branch for: {}", full_raw_start_tag);
    let mut then_inner_pairs = then_nodes_pair.clone().into_inner().peekable();
    let mut then_branch = build_ast_from_pairs::<P>(&mut then_inner_pairs)?;
    trim_outer_text_nodes(&mut then_branch);
    debug!("Parsed 'then' branch AST ({} nodes) for: {}", then_branch.len(), full_raw_start_tag);


    // Check for optional 'else' branch
    let mut else_branch: Option<Vec<AstNode>> = None;
    if let Some(peek_pair) = inner.peek() {
        if peek_pair.as_rule() == Rule::macro_else {
            let else_pair = inner.next().unwrap();
            trace!("Parsing 'else' branch for: {}", full_raw_start_tag);
            let mut else_inner = else_pair.clone().into_inner();
            let _else_tag = else_inner.next().ok_or_else(|| ParserError::Processing("Else macro missing tag".to_string()))?;
            let else_nodes_pair = else_inner.next().ok_or_else(|| ParserError::Processing("Else macro missing content".to_string()))?;
            if else_nodes_pair.as_rule() != Rule::inner_nodes {
                error!("Expected inner_nodes for 'else' branch, found {:?} in {}", else_nodes_pair.as_rule(), full_raw_start_tag);
                return Err(ParserError::Processing(format!("Expected inner_nodes for 'else' branch, found {:?} in {}", else_nodes_pair.as_rule(), full_raw_start_tag)));
            }
            let mut else_inner_pairs = else_nodes_pair.clone().into_inner().peekable();
            let mut parsed_else_branch = build_ast_from_pairs::<P>(&mut else_inner_pairs)?;
            trim_outer_text_nodes(&mut parsed_else_branch);
            debug!("Parsed 'else' branch AST ({} nodes) for: {}", parsed_else_branch.len(), full_raw_start_tag);
            else_branch = Some(parsed_else_branch);
        } else {
            trace!("No 'else' branch found for: {}", full_raw_start_tag);
        }
    }

    // Ensure endif is present
    let endif_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("If macro missing endif tag: {}", full_raw_start_tag)))?;
    if endif_pair.as_rule() != Rule::macro_endif {
        error!("Expected endif tag, found {:?} in {}", endif_pair.as_rule(), full_raw_start_tag);
        return Err(ParserError::Processing(format!("Expected endif tag, found {:?} in {}", endif_pair.as_rule(), full_raw_start_tag)));
    }
    trace!("Found endif tag for: {}", full_raw_start_tag);

    Ok(AstNode::MacroIf {
        condition: Box::new(condition),
        then_branch,
        else_branch,
        raw_tag: full_raw_start_tag,
    })
}

/// Parses a macro foreach tag.
fn parse_macro_foreach<P: PluginBridge + Debug>(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing macro foreach: {:?}", pair.as_str());
    if pair.as_rule() != Rule::macro_foreach {
        error!("Expected macro_foreach rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected macro_foreach rule, got {:?}", pair.as_rule())));
    }
    let mut inner = pair.clone().into_inner();

    let start_tag_pair = inner.next().ok_or_else(|| ParserError::Processing("Foreach macro missing start tag".to_string()))?;
    let start_tag_str = start_tag_pair.as_str();

    let item_var_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing item variable: {}", start_tag_str)))?;
    if item_var_pair.as_rule() != Rule::identifier {
        error!("Expected identifier for item variable in foreach, found {:?}: {}", item_var_pair.as_rule(), start_tag_str);
        return Err(ParserError::Processing(format!("Expected identifier for item variable in foreach, found {:?}: {}", item_var_pair.as_rule(), start_tag_str)));
    }
    let item_variable = item_var_pair.as_str().trim().to_string();
    let item_var_str_full = item_var_pair.as_str();
    trace!("Foreach item variable: {}", item_variable);

    let collection_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing collection after 'in': {}", start_tag_str)))?;
    let collection_str_full = collection_pair.as_str();
    trace!("Parsing foreach collection expression: {}", collection_str_full);
    let collection = parse_expression_pratt(collection_pair.into_inner())?;

    let tag_end_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing end '#}}': {}", start_tag_str)))?;
    let tag_end_str = tag_end_pair.as_str();
    let full_raw_start_tag = format!("{}{}{}{}", start_tag_str, item_var_str_full, collection_str_full, tag_end_str);
    trace!("Parsed foreach start tag: {}", full_raw_start_tag);

    let body_nodes_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing body content: {}", full_raw_start_tag)))?;
    if body_nodes_pair.as_rule() != Rule::inner_nodes {
        error!("Expected inner_nodes for 'foreach' body, found {:?} in {}", body_nodes_pair.as_rule(), full_raw_start_tag);
        return Err(ParserError::Processing(format!("Expected inner_nodes for 'foreach' body, found {:?} in {}", body_nodes_pair.as_rule(), full_raw_start_tag)));
    }
    trace!("Parsing 'foreach' body for: {}", full_raw_start_tag);
    let mut body_inner_pairs = body_nodes_pair.clone().into_inner().peekable();
    let mut body = build_ast_from_pairs::<P>(&mut body_inner_pairs)?;
    trim_outer_text_nodes(&mut body);
    debug!("Parsed 'foreach' body AST ({} nodes) for: {}", body.len(), full_raw_start_tag);


    let endforeach_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Foreach macro missing endforeach tag: {}", full_raw_start_tag)))?;
    if endforeach_pair.as_rule() != Rule::macro_endforeach {
        error!("Expected endforeach tag, found {:?} in {}", endforeach_pair.as_rule(), full_raw_start_tag);
        return Err(ParserError::Processing(format!("Expected endforeach tag, found {:?} in {}", endforeach_pair.as_rule(), full_raw_start_tag)));
    }
    trace!("Found endforeach tag for: {}", full_raw_start_tag);

    Ok(AstNode::MacroForeach {
        item_variable,
        collection: Box::new(collection),
        body,
        raw_tag: full_raw_start_tag,
    })
}

/// Parses an iterator reference within a loop macro.
fn parse_iterator_reference_content<'i, P: PluginBridge + Debug>(
    pair: Pair<'i, Rule>,
) -> Result<AstNode, ParserError> {
    trace!("Parsing iterator reference content: {:?}", pair.as_str());
    if pair.as_rule() != Rule::iterator_reference {
        error!("Expected iterator_reference, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected iterator_reference, got {:?}", pair.as_rule())));
    }
    let raw_tag_string = pair.as_str().to_string();

    // Extract the identifier between {{ and }}
    let name = pair.into_inner() // Go inside iterator_reference
        .next() // Should be the identifier rule
        .ok_or_else(|| ParserError::Processing(format!("Iterator reference missing identifier: {}", raw_tag_string)))?
        .as_str()
        .to_string();

    if name.is_empty() {
        error!("Empty identifier in iterator reference: {}", raw_tag_string);
        return Err(ParserError::Processing(format!("Empty identifier in iterator reference: {}", raw_tag_string)));
    }

    trace!("Parsed iterator reference: name='{}'", name);
    Ok(AstNode::IteratorReference { name, raw_tag: raw_tag_string })
}

/// Parses an *atomic* variable pair.
fn parse_atomic_variable(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing atomic variable: {:?}", pair.as_str());
    if pair.as_rule() != Rule::variable {
        error!("Expected atomic variable rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected atomic variable rule, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    let content = raw_tag.trim_start_matches("{{").trim_end_matches("}}");
    let parts: Vec<&str> = content.splitn(2, ':').collect();
    if parts.len() == 2 {
        let scope = parts[0].trim().to_string();
        let name = parts[1].trim().to_string();
        if scope.is_empty() || name.is_empty() {
            error!("Invalid atomic variable format (empty scope/name): {}", raw_tag);
            Err(ParserError::Processing(format!("Invalid atomic variable format (empty scope/name): {}", raw_tag)))
        } else {
            trace!("Parsed atomic variable: scope='{}', name='{}'", scope, name);
            Ok(AstNode::Variable { scope, name, _raw_tag: raw_tag })
        }
    } else {
        error!("Invalid atomic variable format (missing ':'): {}", raw_tag);
        Err(ParserError::Processing(format!("Invalid atomic variable format (missing ':'): {}", raw_tag)))
    }
}

fn parse_atomic_iterator_reference(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing atomic iterator reference: {:?}", pair.as_str());
    if pair.as_rule() != Rule::iterator_reference {
        error!("Expected atomic iterator_reference rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected atomic iterator_reference rule, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    let content = raw_tag.trim_start_matches("{{").trim_end_matches("}}");
    Ok(AstNode::IteratorReference { name: content.to_string(), raw_tag })
}

/// Parses an *atomic* processor tag pair.
fn parse_atomic_processor(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing atomic processor: {:?}", pair.as_str());
    if pair.as_rule() != Rule::processor {
        error!("Expected atomic processor_tag rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected atomic processor_tag rule, got {:?}", pair.as_rule())));
    }
    let raw_tag = pair.as_str().to_string();
    let content = raw_tag.trim_start_matches("@![")
                            .trim_end_matches(']');

    let (name_str, props_str_opt) = match content.find('(') {
        Some(paren_idx) => {
            if content.ends_with(')') {
                (&content[..paren_idx], Some(&content[paren_idx+1..content.len()-1]))
            } else {
                error!("Malformed atomic processor tag (missing ')'): {}", raw_tag);
                return Err(ParserError::Processing(format!("Malformed atomic processor tag (missing ')'): {}", raw_tag)));
            }
        },
        None => (content, None),
    };

    let name = name_str.trim().to_string();
    if name.is_empty() {
        error!("Malformed atomic processor tag (empty name): {}", raw_tag);
        return Err(ParserError::Processing(format!("Malformed atomic processor tag (empty name): {}", raw_tag)));
    }
    trace!("Atomic processor name: {}", name);

    let properties = match props_str_opt {
        Some(props_str) if !props_str.trim().is_empty() => {
            trace!("Re-parsing atomic processor properties: {}", props_str);
            let prop_pairs = WorldInfoParser::parse(Rule::properties, props_str)
                .map_err(|e| {
                    error!("Pest parse failed for atomic processor properties '{}': {}", props_str, e);
                    ParserError::PestParse(e.with_path(&format!("atomic processor properties: {}", props_str)))
                })?;
            if let Some(props_pair) = prop_pairs.peek() {
                if props_pair.as_rule() == Rule::properties {
                    parse_properties(props_pair)?
                } else {
                    error!("Expected properties rule from inner parse of atomic props, got {:?}", props_pair.as_rule());
                    return Err(ParserError::Internal(format!("Expected properties rule from inner parse, got {:?}", props_pair.as_rule())));
                }
            } else {
                trace!("Inner parse of atomic props yielded no pairs.");
                Vec::new()
            }
        }
        _ => {
            trace!("No properties string for atomic processor {}", name);
            Vec::new()
        },
    };
    trace!("Atomic processor properties parsed (count: {})", properties.len());

    Ok(AstNode::Processor { name, properties, raw_tag })
}


/// Parses a pest pair representing a literal value into an AstNode::NestedValue.
fn parse_literal(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing literal rule {:?}: {:?}", pair.as_rule(), pair.as_str());
    match pair.as_rule() {
        Rule::quoted_string => {
            let inner_content = pair.clone().into_inner()
                .find(|p| p.as_rule() == Rule::string_content)
                .map(|p| p.as_str())
                .unwrap_or("");
            let unescaped = unescape_string(inner_content)?;
            trace!("Parsed quoted string literal: \"{}\"", unescaped);
            Ok(AstNode::NestedValue(Value::String(unescaped)))
        }
        Rule::string => {
            let quoted_string_pair = pair.clone().into_inner().next()
                .ok_or_else(|| ParserError::Processing(format!("Empty string literal pair: {:?}", pair.as_str())))?;
            trace!("Descending into quoted_string from string rule");
            parse_literal(quoted_string_pair) // Recurse
        }
        Rule::number => {
            let num_str = pair.as_str();
            let num = num_str.parse::<serde_json::Number>().map_err(|e| {
                error!("Failed to parse number '{}': {}", num_str, e);
                ParserError::Processing(format!("Failed to parse number '{}': {}", num_str, e))
            })?;
            trace!("Parsed number literal: {}", num);
            Ok(AstNode::NestedValue(Value::Number(num)))
        }
        Rule::boolean => {
            let val = pair.as_str() == "true";
            trace!("Parsed boolean literal: {}", val);
            Ok(AstNode::NestedValue(json!(val)))
        }
        Rule::null => {
            trace!("Parsed null literal");
            Ok(AstNode::NestedValue(Value::Null))
        }
        Rule::literal => {
            let inner_pair = pair.into_inner().next().ok_or_else(|| ParserError::Processing("Empty literal rule".to_string()))?;
            trace!("Descending into actual literal from literal rule");
            parse_literal(inner_pair) // Recurse
        }
        r => {
            error!("Unexpected rule type for literal: {:?} ({:?})", r, pair.as_str());
            Err(ParserError::Processing(format!("Unexpected rule type for literal: {:?} ({:?})", r, pair.as_str())))
        }
    }
}

/// Helper function to unescape string content.
fn unescape_string(s: &str) -> Result<String, ParserError> {
    // No logging added here as it could be very verbose if called frequently
    let mut unescaped = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
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
                        hex_code.push(chars.next().ok_or_else(|| {
                            error!("Incomplete unicode escape sequence in string: {}", s);
                            ParserError::Processing("Incomplete unicode escape sequence: missing hex digits".to_string())
                        })?);
                    }
                    if hex_code.len() != 4 || !hex_code.chars().all(|c| c.is_ascii_hexdigit()) {
                        error!("Invalid unicode escape sequence (non-hex) in string: \\u{}", hex_code);
                        return Err(ParserError::Processing(format!("Invalid unicode escape sequence: non-hex characters in \\u{}", hex_code)));
                    }
                    let code_point = u32::from_str_radix(&hex_code, 16).map_err(|_| {
                        error!("Invalid unicode escape sequence (parse hex failed) in string: \\u{}", hex_code);
                        ParserError::Processing(format!("Invalid unicode escape sequence: failed to parse hex \\u{}", hex_code))
                    })?;
                    unescaped.push(std::char::from_u32(code_point).ok_or_else(|| {
                        error!("Invalid unicode code point from escape: {}", code_point);
                        ParserError::Processing(format!("Invalid unicode code point: {}", code_point))
                    })?);
                }
                Some(other) => {
                    // Treat unrecognized escapes as literal backslash + char
                    warn!("Unrecognized escape sequence '\\{}' in string, treating literally", other);
                    unescaped.push('\\');
                    unescaped.push(other);
                }
                None => {
                    error!("Dangling escape character at end of string: {}", s);
                    return Err(ParserError::Processing("Dangling escape character at end of string".to_string()));
                }
            }
        } else {
            unescaped.push(c);
        }
    }
    Ok(unescaped)
}


/// Parses pseudo-JSON properties into key-value AstNode pairs.
fn parse_properties(pair: Pair<Rule>) -> Result<Vec<(String, AstNode)>, ParserError> {
    trace!("Parsing properties rule: {:?}", pair.as_str());
    if pair.as_rule() != Rule::properties {
        error!("Expected properties rule, got {:?}", pair.as_rule());
        return Err(ParserError::Processing(format!("Expected properties rule, got {:?}", pair.as_rule())));
    }
    let mut props = Vec::new();
    for prop_pair in pair.clone().into_inner() {
        if prop_pair.as_rule() == Rule::property {
            trace!("Parsing property pair: {:?}", prop_pair.as_str());
            let mut inner = prop_pair.clone().into_inner();

            let key_pair_outer = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing property_key pair in property rule: {:?}", prop_pair.as_str())))?;

            if key_pair_outer.as_rule() != Rule::property_key {
                error!("Expected property_key rule, got {:?} in {}", key_pair_outer.as_rule(), prop_pair.as_str());
                return Err(ParserError::Processing(format!("Expected property_key rule, got {:?} in {}", key_pair_outer.as_rule(), prop_pair.as_str())));
            }

            let key_str = key_pair_outer.as_str();
            let key = if key_str.starts_with('"') && key_str.ends_with('"') && key_str.len() >= 2 {
                let inner_content = &key_str[1..key_str.len()-1];
                unescape_string(inner_content)?
            } else {
                key_str.to_string()
            };
            trace!("Parsed property key: \"{}\"", key);

            let actual_value_pair = inner.next().ok_or_else(|| ParserError::Processing(format!("Missing value pair after key '{}' in property rule: {:?}", key, prop_pair.as_str())))?;
            trace!("Parsing property value for key '{}', rule: {:?}", key, actual_value_pair.as_rule());

            let value_node = parse_property_value(actual_value_pair)?;
            props.push((key, value_node));
        } else if !matches!(prop_pair.as_rule(), Rule::WHITESPACE) {
            // Might indicate grammar issue if non-whitespace, non-property appears
            warn!("Unexpected rule {:?} inside properties rule: {:?}", prop_pair.as_rule(), prop_pair.as_str());
        }
    }
    trace!("Finished parsing properties ({} found)", props.len());
    Ok(props)
}


/// Parses a property value into an AstNode.
fn parse_property_value(pair: Pair<Rule>) -> Result<AstNode, ParserError> {
    trace!("Parsing property value rule {:?}: {:?}", pair.as_rule(), pair.as_str());
    match pair.as_rule() {
        Rule::processor => parse_atomic_processor(pair),
        Rule::variable => parse_atomic_variable(pair),
        Rule::object => {
            trace!("Parsing object property value");
            match pair.clone().into_inner().find(|p| p.as_rule() == Rule::properties) {
                Some(props_pair) => {
                    let props = parse_properties(props_pair)?;
                    trace!("Parsed object with {} properties", props.len());
                    Ok(AstNode::NestedObject(props))
                }
                None => {
                    trace!("Parsed empty object {{}}");
                    Ok(AstNode::NestedObject(Vec::new()))
                },
            }
        }
        Rule::array => {
            trace!("Parsing array property value");
            let items = pair.clone().into_inner()
                .filter(|p| !matches!(p.as_rule(), Rule::WHITESPACE)) // Filter only actual value rules
                .map(parse_property_value) // Recursively parse each item
                .collect::<Result<Vec<_>, _>>()?;
            trace!("Parsed array with {} items", items.len());
            Ok(AstNode::NestedArray(items))
        }
        // Delegate literal types to parse_literal
        Rule::number | Rule::boolean | Rule::null | Rule::string | Rule::quoted_string => {
            parse_literal(pair)
        }
        Rule::string_content => {
            // This case might be less common if grammar ensures string_content is wrapped
            warn!("Directly parsing string_content as property value: {:?}", pair.as_str());
            Ok(AstNode::NestedValue(Value::String(pair.as_str().to_string())))
        }
        Rule::literal => {
            let literal_pair = pair.clone().into_inner().next()
                .ok_or_else(|| ParserError::Processing(format!("Empty literal pair: {:?}", pair.as_str())))?;
            trace!("Descending into actual literal from property value literal rule");
            parse_literal(literal_pair)
        }
        Rule::iterator_reference => {
            trace!("Descending into actual iterator_reference from property value iterator_reference rule");
            parse_atomic_iterator_reference(pair)
        }
        r => {
            error!("Unexpected rule type encountered during property value parsing: {:?} ({:?})", r, pair.as_str());
            Err(ParserError::InvalidRule(r))
        }
    }
}


// --- AST Resolution ---

/// Resolves a list of AST nodes into final WorldInfoNode objects.
fn resolve_ast_nodes<P: PluginBridge + Debug>(
    nodes: &[AstNode],
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    trace!("Entering resolve_ast_nodes ({} nodes)", nodes.len());
    let mut resolved_nodes = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        trace!("Resolving AST node {}/{}: {:?}", i + 1, nodes.len(), node.variant_name()); // Use a helper trait/method if needed
        match resolve_single_node(node, registry, entry_id, loop_context) {
            Ok(resolved_node_list) => {
                trace!(" -> Resolved into {} WorldInfoNode(s)", resolved_node_list.len());
                resolved_nodes.extend(resolved_node_list)
            },
            Err(e) => {
                // Error already logged in resolve_single_node or deeper
                error!("Resolution failed for AST node {:?}: {}", node.variant_name(), e);
                return Err(e); // Propagate the error immediately
            }
        }
    }
    trace!("Exiting resolve_ast_nodes ({} resolved nodes)", resolved_nodes.len());
    Ok(resolved_nodes)
}

// Helper to get a name for the AST node variant for logging
impl AstNode {
    fn variant_name(&self) -> &'static str {
        match self {
            AstNode::Text(_) => "Text",
            AstNode::Processor { .. } => "Processor",
            AstNode::ModFunction { .. } => "ModFunction",
            AstNode::Trigger { .. } => "Trigger",
            AstNode::Variable { .. } => "Variable",
            AstNode::MacroIf { .. } => "MacroIf",
            AstNode::MacroForeach { .. } => "MacroForeach",
            AstNode::IteratorReference { .. } => "IteratorReference",
            AstNode::NestedValue(_) => "NestedValue",
            AstNode::NestedArray(_) => "NestedArray",
            AstNode::NestedObject(_) => "NestedObject",
        }
    }
}


/// Resolves a single AST node.
fn resolve_single_node<P: PluginBridge + Debug>(
    node: &AstNode,
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>
) -> Result<Vec<Box<dyn WorldInfoNode>>, ParserError> {
    trace!("Entering resolve_single_node for {:?}", node.variant_name());
    match node {
        AstNode::Text(content) => {
            trace!("Resolving Text node");
            Ok(vec![ Box::new(TextNode { content: content.clone() }) as Box<dyn WorldInfoNode> ])
        },
        AstNode::Trigger { id, .. } => {
            registry.push_activation(id)?;
            Ok(vec![ Box::new(EmptyNode {}) as Box<dyn WorldInfoNode> ])
        }
        AstNode::Processor { name, properties, raw_tag } => {
            trace!("Resolving Processor node '{}'", name);
            let resolved_props = resolve_properties_to_json(properties, registry, entry_id, loop_context)?;
            debug!("Resolved properties for processor '{}': {:?}", name, resolved_props);
            match registry.instantiate_processor(name, &resolved_props) {
                Some(processor) => {
                    trace!("Successfully instantiated processor '{}'", name);
                    Ok(vec![processor as Box<dyn WorldInfoNode>])
                },
                None => {
                    error!("Processor instantiation failed for '{}' with props {:?}. Raw tag: {}", name, resolved_props, raw_tag);
                    Err(ParserError::ProcessorInstantiation( name.clone(),
                        format!("Processor not found or instantiation failed (props: {:?})", resolved_props),
                    ))
                }
            }
        }
        AstNode::ModFunction { name, parameters, raw_tag } => {
            warn!("Unimplemented. Temporarily returns an empty node");
            trace!("Resolving ModFunction node '{}'", name);
            let params = resolve_parameters(parameters, registry, entry_id, loop_context)?;
            trace!("Resolved parameters for mod function '{}': {:?}. Raw tag: {}", name, params, raw_tag);

            let return_value = registry.call_function(name, params)?;

            trace!("Successfully called mod function '{}'. Return value: {:?}", name, return_value);

            match return_value {
                Value::Null => Ok(vec![ Box::new(EmptyNode {}) as Box<dyn WorldInfoNode> ]),
                v => Ok(vec![ Box::new(TextNode { content: v.to_string() }) as Box<dyn WorldInfoNode> ]) // Literal return value
            }
        }
        AstNode::Variable { scope, name, .. } => {
            let full_name = format!("{}:{}", scope, name);
            trace!("Resolving Variable node '{}'", full_name);
            match registry.get_variable(&full_name, loop_context) {
                Ok(var) => {
                    trace!("Successfully retrieved variable '{}'", full_name);
                    Ok(vec![ Box::new(VariableNode::new(var)) as Box<dyn WorldInfoNode> ])
                },
                Err(e) => Err(e),
            }
        }
        AstNode::MacroIf { raw_tag, condition, then_branch, else_branch } => {
            debug!("Resolving MacroIf: {}", raw_tag);
            match evaluate_expression(condition, registry, entry_id, loop_context) {
                Ok(condition_result) => {
                    debug!("MacroIf condition evaluated to: {:?}", condition_result);
                    if is_truthy(&condition_result) {
                        debug!("Executing 'then' branch for MacroIf: {}", raw_tag);
                        resolve_ast_nodes(then_branch, registry, entry_id, loop_context) // Recurse
                    } else if let Some(else_nodes) = else_branch {
                        debug!("Executing 'else' branch for MacroIf: {}", raw_tag);
                        resolve_ast_nodes(else_nodes, registry, entry_id, loop_context) // Recurse
                    } else {
                        debug!("Condition false, no 'else' branch for MacroIf: {}", raw_tag);
                        Ok(Vec::new()) // No nodes if condition false and no else
                    }
                }
                Err(e) => {
                    error!("Error evaluating if condition for {}: {}", raw_tag, e);
                    Err(e) // Propagate evaluation error
                }
            }
        }
        AstNode::MacroForeach { raw_tag, item_variable, collection, body } => {
            debug!("Resolving MacroForeach: {}", raw_tag);
            trace!("Item variable: '{}', Body nodes: {}", item_variable, body.len());

            // 1. Evaluate the collection expression, passing the current loop context
            let collection_value = match evaluate_expression(collection, registry, entry_id, loop_context) {
                Ok(val) => val,
                Err(e) => {
                    error!("Error evaluating collection expression for foreach {}: {}", raw_tag, e);
                    return Err(e);
                }
            };
            debug!("Foreach collection evaluated to: {:?}", collection_value.variant_name());
            trace!("Foreach collection value: {:?}", collection_value);

            // 2. Check if the result is iterable (currently only Array supported)
            if let Value::Array(items) = collection_value {
                let mut all_resolved_nodes = Vec::new();
                let item_count = items.len();
                debug!("Iterating over collection array with {} items for foreach {}", item_count, raw_tag);

                // 3. Iterate and resolve body for each item
                for (index, current_item) in items.into_iter().enumerate() {
                    trace!("Foreach loop iteration {}/{}, item_variable='{}', item={:?}", index + 1, item_count, item_variable, current_item);

                    // Create a *new* context for this iteration.
                    // It could potentially inherit from the outer loop_context if needed,
                    // but for simple foreach, just the current item is usually sufficient.
                    let mut current_iteration_context = HashMap::new();
                    current_iteration_context.insert(item_variable.clone(), current_item);

                    // 4. Resolve the body nodes with the new context for this iteration
                    match resolve_ast_nodes(body, registry, entry_id, Some(&current_iteration_context)) {
                        Ok(resolved_body_nodes) => {
                            trace!(" -> Resolved body for iteration {} into {} nodes", index + 1, resolved_body_nodes.len());
                            all_resolved_nodes.extend(resolved_body_nodes);
                        }
                        Err(e) => {
                            error!("Error resolving body in foreach {} iteration {}: {}", raw_tag, index + 1, e);
                            // Decide whether to stop or continue on error. Stopping is safer.
                            return Err(e);
                        }
                    }
                }
                debug!("Finished foreach {} loop, produced {} total nodes", raw_tag, all_resolved_nodes.len());
                Ok(all_resolved_nodes)

            }
            // TODO: Add support for iterating over Objects if needed
            // else if let Value::Object(obj) = collection_value { ... }
            else {
                error!("Collection expression for foreach {} did not evaluate to an Array (found {:?}). Cannot iterate.", raw_tag, collection_value.variant_name());
                Err(ParserError::Evaluation(format!(
                    "Cannot iterate over collection in foreach {}: Expected Array, got {:?}",
                    raw_tag, collection_value.variant_name()
                )))
            }
        }
        AstNode::IteratorReference { name, raw_tag } => {
            debug!("Resolving iterator reference: {}", raw_tag);
            match loop_context {
                Some(context) => {
                    match context.get(name) {
                        Some(value) => {
                            trace!("Found iterator variable '{}' in loop context", name);
                            // Treat it like a resolved variable
                            Ok(vec![Box::new(VariableNode::new(value.clone())) as Box<dyn WorldInfoNode>])
                        }
                        None => {
                            error!("Iterator variable '{}' not found in loop context for {}", name, raw_tag);
                            Err(ParserError::UndefinedVariable(name.clone())) // Use UndefinedVariable error
                        }
                    }
                }
                None => {
                    error!("Iterator reference {} used outside of a foreach loop context", raw_tag);
                    Err(ParserError::Processing(format!(
                        "Iterator reference {} used outside of a foreach loop",
                        raw_tag
                    )))
                }
            }
        }
        // These should not be present at the top level during final resolution
        AstNode::NestedValue(_) | AstNode::NestedArray(_) | AstNode::NestedObject(_) => {
            error!("Unexpected nested AST node type during final resolution: {:?}", node.variant_name());
            Err(ParserError::Processing(format!("Unexpected nested AST node type during final resolution: {:?}", node.variant_name())))
        }
    }
}

/// Recursively resolves AST nodes within properties into a serde_json::Value object.
fn resolve_properties_to_json<P: PluginBridge + Debug>(
    properties: &[(String, AstNode)],
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Value, ParserError> {
    trace!("Entering resolve_properties_to_json ({} properties)", properties.len());
    let mut map = Map::new();
    for (key, value_node) in properties {
        trace!("Resolving property key '{}', value type {:?}", key, value_node.variant_name());
        let resolved_value = resolve_property_value_to_json(value_node, registry, entry_id, loop_context)?;
        trace!(" -> Resolved value for key '{}': {:?}", key, resolved_value);
        map.insert(key.clone(), resolved_value);
    }
    trace!("Exiting resolve_properties_to_json");
    Ok(Value::Object(map))
}

/// Resolves a list of parameter AST nodes into a Vec<serde_json::Value>.
fn resolve_parameters<P: PluginBridge + Debug>(
    parameters: &[AstNode],
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Vec<Value>, ParserError> {
    trace!("Entering resolve_parameters ({} parameters)", parameters.len());
    let mut resolved_parameters = Vec::new();
    for param_node in parameters {
        trace!("Resolving parameter type {:?}", param_node.variant_name());
        let resolved_value = resolve_property_value_to_json(param_node, registry, entry_id, loop_context)?;
        trace!(" -> Resolved value: {:?}", resolved_value);
        resolved_parameters.push(resolved_value);
    }
    trace!("Exiting resolve_parameters");
    Ok(resolved_parameters)
}

/// Resolves a single property value AST node into a serde_json::Value.
fn resolve_property_value_to_json<P: PluginBridge + Debug>(
    node: &AstNode,
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Value, ParserError> {
    trace!("Entering resolve_property_value_to_json for {:?}", node.variant_name());
    match node {
        // These nodes resolve by executing them and returning their string content
        AstNode::Processor { .. } | AstNode::MacroIf { .. } | AstNode::MacroForeach { .. } | AstNode::Text { .. } | AstNode::Trigger { .. } | AstNode::ModFunction { .. } => {
            trace!("Resolving node {:?} within property to string content", node.variant_name());
            let resolved_nodes = resolve_single_node(node, registry, entry_id, loop_context)?;
            let mut combined_content = String::new();
            for res_node in resolved_nodes {
                match res_node.content() { // Assuming WorldInfoNode has a content() method
                    Ok(content) => combined_content.push_str(&content),
                    Err(e) => {
                        error!("Failed to get content from resolved node {:?} within property: {}", res_node.name(), e);
                        return Err(ParserError::ProcessorExecution(format!("Failed to get content from resolved node within property: {}", e)));
                    }
                }
            }
            trace!(" -> Resolved to string: {:?}", combined_content);
            Ok(Value::String(combined_content))
        }
        // Variables resolve to their value in the registry
        AstNode::Variable { scope, name, .. } => {
            let full_name = format!("{}:{}", scope, name);
            trace!("Resolving variable '{}' within property", full_name);
            registry.get_variable(&full_name, loop_context)
        }
        AstNode::IteratorReference { name, raw_tag } => {
            debug!("Resolving iterator reference {} within property", raw_tag);
            match loop_context {
                Some(context) => {
                    match context.get(name) {
                        Some(value) => {
                            trace!("Found iterator variable '{}' in loop context", name);
                            Ok(value.clone()) // Return the JSON value directly
                        }
                        None => {
                            error!("Iterator variable '{}' not found in loop context for {}", name, raw_tag);
                            Err(ParserError::UndefinedVariable(name.clone()))
                        }
                    }
                }
                None => {
                    error!("Iterator reference {} used outside of a foreach loop context within property", raw_tag);
                    Err(ParserError::Processing(format!(
                        "Iterator reference {} used outside of a foreach loop within property",
                        raw_tag
                    )))
                }
            }
        }
        // Nested values might need re-parsing if they are strings containing tags
        AstNode::NestedValue(v) => {
            trace!("Resolving NestedValue within property: {:?}", v);
            if let Value::String(s) = v {
                // Check if the string itself contains tags that need evaluation
                if s.contains("@[") || s.contains("@![") || s.contains("<trigger") || s.contains("{{") || s.contains("{#") {
                    debug!("String literal contains tags, re-parsing/evaluating: {:?}", s);
                    // Re-parse the string content as if it were top-level input
                    let inner_resolved_nodes = parse_and_evaluate(s, registry, entry_id, loop_context)?;
                    let mut combined_content = String::new();
                    for res_node in inner_resolved_nodes {
                        match res_node.content() {
                            Ok(content) => combined_content.push_str(&content),
                            Err(e) => {
                                error!("Failed to get content from re-parsed string node: {}", e);
                                return Err(ParserError::ProcessorExecution(format!("Failed to get content from re-parsed string node: {}", e)));
                            }
                        }
                    }
                    trace!(" -> Re-parsed string resolved to: {:?}", combined_content);
                    Ok(Value::String(combined_content))
                } else {
                    // String literal has no tags, use it directly
                    trace!(" -> Using string literal directly");
                    Ok(v.clone())
                }
            } else {
                // Not a string, just clone the literal value (Number, Bool, Null)
                trace!(" -> Using non-string literal directly");
                Ok(v.clone())
            }
        }
        // Arrays resolve by resolving each item
        AstNode::NestedArray(items) => {
            trace!("Resolving NestedArray within property ({} items)", items.len());
            let resolved_items = items.iter()
                .map(|item_node| resolve_property_value_to_json(item_node, registry, entry_id, loop_context)) // Recurse
                .collect::<Result<Vec<_>, _>>()?;
            trace!(" -> Resolved array: {:?}", resolved_items);
            Ok(Value::Array(resolved_items))
        }
        // Objects resolve by resolving their properties (recursive call)
        AstNode::NestedObject(props) => {
            trace!("Resolving NestedObject within property ({} props)", props.len());
            resolve_properties_to_json(props, registry, entry_id, loop_context) // Recurse
        }
    }
}


// --- Evaluation Logic ---

// Helper trait for logging Value types
trait VariantName { fn variant_name(&self) -> &'static str; }
impl VariantName for Value {
    fn variant_name(&self) -> &'static str {
        match self {
            Value::Null => "Null", Value::Bool(_) => "Bool", Value::Number(_) => "Number",
            Value::String(_) => "String", Value::Array(_) => "Array", Value::Object(_) => "Object",
        }
    }
}

/// Evaluates an Expression AST node to a serde_json::Value.
fn evaluate_expression<P: PluginBridge + Debug>(
    expr: &Expression,
    registry: &mut ScopedRegistry<P>,
    entry_id: &String,
    loop_context: Option<&HashMap<String, Value>>,
) -> Result<Value, ParserError> {
    // Use debug! for expression evaluation steps
    debug!("EVAL EXPR: {:?}", expr);
    match expr {
        Expression::Literal(value) => {
            debug!("  -> Literal: {:?}", value);
            Ok(value.clone())
        },
        Expression::Variable { scope, name, .. } => {
            let full_name = format!("{}:{}", scope, name);
            debug!("  -> Variable Lookup: {}", full_name);
            let result = registry.get_variable(&full_name, loop_context)?;
            debug!("  -> Variable Result: {:?}", result);
            Ok(result)
        }
        Expression::Processor { name, properties, raw_tag } => {
            debug!("  -> Processor Eval Start: {}", raw_tag);
            let resolved_props = resolve_properties_to_json(properties, registry, entry_id, loop_context)?;
            debug!("  -> Processor Resolved Props: {:?}", resolved_props);

            let processor_instance = registry.instantiate_processor(name, &resolved_props)
            .ok_or_else(|| {
                error!("Processor instantiation failed during expression evaluation for '{}' with props {:?}. Raw tag: {}", name, resolved_props, raw_tag);
                ParserError::ProcessorInstantiation( name.clone(),
                    format!("Processor '{}' not found or instantiation failed during expression evaluation (props: {:?})", name, resolved_props),
                )
            })?;
            debug!("  -> Processor Instantiated: {}", name);

            let content_result = processor_instance.content();
            debug!("  -> Processor Content Result: {:?}", content_result);

            match content_result {
                Ok(content_str) => {
                    // Try parsing as number first
                    if let Ok(num_f64) = content_str.parse::<f64>() {
                        if let Some(num) = serde_json::Number::from_f64(num_f64) {
                            debug!("  -> Processor Result as Number: {:?}", num);
                            Ok(Value::Number(num))
                        } else {
                            warn!("Processor '{}' result '{}' is valid f64 but not representable as JSON number (NaN/Infinity?), using String.", name, content_str);
                            Ok(Value::String(content_str))
                        }
                    } else {
                        debug!("  -> Processor Result as String: {:?}", content_str);
                        Ok(Value::String(content_str))
                    }
                }
                Err(e) => {
                    error!("Error executing processor '{}' in expression: {}", name, e);
                    Err(ParserError::ProcessorExecution(format!("Error executing processor '{}' in expression: {}", name, e)))
                }
            }
        }
        Expression::UnaryOperation { operator, operand } => {
            debug!("  -> Unary Op: {:?}", operator);
            let operand_value = evaluate_expression(operand, registry, entry_id, loop_context)?; // Recurse
            debug!("  -> Unary Operand Value: {:?}", operand_value);
            let result = match operator {
                UnaryOperator::Not => Ok(Value::Bool(!is_truthy(&operand_value))),
            };
            debug!("  -> Unary Result: {:?}", result);
            result
        }
        Expression::BinaryOperation { left, operator, right } => {
            debug!("  -> Binary Op: {:?}", operator);
            let left_value = evaluate_expression(left, registry, entry_id, loop_context)?; // Recurse left
            debug!("  -> Binary Left Value: {:?}", left_value);

            // Short-circuit evaluation for || and &&
            match operator {
                BinaryOperator::Or => {
                    let is_left_truthy = is_truthy(&left_value);
                    debug!("  -> OR Left Truthy: {}", is_left_truthy);
                    if is_left_truthy { return Ok(left_value); } // Return the left value if truthy
                    debug!("  -> OR Evaluating Right");
                    let right_value = evaluate_expression(right, registry, entry_id, loop_context)?; // Recurse right only if needed
                    debug!("  -> OR Right Value: {:?}", right_value);
                    return Ok(right_value); // Return the right value
                }
                BinaryOperator::And => {
                    let is_left_truthy = is_truthy(&left_value);
                     debug!("  -> AND Left Truthy: {}", is_left_truthy);
                    if !is_left_truthy { return Ok(left_value); } // Return the left value if falsy
                     debug!("  -> AND Evaluating Right");
                    let right_value = evaluate_expression(right, registry, entry_id, loop_context)?; // Recurse right only if needed
                    debug!("  -> AND Right Value: {:?}", right_value);
                    return Ok(right_value); // Return the right value
                }
                _ => {} // Continue for comparison and arithmetic
            }

            // Evaluate right operand for non-short-circuiting ops
            debug!("  -> Binary Op Evaluating Right");
            let right_value = evaluate_expression(right, registry, entry_id, loop_context)?; // Recurse right
            debug!("  -> Binary Right Value: {:?}", right_value);
            // Delegate actual operation
            evaluate_binary_operation(&left_value, *operator, &right_value)
        }
    }
}

/// Determines the truthiness of a serde_json::Value.
fn is_truthy(value: &Value) -> bool {
    let result = match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Number(n) => n.as_f64().map_or(false, |f| f != 0.0), // Consider 0 as false
        Value::String(s) => !s.is_empty(),
        Value::Array(a) => !a.is_empty(),
        Value::Object(o) => !o.is_empty(),
    };
    trace!("is_truthy({:?}) -> {}", value, result);
    result
}

/// Performs binary operations between two serde_json::Values.
fn evaluate_binary_operation(left: &Value, op: BinaryOperator, right: &Value) -> Result<Value, ParserError> {
    debug!("  -> Eval Bin Op {:?}: Left: {:?}, Right: {:?}", op, left, right);
    let result = match op {
        // --- Comparisons ---
        BinaryOperator::Eq | BinaryOperator::Neq => {
            // Use PartialEq for direct comparison, handles types correctly
            let are_equal = left == right;
            Ok(Value::Bool(if op == BinaryOperator::Eq { are_equal } else { !are_equal }))
        }
        BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Lte | BinaryOperator::Gte => {
            match (left, right) {
                // Number comparison
                (Value::Number(l), Value::Number(r)) => {
                    match (l.as_f64(), r.as_f64()) {
                        (Some(l_f64), Some(r_f64)) => {
                            Ok(Value::Bool(match op {
                                BinaryOperator::Lt => l_f64 < r_f64, BinaryOperator::Gt => l_f64 > r_f64,
                                BinaryOperator::Lte => l_f64 <= r_f64, BinaryOperator::Gte => l_f64 >= r_f64,
                                _ => unreachable!(),
                            }))
                        }
                        _ => {
                            // Handle potential non-f64 representable numbers if necessary,
                            // or error if comparison isn't possible.
                            error!("Cannot compare non-f64 numbers: {:?}, {:?}", l, r);
                            Err(ParserError::Evaluation(format!("Cannot compare non-f64 numbers: {:?}, {:?}", l, r)))
                        }
                    }
                }
                // String comparison
                (Value::String(l_str), Value::String(r_str)) => {
                    Ok(Value::Bool(match op {
                        BinaryOperator::Lt => l_str < r_str, BinaryOperator::Gt => l_str > r_str,
                        BinaryOperator::Lte => l_str <= r_str, BinaryOperator::Gte => l_str >= r_str,
                        _ => unreachable!(),
                    }))
                }
                // Type mismatch error
                _ => {
                    error!("Cannot apply ordered comparison ({:?}) to types {:?} and {:?}", op, left.variant_name(), right.variant_name());
                    Err(ParserError::Evaluation(format!("Cannot apply ordered comparison ({:?}) to types {:?} and {:?}", op, left.variant_name(), right.variant_name())))
                }
            }
        }

        // --- Arithmetic ---
        BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div => {
            match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    // Perform arithmetic using f64
                    let l_f64 = l.as_f64().ok_or_else(|| ParserError::Evaluation(format!("Left operand is not a valid f64 for arithmetic: {:?}", l)))?;
                    let r_f64 = r.as_f64().ok_or_else(|| ParserError::Evaluation(format!("Right operand is not a valid f64 for arithmetic: {:?}", r)))?;
                    trace!("  -> Arithmetic f64: Left: {}, Right: {}", l_f64, r_f64);

                    let result_f64 = match op {
                        BinaryOperator::Add => l_f64 + r_f64,
                        BinaryOperator::Sub => l_f64 - r_f64,
                        BinaryOperator::Mul => l_f64 * r_f64,
                        BinaryOperator::Div => {
                            if r_f64 == 0.0 {
                                error!("Division by zero attempted: {} / {}", l_f64, r_f64);
                                return Err(ParserError::Evaluation("Division by zero".to_string()));
                            }
                            l_f64 / r_f64
                        }
                        _ => unreachable!(),
                    };
                    trace!("  -> Arithmetic f64 Result: {}", result_f64);

                    // Convert back to JSON Number, handling potential NaN/Infinity
                    serde_json::Number::from_f64(result_f64)
                        .map(Value::Number)
                        .ok_or_else(|| {
                            error!("Arithmetic result is not representable as JSON number: {}", result_f64);
                            ParserError::Evaluation(format!("Arithmetic result is not representable as JSON number: {}", result_f64))
                        })
                }
                 // Type mismatch error
                _ => {
                    error!("Cannot apply arithmetic operation ({:?}) to types {:?} and {:?}", op, left.variant_name(), right.variant_name());
                    Err(ParserError::Evaluation(format!("Cannot apply arithmetic operation ({:?}) to types {:?} and {:?}", op, left.variant_name(), right.variant_name())))
                }
            }
        }
        // And/Or handled by short-circuiting earlier
        BinaryOperator::And | BinaryOperator::Or => unreachable!("Logical operators should be handled by short-circuiting"),
    };
    debug!("  -> Op Result: {:?}", result);
    result
}