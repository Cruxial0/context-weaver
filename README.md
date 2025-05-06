# ContextWeaver

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-stable-orange.svg)]()
[![Documentation](https://img.shields.io/badge/docs-latest-brightgreen.svg)]()

> A dynamic and extensible text evaluation library for procedural content generation

## 📖 Overview

**ContextWeaver** is a powerful Rust library that enables developers to seamlessly integrate a "lorebook" data structure into their applications. It provides a rich macro language for creating dynamic, procedural content that can be evaluated at runtime.

Whether you're building an LLM context system, text-based game, or prompt engine, ContextWeaver gives you the tools to create sophisticated text evaluation with conditional logic, variable substitution, and custom plugins.

## 🚀 Key Features

- **WorldInfo System**: Create collections of entries and documents that activate based on configurable conditions
- **Rich Macro Language**: Build dynamic content with variables, conditionals, loops, and more
- **Plugin Architecture**: Extend functionality with custom processors and functions
- **Powerful Activation Logic**: Trigger content via keywords, regex patterns, or recursive activation

## 🔧 Use Cases

- **LLM Context Management**: Define dynamic context sequences for large language models
- **Text Game Systems**: Build character generators or procedural game elements
- **Prompt Engines**: Create sophisticated, conditional prompt templates
- **Any System Requiring Procedural Text**: Generate dynamic content based on state or conditions

## 🛠️ The Weaver Language

ContextWeaver's expression language combines simplicity with power. Built using [pest.rs](https://pest.rs/), it emphasizes component interchangeability - any component with a return value can be used virtually anywhere in your templates.

### Core Components

#### Variables

```
{{scope:variable}}
```

Variables store and retrieve values within defined scopes. Currently supported scopes:
- `global`: Accessible from all entries
- `local`: Entry-specific variables (syntactic sugar for the entry's ID)

#### Processors

```
@![processor.name(property: value, another: "string")]
```

Inline programs that execute custom logic. Built-in processors include:

| Identifier          | Description                                  |
|---------------------|----------------------------------------------|
| core.weaver.rng     | Generates a random number                    |
| core.weaver.wildcard| Picks one item at random from an input array |

#### Macros

Multi-line operations for control flow:

```
{# if condition #}
  content when condition is true
{# endif #}
```

```
{# foreach item in array #}
  content with {{item}}
{# endforeach #}
```

#### Functions

```
@[namespace.func(param1, param2)]
```

Functions modify the internal state of the WorldInfo, with access to the registry and variable scopes.

#### Triggers

```
<trigger id="entry-id">
```

Deterministically activate another entry when this tag is encountered during evaluation.

#### Documents (Work in Progress)

```
[[DOCUMENT_ID]]
```

Reusable content blocks that can be imported into entries.

## 🔌 Plugin Support

ContextWeaver is designed for extensibility, allowing you to add custom logic through processors and functions.

### Creating Custom Functions

Implement the `ModFunction` trait to create your own functions:

```rust
impl<P : PluginBridge> ModFunction<P> for YourCustomFunction<P> {
    fn call(&self, args: Vec<Value>, registry: &ScopedRegistry<P>) -> Result<Value, ParserError> {
        // Your implementation here
    }

    fn signature(&self) -> (String, Vec<ParamSignature>) {
        // Define your function signature
    }
}
```

### Creating Custom Processors

Implement the `PluginBridge` trait to connect external logic to your processors:

```rust
impl PluginBridge for YourPluginBridge {
    type PluginId = YourIdType;

    fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) 
        -> Result<String, WorldInfoError> {
        // Your plugin invocation logic
    }
}
```

## Installation
### Cargo
```bash
cargo add context-weaver
```

### Clone the repository
```bash
git clone https://github.com/Cruxial0/context-weaver
# Add a workspace reference to your Cargo.toml
```

## 🚧 Roadmap

| Feature                    | Status |
|----------------------------|--------|
| **Processor & Activation** |        |
| Plugin support             | ✅     |
| Various property types     | ✅     |
| Documents                  | 🔄     |
| Keyword/regex activation   | ✅     |
| **Persistence**            |       |
| JSON save/load of state    | 🔄     |
| **Context Insertion**      |       |
| Configurable strategies    | 🔄     |
| **Macro & Templating**     |       |
| If-blocks                  | ✅     |
| Scoped variables           | ✅     |
| Extended conditionals      | ✅     |
| **Syntax & Parsing**       |       |
| Pseudo-JSON via Pest       | ✅     |
| Structured diagnostics     | ✅     |

## 📋 Language Syntax Overview

| Construct      | Syntax                            | Status |
|----------------|-----------------------------------|--------|
| Variables      | `{{scope:variable}}`              | ✅     |
| Processors     | `@![processor.name(props)]`       | ✅     |
| Triggers       | `<trigger id="entry-id">`         | ✅     |
| If-Macro       | `{# if ... #}...{# endif #}`      | ✅     |
| Foreach-Macro  | `{# foreach ... #}...{# endforeach #}` | ✅ |
| Functions      | `@[namespace.func(params)]`       | ✅     |
| Documents      | `[[DOCUMENT_ID]]`                 | 🔄     |

## 🤝 Contributing

Contributions are welcome! Please feel free to:

- Open issues for bugs or feature requests
- Submit pull requests with improvements
- Share feedback on the documentation

Visit the [GitHub repository](https://github.com/Cruxial0/context-weaver) to get involved.

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.