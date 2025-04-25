# ContextWeaver

### Please note that this project is in a very early stage of development - Breaking changes will occur often.

**`ContextWeaver`** is a *(WIP)* powerful system based on WorldInfo/Lorebooks. It's designed to dynamically manage and inject information into language model (LLM) contexts, and potentially other applications requiring dynamic text generation. It achieves this through **entries** with **activation conditions** and inline **processors**.

---

## ✨ Core Concepts

At its heart, `ContextWeaver` operates on two main principles:

1.  **📄 Context Injection:** Entries (pieces of information or "lore") are assigned activation conditions. When these conditions are met during processing, the content of the entry is injected into the target context (e.g., an LLM's prompt or history). *(Activation condition details are planned - see Roadmap)*
2.  **⚙️ Dynamic Content:** Entries are not static. They can contain **processors** – small, inline programs defined within your configuration or entries themselves – that generate content dynamically when evaluated.

---

## 💡 How Processors Work

Processors allow you to embed procedural logic directly into your text entries.

### Basic Example

Consider this simple entry using a built-in processor:

```html
The generated number is: @[weaver.core.rng(min: 0, max: 100)]
```

This defines an `rng` (random number generator) processor. When this part of the entry is evaluated, the processor executes and replaces itself with its output:

```
The generated number is: 73
```

### Syntax & Nesting

`ContextWeaver` uses a flexible pseudo-JSON format for defining processor properties and other components. The key difference from standard JSON is that property keys are **unquoted**, and attributes can be used alongside properties:

```json
// Standard JSON
{
  "property": "value"
}
```

```json
// ContextWeaver pseudo-JSON
{
  property: "value", // Unquoted keys in properties
}

<trigger id=0> // Attributes in tags
```

The real power emerges when you **nest** processors within each other:

```javascript
@[weaver.core.wildcard(
  items: [
    "The weather is: @[weaver.core.wildcard(items:["sunny", "cloudy", "rainy"])]",
    "The number is @[weaver.core.rng(min: 0, max: 100)]"
  ]
)]
```

**Evaluation Breakdown:**

1.  **Inner Evaluation:** The system first finds and evaluates the innermost processors:
    * `@[weaver.core.wildcard(items:["sunny", "cloudy", "rainy"])]` might evaluate to `cloudy`.
    * `@[weaver.core.rng(min: 0, max: 100)]` might evaluate to `35`.
2.  **Outer Substitution:** The results replace the inner processor calls within the outer processor's definition:
    * The outer processor becomes: `@[weaver.core.wildcard(items: ["The weather is: cloudy", "The number is 35"])]`
3.  **Outer Evaluation:** The outer `wildcard` processor now evaluates, randomly selecting one of its processed items, resulting in either:
    * `The weather is: cloudy`
    * `The number is 35`

This nesting allows for complex, emergent text generation based on combining simple processor functions.

---

### Variables
Variables are simple key-value pairs that can be inserted anywhere in your input, from directly in the text, to the input of a separate processor. Conceptually speaking, a variable can be used everywhere, however, realistically they should be used with caution. The type of a variable is not known until evaluation, and is computed at evaluation-time, meaning that if used as an input for a processor that does not properly handle mismatched types, it may fail.

> ![INFO]
> All core processors handle type mismatch gracefully

The syntax is as follows:
```
{{SCOPE:VAR_NAME}}
```
Every entry has access to two scopes: `global` and `ENTRY_ID` (where `ENTRY_ID` is the actual ID of the entry). Variables defined within the `ENTRY_ID` scope can only be accessed from within that specific entry, while `global`-scoped variables can be accessed everywhere. If you are in for some reason in need for additional scopes, custom scopes are planned down the road.

### Macros
Macros allow you to execute chains of more complex commands. The syntax of a macro varies from type-to-type, but all are contained within `{# ... #}` closures.

#### If-Macro
If Macros are simple if/else statements. The syntax is defined as:
```
{# if CONDITION #}
    [CONTENT]
({# else #})
    ([CONTENT])
{# endif #}
```
The condition can be more or less anything. It supports all binary operators, aswell as arthimetic operators. You can nest/group conditions within eachother, and use variables with the same syntax as stated above. You also have access to the following functions:
| Function       | Input                        | Returns                             |
|----------------|------------------------------|-------------------------------------|
| len(x)         | x: String, List, HashMap     | Length of the provided item `(Int)` |
| contains(x, y) | x: String, List<br>y: String | Whether `x` contains `y` `(Bool)`   |

#### Foreach Macro
A foreach macro allows you to perform iterative logic over a collection. Supported types include: `Lists`, `HashMaps` and `Strings`. Remember that all collections (apart from `Strings`) are loosely typed, meaning a collection may include mixed types.

The syntax is defined as:
```
{# foreach VAR_NAME in COLLECTION #}
    [ITERATIVE LOGIC (using VAR_NAME)]
{# endforeach #}
```

## 🔌 Plugin Support

Extend `ContextWeaver` with your own custom logic through **plugins**. By implementing a simple bridge interface, you can define and call your own processors.

### Example Plugin Bridge (Rust)

Here's how you might define a bridge in Rust to connect your plugin system:

```rust
use context_weaver::core::processors::PluginBridge;
use serde_json::Value;
use std::sync::Arc;
use rand::Rng; // For the dummy logic

#[derive(Clone)]
struct MyCustomPluginBridge;

impl PluginBridge for MyCustomPluginBridge {
    // Define your unique identifier type for plugins
    type PluginId = u32;

    fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) -> Result<String, WorldInfoError> {
        // Lookup plugin by ID. Your implementation would likely be more complex
        match plugin_id {
            0 => Ok(dummy_plugin(properties)),
            _ => Err(WorldInfoError::PluginError("Invalid plugin id".to_string())),
        }
    }
}

// Example custom logic for plugin ID 0
fn dummy_plugin_logic(properties: Value) -> Result<String, WorldInfoError> {
    // Extract data needed from the properties passed in the entry
    // Safely handle potential errors during property access
    match properties.get("items").and_then(Value::as_array) {
        Some(items) => {
            if items.is_empty() {
                Err(WorldInfoError::PluginError("Plugin Error: 'items' array is empty.".to_string()))
            } else {
                let index = rand::thread_rng().gen_range(0..items.len());
                match items[index].as_str() {
                    Some(item_str) => Ok(format!("The plugin's forecast is: {}", item_str)),
                    None => Err(WorldInfoError::PluginError(format!("Plugin Error: Item at index {} is not a string.", index))),
                }
            }
        },
        None => Err(WorldInfoError::PluginError("Plugin Error: Missing or invalid 'items' property.".to_string())),
    }
}

```

### Using the Plugin

To use your custom plugin, register it with the `ProcessorRegistry` and provide your bridge implementation:

```rust
use weaver_world_info::{WorldInfo, WorldInfoEntry, ProcessorRegistry};
use std::sync::Arc;

// 1. Create the registry, providing an instance of your bridge
let my_bridge = Arc::new(MyCustomPluginBridge);
let registry = ProcessorRegistry::new(my_bridge);

// 2. Register your plugin processor factory under a specific author and name
registry.register_plugin_processor("dummy", "test");

// 3. Inputs should be structured like this.
// plugin_author, plugin_name, plugin_id are needed for identification
let input = r#"@[weaver.plugin.dummy.test(
    plugin_author: "dummy", // Required
    plugin_name: "test",    // Required
    plugin_id: 0,           // Required
    plugin_data: {          // Optional, arbitrary plugin data
        items: ["sunny", "cloudy", "rainy"] 
    }
    )]"#;

// 4. Setup WorldInfo and evaluate
let mut worldinfo = WorldInfo::new(Box::new(registry));
let mut entry = WorldInfoEntry::create("test_entry", 0, 0);
entry.set_text(&input);
worldinfo.insert_entry(entry);

match worldinfo.evaluate() { 
    Ok(evaluated_result) => {
        println!("Evaluated Result: {}", evaluated_result);
        // Example Output: Evaluated Result: The plugin's forecast is: cloudy
    }
    Err(e) => {
        eprintln!("Evaluation Error: {}", e[0]); // Evaluate returns an array of errors
    }
}

```

When evaluated, `ContextWeaver` calls `invoke_plugin` on your bridge with the `plugin_id` (0) and the properties (`items`, etc.). Your custom Rust function (`dummy_plugin_logic`) runs and returns the dynamic result.

> [!IMPORTANT]
> Plugin processors should follow the naming scheme:
> `weaver.plugin.<plugin_author>.<plugin_name>`
> The `plugin_id` and any other properties are passed to your `invoke_plugin` method.

---

## 🛠️ Installation
### 1. Using Cargo
```bash
# Does not actually work yet, lol
cargo add context-weaver
```

### 2. Cloning the repository
1. Clone the repository using `git clone https://github.com/Cruxial0/context-weaver.git`
2. Create a **Cargo Workspace** and link the library to your main application

---

## 🚀 Usage Examples

* LLM context manipulation
* Text-based games (Can be used to generate random characters and scenarios)
* Applications that benefit from dynamic prompts (ComfyUI, A1111, etc.)

---

## 🚧 Roadmap & Progress

Here's a look at planned features and current progress:

### 🧠 Processor Workflow
- [x] Plugin support via bridge interface
- [x] More property types (int, bool, lists)
- [ ] Global-scoped processors (usable across all entries without import)

### 🚀 Activation Features
- [ ] Keyword-based activation (trigger entry on specific words)
- [ ] Regex-based activation (trigger entry on pattern match)
- [ ] Recursive activation (one entry activating others)

### 💾 Persistence
- [ ] JSON-based saving/loading of `WorldInfo` state and entries

### 🧩 Context Insertion
- [ ] Configurable insertion strategies (e.g., always insert, insert if condition met, disable)
- [ ] Frequency controls (e.g., insert only once, insert every N turns)

### 🪄 Macro & Templating System
- [x] Conditional blocks (`{{if}}` / `{{else}}`)
- [ ] Extended conditional statements
    - [ ] Array.length | String.length
    - [ ] Array.contains
    - [ ] Arthimetic operators (`+`, `-`, `*`, `/`)
- [x] Scoped variables (`{global:char}`, `{entry:activations}`)
- [ ] Custom scopes (tag system?)
- [ ] Variable mutation (`@[set(global:foo, "bar")]`, `@[modify(entry:user_score, operation='add', value=10)]`)
- [ ] Value piping/formatting functions (e.g., `{{variable | uppercase}}`)
- [ ] Triggers (macros that can activate other entries)

### 🧬 Syntax & Parsing
- [x] Improved input syntax (pseudo-JSON via. `pest`)
- [x] Structured syntax parsing implemented
- [x] Enhanced diagnostics for syntax errors
- [x] Robust error reporting with clear messages and locations

### Planned Syntax
| Type                  | Syntax                                              | Supported |
|-----------------------|-----------------------------------------------------|-----------|
| Processor             | @[processor.name(..properties)]                     | Yes       |
| Trigger               | <trigger id=0>                                      | Yes*      |
| If Macro              | {# if foo = bar #} baz {# endif #}                  | Yes*      |
| Foreach Macro         | {# foreach foo in bar #} baz(foo) {# endforeach #}  | No        |
| Variable Manipulation | @[set(var, value)]; @[modify(var, op, change)]; ... | No        |

> * Incomplete or partly supported

---

## 🤝 Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request.

---
