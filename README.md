# ContextWeaver

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

```javascript
// Standard JSON
{
  "property": "value"
}
```

```javascript
// ContextWeaver pseudo-JSON
{
  property: "value", // Unquoted key
  attribute="value"  // Attribute style also supported
}
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

## 🔌 Plugin Support

Extend `ContextWeaver` with your own custom logic through **plugins**. By implementing a simple bridge interface, you can define and call your own processors.

### Example Plugin Bridge (Rust)

Here's how you might define a bridge in Rust to connect your plugin system:

```rust
use weaver_world_info::{PluginBridge, ProcessorResult}; // Assuming ProcessorResult is the return type
use serde_json::Value;
use std::sync::Arc;
use rand::Rng; // For the dummy logic

#[derive(Clone)]
struct MyCustomPluginBridge;

// Define your unique identifier type for plugins
type MyPluginId = u32;

impl PluginBridge for DummyPluginBridge {
    type PluginId = MyPluginId;

    fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) -> Result<String, WorldInfoError> {
        // Lookup plugin by ID. Your implementation would likely be more complex
        match plugin_id {
            0 => Ok(dummy_plugin(properties)),
            _ => Err(crate::WorldInfoError::ProcessorError("Invalid plugin id".to_string())),
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
                Err("Plugin Error: 'items' array is empty.".to_string())
            } else {
                let index = rand::thread_rng().gen_range(0..items.len());
                match items[index].as_str() {
                    Some(item_str) => Ok(format!("The plugin's forecast is: {}", item_str)),
                    None => Err(format!("Plugin Error: Item at index {} is not a string.", index)),
                }
            }
        },
        None => Err("Plugin Error: Missing or invalid 'items' property.".to_string()),
    }
}

```

### Using the Plugin

To use your custom plugin, register it with the `ProcessorRegistry` and provide your bridge implementation:

```rust
use weaver_world_info::{WorldInfo, WorldInfoEntry, ProcessorRegistry, plugin::PluginProcessorFactory};
use std::sync::Arc;
// Assuming the bridge and dummy logic from the previous example are accessible
// use crate::MyCustomPluginBridge; // If in another module

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
    items: ["sunny", "cloudy", "rainy"] // Optional, Custom properties for your logic
    )]"#;

// 4. Setup WorldInfo and evaluate
let mut worldinfo = WorldInfo::new(Box::new(registry));
let mut entry = WorldInfoEntry::create("test_entry", 0, 0); // Name, priority, order
entry.set_text(&input);
worldinfo.insert_entry(entry);

match worldinfo.evaluate() { // Assuming evaluate returns a Result
    Ok(evaluated_result) => {
        println!("Evaluated Result: {}", evaluated_result);
        // Example Output: Evaluated Result: The plugin's forecast is: cloudy
    }
    Err(e) => {
        eprintln!("Evaluation Error: {}", e);
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

```bash
# Example using Cargo (if applicable)
# cargo add ContextWeaver
```

---

## 🚀 Usage Examples

*(Add more comprehensive usage examples here, potentially showing:*
* *Creating and managing multiple entries.*
* *Defining activation conditions (once implemented).*
* *More complex processor interactions.*
* *Integration with an LLM pipeline.)*

The examples under "How Processors Work" and "Plugin Support" demonstrate basic evaluation and extension.

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
- [ ] Conditional blocks (`{{if}}` / `{{else}}`)
- [ ] Scoped variables (`{global:char}`, `{entry:activations}`)
- [ ] Variable mutation ()
- [ ] Value piping/formatting functions (e.g., `{{variable | uppercase}}`)
- [ ] Triggers (macros that can activate other entries)

### 🧬 Syntax & Parsing
- [x] Improved input syntax (pseudo-JSON via. `pest`)
- [x] Structured syntax parsing implemented
- [x] Enhanced diagnostics for syntax errors
- [x] Robust error reporting with clear messages and locations

---

## 🤝 Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request.

*(Add contribution guidelines if you have specific requirements)*

---

## 📜 License

*(Specify your project's license here, e.g., MIT, Apache 2.0)*