# ContextWeaver

> **Warning:** This project is in an **early development** stage. Breaking changes will be frequent.

**ContextWeaver** is a *(WIP)* framework for dynamically managing and injecting “lore” into text-generation contexts (e.g., LLM prompts, game scripts, dynamic UIs). It uses _entries_ with _activation conditions_ plus inline _processors_ to produce context-aware, procedurally generated text.

---

## ✨ Core Concepts

1. **📄 Context Injection**  
   Each entry contains a payload of text (“lore”) and one or more activation conditions. When a condition is met, that text is inserted into the target context (for example, an LLM prompt or chat history).  
   _Activation conditions: see Roadmap._

2. **⚙️ Dynamic Content via Processors**  
   Entries can include inline “processors”—small snippets of logic that run at evaluation time and replace themselves with generated output. This makes your injected text dynamic rather than static.

---

## 💡 Processors

Processors embed procedural logic directly in your entries using a familiar, JSON-like syntax.

### Syntax

- **Basic call:**  
  ```text
  @[weaver.core.rng(min: 0, max: 100)]
  ```
- **Pseudo-JSON properties** use unquoted keys and allow inline comments:
  ```text
  {
    property: "value", // no quotes around property
  }
  ```
- **Tag attributes:**  
  ```html
  <trigger id=0>
  ```

### Example: Random Number

```text
The generated number is: @[weaver.core.rng(min: 0, max: 100)]
```

→  
```
The generated number is: 73
```

### Nesting

You can nest processors arbitrarily:

```text
@[weaver.core.wildcard(
  items: [
    "Weather: @[weaver.core.wildcard(items:[\"sunny\",\"cloudy\",\"rainy\"])]",
    "Number: @[weaver.core.rng(min:0,max:100)]"
  ]
)]
```

**Evaluation steps:**
1. **Inner processors** resolve first (e.g. picks `"cloudy"` or a random number).  
2. **Outer wildcard** then picks among the now-concrete strings.

---

## 📝 Variables

Use variables to inject key-value data at evaluation time.

- **Syntax:** `{{SCOPE:VAR_NAME}}`  
- **Scopes:**  
  - `global` — accessible everywhere  
  - `ENTRY_ID` — only within that entry  
- **Type note:** Types are inferred at runtime. Core processors handle mismatches gracefully, but custom logic should guard against bad types.

---

## 🔄 Macros

Macros let you write control flow in your entries. They live in `{# ... #}` blocks.

### If-Macro

```text
{# if CONDITION #}
  …true branch…
{# else #}
  …false branch…
{# endif #}
```

- Supports binary and arithmetic operators, grouping, and variable interpolation.
- Built-in helper functions:
  | Function       | Input                 | Returns           |
  | -------------- | --------------------- | ----------------- |
  | `len(x)`       | String, List, HashMap | Integer length    |
  | `contains(x,y)`| String/List & String  | Boolean membership|

### Foreach-Macro

```text
{# foreach ITEM in COLLECTION #}
  …use {{ITEM}}…
{# endforeach #}
```

Iterate over lists, maps, or strings; collections may mix types.

---

## 🔌 Plugin Support

Define your own processors via a _bridge interface_. Below is a Rust example.

### Bridge Trait (Rust)

```rust
use context_weaver::core::processors::PluginBridge;
use serde_json::Value;

struct MyBridge;

impl PluginBridge for MyBridge {
    type PluginId = u32;

    fn invoke_plugin(
        &self,
        plugin_id: Self::PluginId,
        properties: Value
    ) -> Result<String, WorldInfoError> {
        match plugin_id {
            0 => dummy_logic(properties),
            _ => Err(WorldInfoError::PluginError("Unknown plugin".into())),
        }
    }
}
```

### Dummy Plugin Logic

```rust
fn dummy_logic(props: Value) -> Result<String, WorldInfoError> {
    // Parse input properties carefully
    let items = props.get("items").and_then(Value::as_array)
        .ok_or(WorldInfoError::PluginError("'items' missing".into()))?;

    let choice = items
        .iter()
        .filter_map(Value::as_str)
        .choose(&mut rand::thread_rng())
        .ok_or(WorldInfoError::PluginError("No valid items".into()))?;
    Ok(format!("Forecast: {}", choice))
}
```

### Register & Use

```rust
let registry = ProcessorRegistry::new(Arc::new(MyBridge));
registry.register_plugin_processor("dummy", "forecast");

let input = r#"@[weaver.plugin.dummy.forecast(
  plugin_author:"dummy",
  plugin_name:"forecast",
  plugin_id:0,
  plugin_data:{items:["sunny","rainy"]}
)]"#;

let mut wi = WorldInfo::new(Box::new(registry));
wi.insert_entry(WorldInfoEntry::create("e1", 0).with_text(input));

let result = wi.evaluate()?;
println!("{}", result); // e.g. "Forecast: rainy"
```

> **Naming convention:** `weaver.plugin.<author>.<name>`

---

## 🛠️ Installation

#### Cargo

```bash
cargo add context-weaver
```

#### From Source

```bash
git clone https://github.com/Cruxial0/context-weaver.git
# Add to your Cargo.toml as a workspace member or dependency path
```

---

## 🚀 Usage Scenarios

- **LLM prompt orchestration**  
- **Text-based games:** random NPCs, scenarios  
- **Dynamic UIs:** ComfyUI, AUTOMATIC1111 prompts  
- **Any app** needing runtime-computed text

---

## 🚧 Roadmap

### Processor & Activation

- ✅ Plugin support  
- ✅ Various property types  
- ❌ Documents ([[DOCUMENT_ID]])
- ✅ Keyword/regex/recursive activation  

### Persistence

- ❌ JSON save/load of state

### Context Insertion

- ❌ Configurable strategies & frequency

### Macro & Templating

- ✅ If-blocks  
- ✅ Scoped variables  
- ❌ Extended conditionals, variable mutation, piping

### Syntax & Parsing

- ✅ Pseudo-JSON syntax via Pest  
- ✅ Structured parsing & diagnostics  
- ✅ Clear error messages  

| Construct               | Syntax                                  | Supported |
|-------------------------|-----------------------------------------|-----------|
| Processor               | `@[processor.name(props)]`              | Yes       |
| Trigger                 | `<trigger id=…>`                        | Yes       |
| If-Macro                | `{# if … #}…{# endif #}`                | Yes       |
| Foreach-Macro           | `{# foreach … #}…{# endforeach #}`      | Yes       |
| Variable mutation       | `@[set]`, `@[modify]`, etc.             | No        |
| Documents               | `[[DOCUMENT_ID]]`                       | Kinda (no)|

---

## 🤝 Contributing

Contributions are welcome! Please open issues or PRs on the [GitHub repository](https://github.com/Cruxial0/context-weaver).