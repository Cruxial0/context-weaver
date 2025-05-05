# ContextWeaver

**ContextWeaver** is a dynamic and highly extensible text evaluation library. It allows developers to seamlessly integrate a "lorebook" type data structure into their application. 

The library comes shipped with a macro language to aid in creating dynamic content. It can be used for anything from reading and writing simple variables, to executing robust logic on evaluation-time. More about that later.

## Core Principles
**ContextWeaver** Allows you to create what can only be described as a "Book". The WorldInfo ("Book") can be thought of as a collection of entries and documents (pages). Each entry contains text that will be evaluated upon activation, and each entry can be configured to suit your needs. The macro language lets you take your book a step further by creating highly dynamic and procedural logic based on the given input or the WorldInfo's internal state (such as variables).

## Use cases
The primary focus of **ContextWeaver** is to create an environment for end-users to define their own LLM context sequences. It can be used for more serious work, to more fun concepts such as RP. Apart from this, **ContextWeaver** can be used anywhere where procedural text systems are required, a few examples being:
- Text Game Systems (Character Generators)
- Prompt Engines
- more...

## The Weaver Language
The weaver language operates as you'd expect. It converts plain text into runtime logic that will be evaluated on-the-fly as it's parent entries are activated. It's written using [pest.rs](https://pest.rs/), and has a heavy focus on interchangability (<- this word almost definitely does not exist), meaning a component (given it has a return type) can be used virtually anywhere--from a plain-text output, to the input of another component.

### Components
#### Variables
Variables are defined as `{{scope:variable}}`. They are, well... variables. They can be used either on root level to replace it's identifier with the internal value, or used as the input for another component. Currently, only two scopes are included: `global` and `local`, but custom scopes are planned. It is also worth mentioning that `local` is syntactic sugar for the entry's id. In evaluation, `{{local:username}}` will be replaced with something alone the lines of `{{2ad28e:username}}`. Local variables are only acessible from within the entry, while global variables are accessible from all entries.

#### Processors
Processors are defined as `@![processor.name[..props]]`. They are inline programs that execute custom logic on the given input. These processors can be anything from a simple **Random Number Generator** to something more complex such as **Spell Checking**. **ContextWeaver** comes pre-packed with a few processors which can be enabled:
| Identifier          | Description                                  |
|---------------------|----------------------------------------------|
|core.weaver.rng      | Generates a random number                    |
|core.weaver.wildcard | Picks one item at random from an input array |
|more soon...         |                                              |

#### Macros
Macros are simple operations that support dynamic, multi-line data. They are generally defined using `{# ... #}`, however, the syntax from macro-to-macro varies, so make sure to check the documentation first. Macros mirror common programming concepts, such as **If Statements** and **Foreach Loops**. Currently, only these two are supported.

#### Functions
Defined as `@[namespace.func(..params)]`, a function is a way to modify the internal state of the WorldInfo. It has registry access, meaning it can access variables (within scope permissions) and apply data mutation. Developers can also extend their WorldInfo with **Custom Functions**, allowing you to do virtually anything. Safety is not guaranteed out of the box (yet).

#### Triggers
Triggers are a way of deterministically activating an entry. It's defined as `<trigger id=...>`, and upon being reached, will guarantee the linked entry to be triggered. No special quirks. That's all there is to it.

#### Documents (W.I.P)
Documents are conceptually the same as entries, but with a few glaring changes. Documents can not be activated using activation conditions, and must be explicitly imported via another entry. Documents can not create local variables (and might not have write permission. Undecided as of now.). Apart from that, a document is the same as entries. It can contain any components, which will be evaluated just as an entry would. A document can be used in another entry with the `[[DOCUMENT_ID]]` syntax.

## Plugin Support
ContextWeaver is built with extensibility in mind. You, the developer, can easily add your own logic through Processors and Functions.

### Custom Functions
Custom functions are added by imlementing the `ModFunction` trait. The function expects a `call` function, aswell as a method signature function. Here is a basic example from the internal `pretty_print` function:
```rs
impl<P : PluginBridge> ModFunction<P> for PrettyPrintFunction<P> {
    fn call(&self, args: Vec<Value>, _: &ScopedRegistry<P>) -> Result<Value, ParserError> {
        let value = &self.signature().1[0];
        let input = args[0].clone();
        let concrete = value.param_type.concrete_type_for(&input);
        if concrete.is_none() {
            return Err(ParserError::TypeMismatch(format!("Expected {}, got {}", value.param_type, input)));
        }

        let pretty_output = serde_json::to_string_pretty(&input).unwrap();

        Ok(Value::String(pretty_output))
    }

    fn signature(&self) -> (String, Vec<ParamSignature>) {
        let input_param = ParamSignature {
            param_type: ParamType::Any(vec![ParamType::Array, ParamType::Object]),
            name: "input".to_string(),
            description: "The input to pretty print".to_string(),
        };

        ("pretty_print".to_string(), vec![input_param])
    }
}
```
That's all there is to it! You can now register your function through `Registry::register_function`, and it will be available during evaluation.

### Custom Processors
Custom processors are a bit more work to get working. It works using a `PluginBridge` to pass logic from the evaluated text, to your plugin system. Here is a basic implementation:
```rs
#[derive(Debug, Clone)]
struct DummyPluginBridge;

// This is a simple representation of a plugin system. In reality you'd need a more robust system
impl PluginBridge for DummyPluginBridge {
    type PluginId = u32; // You can define your own id type

    fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) -> Result<String, WorldInfoError> {
        match plugin_id {
            0 => Ok(dummy_plugin(properties)),
            _ => Err(crate::WorldInfoError::ProcessorError("Invalid plugin id".to_string())),
        }
    }
}
```
This defines the plugin bridge you'll use. All properties will be passed through this bridge. You can now create your registry using this bridge:
```rs
let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
registry.register_plugin_processor("dummy", "test");
let input = r#"@![weaver.plugin.dummy.test(
    plugin_author: "dummy", 
    plugin_name: "test", 
    plugin_id: 0, 
    plugin_data: {
        items: ["sunny", "cloudy", "rainy"]
    }
    )]!"#;
```
It's important to note that when creating plugin processors, the `plugin_author`, `plugin_name`, `plugin_id` fields are all required. The internal system identifies plugins as `weaver.core.<author>.<name>`. `plugin_data` is an arbitrary json object that will be passed to your plugin handler.

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
- ✅ Extended conditionals, variable mutation, piping

### Syntax & Parsing

- ✅ Pseudo-JSON syntax via Pest  
- ✅ Structured parsing & diagnostics  
- ✅ Clear error messages  

| Construct               | Syntax                                  | Supported |
|-------------------------|-----------------------------------------|-----------|
| Processor               | `@![processor.name(props)]`             | Yes       |
| Trigger                 | `<trigger id=…>`                        | Yes       |
| If-Macro                | `{# if … #}…{# endif #}`                | Yes       |
| Foreach-Macro           | `{# foreach … #}…{# endforeach #}`      | Yes       |
| Variable mutation       | `@[set]`, `@[modify]`, etc.             | No        |
| Documents               | `[[DOCUMENT_ID]]`                       | Kinda (no)|

---

## 🤝 Contributing

Contributions are welcome! Please open issues or PRs on the [GitHub repository](https://github.com/Cruxial0/context-weaver).