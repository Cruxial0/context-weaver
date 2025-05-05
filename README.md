# ContextWeaver

**ContextWeaver** is a dynamic and highly extensible text evaluation library. It allows developers to seamlessly integrate a "lorebook" type data structure into their application. 

The library comes shipped with a macro language to aid in creating dynamic content. It can be used for anything from reading and writing simple variables, to executing robust logic on evaluation-time. More about that later.

## Core Principles
**ContextWeaver** Allows you to create what can only be described as a "Book". The WorldInfo ("Book") can be thought of as a collection of entries and documents (pages). Each entry contains text that will be evaluated upon activation, and each entry can be configured to suit your needs. The macro language lets you take your book a step further by creating highly dynamic and procedural logic based on the given input or the WorldInfo's internal state (such as variables).

## The Macro Language
The macro language operates as you'd expect. It converts plain text into runtime logic that will be evaluated on-the-fly as it's parent entries are activated.


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
| Processor               | `@![processor.name(props)]`              | Yes       |
| Trigger                 | `<trigger id=…>`                        | Yes       |
| If-Macro                | `{# if … #}…{# endif #}`                | Yes       |
| Foreach-Macro           | `{# foreach … #}…{# endforeach #}`      | Yes       |
| Variable mutation       | `@[set]`, `@[modify]`, etc.             | No        |
| Documents               | `[[DOCUMENT_ID]]`                       | Kinda (no)|

---

## 🤝 Contributing

Contributions are welcome! Please open issues or PRs on the [GitHub repository](https://github.com/Cruxial0/context-weaver).