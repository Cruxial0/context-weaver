#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use rand::Rng;
    use crate::{core::processors::{PluginBridge, RngProcessorFactory, WildcardProcessorFactory, WorldInfoRegistry}, id, Context, ContextNode, EntryFactory, ParserError, WorldInfo, WorldInfoEntry, WorldInfoError, WorldInfoFactory};

    #[derive(Debug, Clone)]
    struct DummyPluginBridge;

    impl PluginBridge for DummyPluginBridge {
        type PluginId = u32;

        fn invoke_plugin(&self, plugin_id: Self::PluginId, properties: serde_json::Value) -> Result<String, crate::WorldInfoError> {
            match plugin_id {
                0 => Ok(dummy_plugin(properties)),
                _ => Err(crate::WorldInfoError::ProcessorError("Invalid plugin id".to_string())),
            }
        }
    }

    fn dummy_plugin(properties: serde_json::Value) -> String {
        println!("{:?}", properties);
        let items = properties["items"].as_array().unwrap();
        let index = rand::rng().random_range(0..items.len());
        format!("The plugin's forecast is: {}", items[index].as_str().unwrap())
    }

    static EXAMPLE_INPUT: &str = "This is an example string with meant to serve as an example context. It contains words such as TRIGGER and TRIGGER2. Here is a short story: \"The quick brown fox jumps over the lazy dog.\"";

    fn init() {
        let _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).try_init();
    }

    fn example_context() -> Context {
        let mut context = Context::new();
        context.push(ContextNode::TextChunk(EXAMPLE_INPUT.to_string()));
        context.push(ContextNode::InsertionPoint("Default".to_string()));
        context
    }

    #[test]
    fn test_parser() {
        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.wildcard", Box::new(WildcardProcessorFactory));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        //let input = r#"Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "rainy"], test: 100)]!"#;
        let input = r#"The generated number is @[weaver.core.rng(min: 0, max: 100)]!, Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "rainy"])]!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));

        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        let mut valid_results = vec![];
        for i in 0..100 {
            valid_results.extend_from_slice(
                &[
                    format!("{}\nThe generated number is {}!, Today's weather is sunny!", context.text(), i),
                    format!("{}\nThe generated number is {}!, Today's weather is cloudy!",context.text(), i),
                    format!("{}\nThe generated number is {}!, Today's weather is rainy!", context.text(), i),
                ]
            );
        }

        let evaluated_result = worldinfo.evaluate(context).unwrap();
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_nested_parser() {
        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.wildcard", Box::new(WildcardProcessorFactory));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        
        let input = r#"Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "random: @[weaver.core.rng(min: 0, max: 100)]"])]!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        println!("Number of nodes: {}", worldinfo.entries[0].nodes.len());

        let mut valid_results = vec![
            format!("{}\nToday's weather is sunny!", context.text()),
            format!("{}\nToday's weather is cloudy!", context.text()),
        ];

        for i in 0..100 {
            valid_results.push(format!("{}\nToday's weather is random: {}!", context.text(), i));
        }

        let evaluated_result = worldinfo.evaluate(context).unwrap();
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_plugin_processor() {
        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_plugin_processor("dummy", "test");
        let input = r#"@[weaver.plugin.dummy.test(
            plugin_author: "dummy", 
            plugin_name: "test", 
            plugin_id: 0, 
            plugin_data: {
                items: ["sunny", "cloudy", "rainy"]
            }
            )]!"#;
        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap();
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        let valid_results = vec![
            format!("{}\nThe plugin's forecast is: sunny!", context.text()),
            format!("{}\nThe plugin's forecast is: cloudy!", context.text()),
            format!("{}\nThe plugin's forecast is: rainy!", context.text()),
        ];

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_variables() {
        init();

        let mut registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_variable("global:test".to_string(), "test".into());

        let input = r#"The variable's contents are "{{global:test}}"!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap();
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        assert_eq!(evaluated_result, format!("{}\nThe variable's contents are \"test\"!", context.text()));
    }
    
    #[test]
    fn test_if_macro() {      
        init();

        let mut registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        registry.register_variable("global:test".to_string(), true.into());
        registry.register_variable("global:counter".to_string(), 0.into());

        let input = r#"
        {# if {{global:test}} == true && (true == true && {{global:counter}} + @[weaver.core.rng(min: 0, max: 10)] > 5) #}
            The prophecy is true!
        {# else #}
            The prophecy is but a mere hoax!
        {# endif #}
        "#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);

        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap();
        let possible_results = vec![
            format!("{}\nThe prophecy is true!", context.text()),
            format!("{}\nThe prophecy is but a mere hoax!", context.text()),
            ]
            .iter().map(|s| s.to_string()).collect::<Vec<String>>();
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        assert!(possible_results.contains(&evaluated_result));
    }

    #[test]
    fn test_foreach_macro() {
        init();

        let mut registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        registry.register_variable("global:array".to_string(), vec![10, 15, 20, 25, 30].into());

        let input = "
        {# foreach item in {{global:array}} #}
            The item is {{item}}!\n
        {# endforeach #}
        ";

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);

        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let context = example_context();

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap();
        let result = format!("{}\nThe item is 10!The item is 15!The item is 20!The item is 25!The item is 30!", context.text());
        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        assert_eq!(evaluated_result, result);
    }

    #[test]
    fn test_invalid_processor() {
        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));

        let input = r#"@[weaver.core.wildcard(invalid: true)]"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);

        entry.set_text(&input);
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap_err(); // Should fail
        let valid = matches!(
            &evaluated_result[0],
            WorldInfoError::ParserError(ParserError::ProcessorInstantiation(_, _))
        );

        assert!(valid);
    }

    #[test]
    fn test_activation_conditions() {
        init();

        let conditions = vec!["TRIGGER", "{{global:trigger}}", "{{global:counter}} == 5", "@[weaver.core.rng(min: 2, max: 10)] > 1", "{{global:counter}} >= 5 && {{global:counter}} < 10"]
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();

        for cond in conditions {
            let mut registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
            registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
            registry.register_variable("global:trigger".to_string(), true.into());
            registry.register_variable("global:counter".to_string(), 5.into());

            let input = "We have reached this point!";

            let mut worldinfo = WorldInfo::new(Box::new(registry));
            let entry = worldinfo.new_entry("test", 0);

            entry.set_text(&input);
            entry.set_conditions(vec![cond]);
            entry.set_insertion_point("Default".to_string());

            let context = example_context();

            let evaluated_result = worldinfo.evaluate(example_context()).unwrap();
            let possible_results = format!("{}\nWe have reached this point!", context.text());
            println!("Evaluated result: {{\n{}\n}}", evaluated_result);

            assert_eq!(evaluated_result, possible_results);
        }
    }

    #[test]
    fn test_trigger_activation() {
        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));

        let id_generator = id::IdGenerator::new(6);
        let id_1 = id_generator.generate(); 
        let id_2 = id_generator.generate();

        // Entry 1: statically triggered, contains a trigger for Entry 2
        let mut entry = WorldInfoEntry::new("test1".to_string(), id_1, 0);
        entry.set_text(&format!(r#"This is a test containing a trigger <trigger id="{}">"#, id_2));
        entry.set_constant(true);
        entry.set_insertion_point("Default".to_string());

        // Entry 2: triggered by the trigger in Entry 1
        let mut entry2 = WorldInfoEntry::new("test2".to_string(), id_2, 1);
        entry2.set_text("Entry 2 was triggered!");
        entry2.set_insertion_point("Default".to_string());

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        worldinfo.insert_entry(entry);
        worldinfo.insert_entry(entry2);

        let context = example_context();

        let evaluated_result = worldinfo.evaluate(example_context()).unwrap();

        println!("Evaluated result: {{\n{}\n}}", evaluated_result);

        let result = format!("{}\nThis is a test containing a trigger \nEntry 2 was triggered!", context.text());
        assert_eq!(evaluated_result, result);

    }
}