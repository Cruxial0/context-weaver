#[cfg(test)]
mod tests {
    use rand::Rng;
    use crate::core::processors::PluginBridge;

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

    fn init() {
        let _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Trace).try_init();
    }

    #[test]
    fn test_parser() {
        use crate::core::processors::{WildcardProcessorFactory, WorldInfoRegistry, RngProcessorFactory};
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory};
        use std::sync::Arc;

        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.wildcard", Box::new(WildcardProcessorFactory));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        //let input = r#"Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "rainy"], test: 100)]!"#;
        let input = r#"The generated number is @[weaver.core.rng(min: 0, max: 100)]!, Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "rainy"])]!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));

        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(input);

        let mut valid_results = vec![];
        for i in 0..100 {
            valid_results.extend_from_slice(
                &[
                    format!("The generated number is {}!, Today's weather is sunny!", i),
                    format!("The generated number is {}!, Today's weather is cloudy!", i),
                    format!("The generated number is {}!, Today's weather is rainy!", i),
                ]
            );
        }

        let evaluated_result = worldinfo.evaluate().unwrap();
        println!("Evaluated result: {}", evaluated_result);

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_nested_parser() {
        use crate::core::processors::{WorldInfoRegistry, WildcardProcessorFactory, RngProcessorFactory};
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory};
        use std::sync::Arc;

        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_processor("weaver.core.wildcard", Box::new(WildcardProcessorFactory));
        registry.register_processor("weaver.core.rng", Box::new(RngProcessorFactory));
        
        let input = r#"Today's weather is @[weaver.core.wildcard(items: ["sunny", "cloudy", "random: @[weaver.core.rng(min: 0, max: 100)]"])]!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);
        entry.set_text(&input);

        println!("Number of nodes: {}", worldinfo.entries[0].nodes.len());

        let mut valid_results = vec![
            "Today's weather is sunny!".to_string(),
            "Today's weather is cloudy!".to_string(),
            "Today's weather is very sunny!".to_string(),
            "Today's weather is very cloudy!".to_string(),
        ];

        for i in 0..100 {
            valid_results.push(format!("Today's weather is random: {}!", i));
        }

        let evaluated_result = worldinfo.evaluate().unwrap();
        println!("Evaluated result: {}", evaluated_result);

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_plugin_processor() {
        use crate::core::processors::WorldInfoRegistry;
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory,};
        use std::sync::Arc;

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

        let evaluated_result = worldinfo.evaluate().unwrap();
        println!("Evaluated result: {}", evaluated_result);

        let valid_results = vec![
            "The plugin's forecast is: sunny!".to_string(),
            "The plugin's forecast is: cloudy!".to_string(),
            "The plugin's forecast is: rainy!".to_string(),
        ];

        assert!(valid_results.contains(&evaluated_result));
    }

    #[test]
    fn test_variables() {
        use crate::core::processors::WorldInfoRegistry;
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory};
        use std::sync::Arc;

        init();

        let mut registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));
        registry.register_variable("global:test".to_string(), "test".into());

        let input = r#"The variable's contents are "{{global:test}}"!"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);

        entry.set_text(&input);

        let evaluated_result = worldinfo.evaluate().unwrap();
        println!("Evaluated result: {}", evaluated_result);

        assert_eq!(evaluated_result, "The variable's contents are \"test\"!");
    }
    
    #[test]
    fn test_if_macro() {
        use crate::core::processors::{WorldInfoRegistry, RngProcessorFactory};
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory};
        use std::sync::Arc;
        
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

        let evaluated_result = worldinfo.evaluate().unwrap();
        let possible_results = vec!["The prophecy is true!", "The prophecy is but a mere hoax!"].iter().map(|s| s.to_string()).collect::<Vec<String>>();
        println!("Evaluated result: {}", evaluated_result);

        assert!(possible_results.contains(&evaluated_result));
    }

    #[test]
    fn test_invalid_processor() {
        use crate::{ParserError, WorldInfoError};
        use crate::core::processors::WorldInfoRegistry;
        use crate::{WorldInfo, EntryFactory, WorldInfoFactory};
        use std::sync::Arc;

        init();

        let registry = WorldInfoRegistry::new(Arc::new(DummyPluginBridge));

        let input = r#"@[weaver.core.wildcard(invalid: true)]"#;

        let mut worldinfo = WorldInfo::new(Box::new(registry));
        let entry = worldinfo.new_entry("test", 0);

        entry.set_text(&input);

        let evaluated_result = worldinfo.evaluate().unwrap_err(); // Should fail
        let valid = matches!(
            &evaluated_result[0],
            WorldInfoError::ParserError(ParserError::ProcessorInstantiation(_, _))
        );

        assert!(valid);
    }
}