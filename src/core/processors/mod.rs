// Declare submodules
pub mod wildcard;
pub mod rng;
pub mod plugin;

// Re-export processors and factories
pub use wildcard::{WildcardProcessor, WildcardProcessorFactory};
pub use rng::{RngProcessor, RngProcessorFactory};