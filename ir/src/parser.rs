use crate::model::Module;

pub trait Parser {
    fn parse(&self, input: &str) -> anyhow::Result<Module>;
}
