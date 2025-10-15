use crate::model::Module;

pub trait Serializer {
    fn serialize(&self, module: Module) -> anyhow::Result<String>;
}
