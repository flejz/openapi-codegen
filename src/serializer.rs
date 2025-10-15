use crate::parser::Type;

pub trait TypeSerializer {
    fn serialize(&self, r#type: &Type) -> anyhow::Result<String>;
}
