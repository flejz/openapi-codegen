use ir::{Attrs, Primitive, QualifiedName, TypeKind, TypeRef};
use openapiv3::{ReferenceOr, Schema, SchemaKind, Type};

pub fn to_type_kind(schema: &Schema) -> TypeKind {
    match &schema.schema_kind {
        SchemaKind::Type(ty) => match ty {
            Type::String(_) => TypeKind::Primitive(Primitive::String),
            Type::Number(_) => TypeKind::Primitive(Primitive::F64),
            Type::Integer(_) => TypeKind::Primitive(Primitive::I64),
            Type::Boolean(_) => TypeKind::Primitive(Primitive::Bool),
            Type::Array(ty) => TypeKind::Array(Box::new(TypeRef {
                kind: match &ty.items {
                    Some(ref_or_schema) => match ref_or_schema {
                        ReferenceOr::Item(schema) => to_type_kind(&schema),
                        ReferenceOr::Reference { reference } => {
                            TypeKind::Named(QualifiedName(reference.clone()), vec![])
                        }
                    },
                    None => TypeKind::Primitive(Primitive::Any),
                },
                nullable: schema.schema_data.nullable,
                optional: schema.schema_data.nullable,
                mutable: !schema.schema_data.read_only,
                attrs: Attrs::default(),
            })),
            Type::Object(_) => TypeKind::Primitive(Primitive::Object),
        },
        SchemaKind::OneOf { one_of: _ } => todo!("one of is not supported"),
        SchemaKind::AllOf { all_of: _ } => todo!("all of is not supported"),
        SchemaKind::AnyOf { any_of: _ } => todo!("any of is not supported"),
        SchemaKind::Not { not: _ } => todo!("not is not supported"),
        SchemaKind::Any(_) => TypeKind::Primitive(Primitive::Any),
    }
}
