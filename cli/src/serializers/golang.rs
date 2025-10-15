use std::fmt;

use crate::{
    parser::{Field, FieldKind, Type, TypeKind},
    serializer::TypeSerializer,
};

#[derive(Debug, thiserror::Error)]
pub enum GolangSerializerError {
    #[error("failed to serialize")]
    FailedToSerialize,
    #[error("field type not supported")]
    FieldTypeNotSupported,
    #[error("kind not supported")]
    KindNotSupported,
}

pub struct GolangTypeSerializer;

impl GolangTypeSerializer {
    fn format_field_name(name: &String) -> String {
        let mut s = String::new();
        let mut capitalize = true;
        for c in name.chars() {
            if c == '_' || c == '-' {
                capitalize = true;
                continue;
            }
            if capitalize {
                s.extend(c.to_uppercase());
                capitalize = false;
            } else {
                s.push(c);
            }
        }
        s
    }

    fn parse_field_kind(field: &Field) -> String {
        match &field.kind {
            FieldKind::String => "string".into(),
            FieldKind::Number => "float64".into(),
            FieldKind::Integer => "int".into(),
            FieldKind::Boolean => "bool".into(),
            FieldKind::Array(array_type) => {
                let field_type = match &array_type.fields.first() {
                    Some(field) => Self::parse_field_kind(field),
                    None => "interface{}".into(),
                };
                format!("[]{}", field_type)
            }
            FieldKind::Object(object_type) => Self::format_field_name(&object_type.id),
            FieldKind::Reference(reference_type) => Self::format_field_name(&reference_type.id),
        }
    }

    fn parse_type_kind(r#type: &Type) -> String {
        match &r#type.kind {
            TypeKind::String => "string".into(),
            TypeKind::Number => "float64".into(),
            TypeKind::Integer => "int".into(),
            TypeKind::Boolean => "bool".into(),
            TypeKind::Array => format!(
                "[]{}{{}}",
                Self::parse_field_kind(&r#type.fields.first().unwrap())
            ),
            TypeKind::Object => "struct".into(),
            TypeKind::Enum => "string".into(),
        }
    }

    fn serialize_field(field: &Field) -> String {
        format!(
            "  {} {} `json:\"{}\"`\n",
            Self::format_field_name(&field.id),
            Self::parse_field_kind(&field),
            field.id
        )
    }

    fn serialize_enum_item(r#type: &Type, field: &Field) -> String {
        format!(
            "  {}{} {} = \"{}\"\n",
            Self::format_field_name(&r#type.id),
            Self::format_field_name(&field.id),
            Self::format_field_name(&r#type.id),
            match field.kind {
                FieldKind::String => format!("{}", field.id),
                _ => field.id.clone(),
            }
        )
    }
}

impl TypeSerializer for GolangTypeSerializer {
    fn serialize(&self, r#type: &Type) -> anyhow::Result<String> {
        let mut codegen = format!("mod {}", &r#type.id,);

        let type_kind = Self::parse_type_kind(r#type);

        codegen.push_str(&format!(
            "\n\ntype {} {}",
            Self::format_field_name(&r#type.id),
            type_kind
        ));

        let mut nested_types = Vec::new();
        let mut codegen = match r#type.kind {
            TypeKind::Enum => {
                codegen.push_str("\n\nconst (\n");

                r#type
                    .fields
                    .iter()
                    .for_each(|field| codegen.push_str(&Self::serialize_enum_item(r#type, field)));

                codegen.push_str(")");
                codegen
            }
            TypeKind::Object => {
                codegen.push_str(" {\n");
                for field in r#type.fields.iter() {
                    codegen.push_str(&Self::serialize_field(field));

                    if let FieldKind::Object(nested_type) = &field.kind
                        && !nested_type.fields.is_empty()
                    {
                        nested_types.push(nested_type);
                    }
                }

                codegen.push_str("}");
                codegen
            }
            _ => codegen,
        };

        for nested_type in nested_types {
            codegen.push_str(
                &self
                    .serialize(&nested_type)?
                    .replace(&format!("mod {}", nested_type.id), ""),
            );
        }

        Ok(codegen)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Type;

    use super::*;
    use openapiv3::SchemaData;

    #[test]
    fn golang_string_enum_generation() {
        let r#type = Type {
            id: "Color".into(),
            kind: TypeKind::Enum,
            fields: vec![
                Field {
                    id: "RED".into(),
                    kind: FieldKind::String,
                    meta: SchemaData::default(),
                },
                Field {
                    id: "GREEN".into(),
                    kind: FieldKind::String,
                    meta: SchemaData::default(),
                },
                Field {
                    id: "BLUE".into(),
                    kind: FieldKind::String,
                    meta: SchemaData::default(),
                },
            ],
        };

        let result = GolangTypeSerializer.serialize(&r#type);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(
            result,
            r#"mod Color

type Color string

const (
  ColorRED Color = "RED"
  ColorGREEN Color = "GREEN"
  ColorBLUE Color = "BLUE"
)"#
        );
    }

    #[test]
    fn golang_object_generation() {
        let r#type = Type {
            id: "User".into(),
            kind: TypeKind::Object,
            fields: vec![
                Field {
                    id: "email".into(),
                    kind: FieldKind::String,
                    meta: SchemaData::default(),
                },
                Field {
                    id: "age".into(),
                    kind: FieldKind::Integer,
                    meta: SchemaData::default(),
                },
            ],
        };

        let result = GolangTypeSerializer.serialize(&r#type);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(
            result,
            r#"mod User

type User struct {
  Email string `json:"email"`
  Age int `json:"age"`
}"#
        );
    }

    #[test]
    fn golang_nested_object_generation() {
        let r#type = Type {
            id: "NestedUser".into(),
            kind: TypeKind::Object,
            fields: vec![
                Field {
                    id: "email".into(),
                    kind: FieldKind::String,
                    meta: SchemaData::default(),
                },
                Field {
                    id: "age".into(),
                    kind: FieldKind::Integer,
                    meta: SchemaData::default(),
                },
                Field {
                    id: "address".into(),
                    kind: FieldKind::Object(Type {
                        id: "address".into(),
                        kind: TypeKind::Object,
                        fields: vec![
                            Field {
                                id: "street".into(),
                                kind: FieldKind::String,
                                meta: SchemaData::default(),
                            },
                            Field {
                                id: "number".into(),
                                kind: FieldKind::Integer,
                                meta: SchemaData::default(),
                            },
                        ],
                    }),
                    meta: SchemaData::default(),
                },
            ],
        };

        let result = GolangTypeSerializer.serialize(&r#type);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(
            result,
            r#"mod NestedUser

type NestedUser struct {
  Email string `json:"email"`
  Age int `json:"age"`
  Address Address `json:"address"`
}

type Address struct {
  Street string `json:"street"`
  Number int `json:"number"`
}"#
        );
    }
}
