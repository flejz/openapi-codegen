use openapiv3::{ObjectType, Schema, SchemaData, SchemaKind, Type as OpenAPIType};
use regex::Regex;

#[derive(Debug, Default, PartialEq)]
pub(crate) enum FieldKind {
    #[default]
    String,
    Number,
    Integer,
    Boolean,
    Array(Type),
    Object(Type),
    Reference(Type),
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Field {
    pub id: String,
    pub kind: FieldKind,
    pub meta: SchemaData,
}

impl Field {
    pub fn new_string(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::String,
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_enum(id: &String, schema: &Schema) -> Vec<Self> {
        if let SchemaKind::Type(OpenAPIType::String(string_type)) = &schema.schema_kind {
            string_type
                .enumeration
                .iter()
                .filter_map(|item| {
                    if let Some(_) = item {
                        Self {
                            id: id.clone(),
                            kind: FieldKind::String,
                            meta: schema.schema_data.clone(),
                        }
                        .into()
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            panic!("Enum schema is not a string");
        }
    }

    pub fn new_number(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Number,
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_integer(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Integer,
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_boolean(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Boolean,
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_array(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Array(
                Type::try_from(id, schema).expect("failed to parse array schema"),
            ),
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_object(id: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Object(
                Type::try_from(id, schema).expect("failed to parse object schema"),
            ),
            meta: schema.schema_data.clone(),
        }
    }

    pub fn new_reference(id: &String, reference: &String, schema: &Schema) -> Self {
        Self {
            id: id.clone(),
            kind: FieldKind::Reference(
                Type::try_from(
                    &Regex::new(r"#.*/")
                        .unwrap()
                        .replace(reference, "")
                        .to_string(),
                    &Schema {
                        schema_data: SchemaData::default(),
                        schema_kind: SchemaKind::Type(OpenAPIType::Object(ObjectType::default())),
                    },
                )
                .expect("failed to parse reference schema"),
            ),
            meta: schema.schema_data.clone(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum TypeError {
    #[error("failed to parse schema")]
    FailedToParse,
}

#[derive(Debug, Default, PartialEq)]
pub(crate) enum TypeKind {
    Enum,
    String,
    Number,
    Integer,
    Boolean,
    Array,
    #[default]
    Object,
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Type {
    pub id: String,
    pub kind: TypeKind,
    pub fields: Vec<Field>,
}

impl Type {
    pub fn try_from(id: &String, schema: &Schema) -> Result<Self, TypeError> {
        use openapiv3::{ReferenceOr, SchemaKind, Type};

        match &schema.schema_kind {
            SchemaKind::Type(t) => Ok(Self {
                id: id.clone(),
                kind: match t {
                    Type::String(string_type) => {
                        if string_type.enumeration.is_empty() {
                            TypeKind::String
                        } else {
                            TypeKind::Enum
                        }
                    }
                    Type::Number(_) => TypeKind::Number,
                    Type::Integer(_) => TypeKind::Integer,
                    Type::Boolean(_) => TypeKind::Boolean,
                    Type::Array(_) => TypeKind::Array,
                    Type::Object(_) => TypeKind::Object,
                },
                fields: match t {
                    Type::Array(array_type) => array_type
                        .items
                        .iter()
                        .map(|schema| match schema {
                            ReferenceOr::Item(item) => match &item.schema_kind {
                                SchemaKind::Type(t) => match t {
                                    Type::String(_) => Field::new_string(id, &item),
                                    Type::Number(_) => Field::new_number(id, &item),
                                    Type::Integer(_) => Field::new_integer(id, &item),
                                    Type::Boolean(_) => Field::new_boolean(id, &item),
                                    Type::Array(_) => Field::new_array(id, &item),
                                    Type::Object(_) => Field::new_object(id, &item),
                                    _ => panic!("Unknown schema type"),
                                },
                                _ => panic!("Unknown schema kind"),
                            },
                            ReferenceOr::Reference { reference } => {
                                todo!("Reference: {reference}")
                            }
                        })
                        .collect(),
                    Type::Object(object_type) => object_type
                        .properties
                        .iter()
                        .map(|(id, ref_or_schema)| match ref_or_schema {
                            ReferenceOr::Item(schema) => match &schema.schema_kind {
                                SchemaKind::Type(t) => match t {
                                    Type::String(_) => Field::new_string(id, &schema),
                                    Type::Number(_) => Field::new_number(id, &schema),
                                    Type::Integer(_) => Field::new_integer(id, &schema),
                                    Type::Boolean(_) => Field::new_boolean(id, &schema),
                                    Type::Array(_) => Field::new_array(id, &schema),
                                    Type::Object(_) => Field::new_object(id, &schema),
                                    _ => todo!("Unknown schema type"),
                                },
                                _ => todo!("Unknown schema kind"),
                            },
                            ReferenceOr::Reference { reference } => {
                                Field::new_reference(id, reference, &schema)
                            }
                        })
                        .collect(),
                    Type::String(string_type) if !string_type.enumeration.is_empty() => string_type
                        .enumeration
                        .iter()
                        .filter_map(|item| {
                            if let Some(id) = item {
                                Field::new_string(id, &schema).into()
                            } else {
                                None
                            }
                        })
                        .collect(),
                    _ => vec![],
                },
            }),
            _ => Err(TypeError::FailedToParse),
        }
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use openapiv3::{
        ArrayType, BooleanType, IntegerType, NumberType, ObjectType, ReferenceOr, Schema,
        SchemaData, SchemaKind, StringType, Type as OpenAPIType, VariantOrUnknownOrEmpty,
    };

    use crate::parser::{Field, FieldKind, Type, TypeKind};

    #[test]
    fn test_primitive_types() {
        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::String(StringType::default())),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert!(result.fields.is_empty());
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::String);

        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::Number(NumberType::default())),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert!(result.fields.is_empty());
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Number);

        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::Integer(IntegerType::default())),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert!(result.fields.is_empty());
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Integer);

        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::Boolean(BooleanType::default())),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert!(result.fields.is_empty());
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Boolean);
    }

    #[test]
    fn test_enum_type() {
        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::String(StringType {
                format: VariantOrUnknownOrEmpty::Empty,
                pattern: None,
                enumeration: vec![Some("MANUAL".into()), Some("DIGITAL".into())],
                min_length: None,
                max_length: None,
            })),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Enum);
        assert_eq!(result.fields.len(), 2);
        assert_eq!(result.fields[0].id, "MANUAL");
        assert_eq!(result.fields[0].kind, FieldKind::String);
        assert_eq!(result.fields[1].id, "DIGITAL");
        assert_eq!(result.fields[1].kind, FieldKind::String);
    }

    #[test]
    fn test_array_type() {
        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::String(StringType::default())),
        };

        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::Array(ArrayType {
                items: Some(ReferenceOr::Item(Box::new(schema))),
                min_items: None,
                max_items: None,
                unique_items: false,
            })),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert_eq!(result.fields.len(), 1);
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Array);
        assert_eq!(result.fields[0].id, "Test");
        assert_eq!(result.fields[0].kind, FieldKind::String);
    }

    #[test]
    fn test_object_type() {
        let schema = Schema {
            schema_data: SchemaData::default(),
            schema_kind: SchemaKind::Type(OpenAPIType::Object(ObjectType {
                properties: IndexMap::from([(
                    "address".into(),
                    ReferenceOr::Item(Box::new(Schema {
                        schema_data: SchemaData::default(),
                        schema_kind: SchemaKind::Type(OpenAPIType::Object(ObjectType {
                            properties: IndexMap::from([
                                (
                                    "street".into(),
                                    ReferenceOr::Item(Box::new(Schema {
                                        schema_data: SchemaData::default(),
                                        schema_kind: SchemaKind::Type(OpenAPIType::String(
                                            StringType::default(),
                                        )),
                                    })),
                                ),
                                (
                                    "number".into(),
                                    ReferenceOr::Item(Box::new(Schema {
                                        schema_data: SchemaData::default(),
                                        schema_kind: SchemaKind::Type(OpenAPIType::Integer(
                                            IntegerType::default(),
                                        )),
                                    })),
                                ),
                            ]),
                            required: vec![],
                            additional_properties: None,
                            min_properties: None,
                            max_properties: None,
                        })),
                    })),
                )]),
                required: vec![],
                additional_properties: None,
                min_properties: None,
                max_properties: None,
            })),
        };

        let result = Type::try_from(&"Test".into(), &schema).expect("failed to parse schema");
        assert_eq!(result.id, "Test");
        assert_eq!(result.kind, TypeKind::Object);
        assert_eq!(result.fields.len(), 1);
        assert_eq!(result.fields[0].id, "address");
        assert_eq!(
            result.fields[0].kind,
            FieldKind::Object(Type {
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
            })
        );

        if let FieldKind::Object(object) = &result.fields[0].kind {
            assert_eq!(object.id, "address");
            assert_eq!(object.kind, TypeKind::Object);
            assert_eq!(object.fields.len(), 2);
            assert_eq!(object.fields[0].id, "street");
            assert_eq!(object.fields[0].kind, FieldKind::String);
            assert_eq!(object.fields[1].id, "number");
            assert_eq!(object.fields[1].kind, FieldKind::Integer);
        } else {
            panic!("Field kind is not an object");
        }
    }
}
