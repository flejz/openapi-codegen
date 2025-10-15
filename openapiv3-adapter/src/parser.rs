use ir::{
    Attrs, Enum, EnumRepr, Field, Literal, Module, Parser, Primitive, QualifiedName, Struct,
    Symbol, TypeAlias, TypeKind, TypeRef, Variant, VariantPayload, Visibility,
};
use openapiv3::{OpenAPI, ReferenceOr, Schema, SchemaKind, Type};

#[derive(Debug, thiserror::Error)]
pub enum OpenAPIV3ParserError {
    #[error("failed to parse OpenAPI spec")]
    FailedToParse,
}

pub struct OpenAPIV3Parser;

impl OpenAPIV3Parser {
    fn to_type_kind(schema: &Schema) -> TypeKind {
        match &schema.schema_kind {
            SchemaKind::Type(ty) => match ty {
                Type::String(_) => TypeKind::Primitive(Primitive::String),
                Type::Number(_) => TypeKind::Primitive(Primitive::F64),
                Type::Integer(_) => TypeKind::Primitive(Primitive::I64),
                Type::Boolean(_) => TypeKind::Primitive(Primitive::Bool),
                Type::Array(ty) => TypeKind::Array(Box::new(TypeRef {
                    kind: match &ty.items {
                        Some(ref_or_schema) => match ref_or_schema {
                            ReferenceOr::Item(schema) => Self::to_type_kind(&schema),
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

    fn to_symbol((name, ref_or_schema): (&String, &ReferenceOr<Schema>)) -> Symbol {
        let schema = match ref_or_schema {
            ReferenceOr::Item(schema) => schema,
            ReferenceOr::Reference { reference } => {
                return Symbol::TypeAlias(TypeAlias {
                    name: name.clone(),
                    target: TypeRef {
                        kind: TypeKind::Named(QualifiedName(reference.clone()), vec![]),
                        nullable: false,
                        optional: false,
                        mutable: false,
                        attrs: Attrs::default(),
                    },
                    vis: Visibility::Public,
                    docs: String::default(),
                    attrs: Attrs::default(),
                });
            }
        };

        let type_ref = TypeRef {
            kind: Self::to_type_kind(schema),
            nullable: schema.schema_data.nullable,
            optional: schema.schema_data.nullable,
            mutable: !schema.schema_data.read_only,
            attrs: Attrs::default(),
        };

        match &schema.schema_kind {
            SchemaKind::Type(t) => match t {
                Type::String(ty) => {
                    if ty.enumeration.is_empty() {
                        Symbol::TypeAlias(TypeAlias {
                            name: name.clone(),
                            target: type_ref,
                            vis: Visibility::Public,
                            docs: String::default(),
                            attrs: Attrs::default(),
                        })
                    } else {
                        Symbol::Enum(Enum {
                            name: name.clone(),
                            generics: vec![],
                            representation: EnumRepr::Stringy,
                            variants: ty
                                .enumeration
                                .iter()
                                .filter_map(|var| {
                                    if let Some(var) = var {
                                        Variant {
                                            name: var.clone(),
                                            payload: VariantPayload::Unit,
                                            discriminant: None,
                                            docs: String::default(),
                                            attrs: Attrs::default(),
                                        }
                                        .into()
                                    } else {
                                        None
                                    }
                                })
                                .collect(),
                            vis: Visibility::Public,
                            docs: String::default(),
                            attrs: Attrs::default(),
                        })
                    }
                }
                Type::Number(ty) => Symbol::TypeAlias(TypeAlias {
                    name: name.clone(),
                    target: type_ref,
                    vis: Visibility::Public,
                    docs: String::default(),
                    attrs: Attrs::default(),
                }),
                Type::Integer(ty) => Symbol::TypeAlias(TypeAlias {
                    name: name.clone(),
                    target: type_ref,
                    vis: Visibility::Public,
                    docs: String::default(),
                    attrs: Attrs::default(),
                }),
                Type::Boolean(ty) => Symbol::TypeAlias(TypeAlias {
                    name: name.clone(),
                    target: type_ref,
                    vis: Visibility::Public,
                    docs: String::default(),
                    attrs: Attrs::default(),
                }),
                Type::Array(ty) => Symbol::TypeAlias(TypeAlias {
                    name: name.clone(),
                    target: type_ref,
                    vis: Visibility::Public,
                    docs: String::default(),
                    attrs: Attrs::default(),
                }),
                Type::Object(object_type) => {
                    let fields = object_type
                        .properties
                        .iter()
                        .map(|(name, ref_or_schema)| match ref_or_schema {
                            ReferenceOr::Item(schema) => Field {
                                name: name.clone(),
                                ty: TypeRef {
                                    kind: Self::to_type_kind(schema),
                                    nullable: schema.schema_data.nullable,
                                    optional: schema.schema_data.nullable,
                                    mutable: !schema.schema_data.read_only,
                                    attrs: Attrs::default(),
                                },
                                default: schema
                                    .schema_data
                                    .default
                                    .as_ref()
                                    .map(|val| Literal::String(val.to_string())),
                                mutable: !schema.schema_data.read_only,
                                optional: schema.schema_data.nullable,
                                attrs: Attrs::default(),
                                docs: String::default(),
                            },
                            ReferenceOr::Reference { reference } => Field {
                                name: name.clone(),
                                ty: TypeRef {
                                    kind: TypeKind::Named(QualifiedName(reference.clone()), vec![]),
                                    nullable: schema.schema_data.nullable,
                                    optional: schema.schema_data.nullable,
                                    mutable: !schema.schema_data.read_only,
                                    attrs: Attrs::default(),
                                },
                                default: schema
                                    .schema_data
                                    .default
                                    .as_ref()
                                    .map(|val| Literal::String(val.to_string())),
                                mutable: !schema.schema_data.read_only,
                                optional: schema.schema_data.nullable,
                                attrs: Attrs::default(),
                                docs: String::default(),
                            },
                        })
                        .collect();

                    Symbol::Struct(Struct {
                        name: name.clone(),
                        generics: vec![],
                        fields,
                        inherits: vec![],
                        implements: vec![],
                        vis: Visibility::Public,
                        docs: String::new(),
                        attrs: Attrs::default(),
                    })
                }
            },
            SchemaKind::OneOf { one_of: _ } => todo!("one of is not supported"),
            SchemaKind::AllOf { all_of: _ } => todo!("all of is not supported"),
            SchemaKind::AnyOf { any_of: _ } => todo!("any of is not supported"),
            SchemaKind::Not { not: _ } => todo!("not is not supported"),
            SchemaKind::Any(_) => Symbol::TypeAlias(TypeAlias {
                name: name.clone(),
                target: type_ref,
                vis: Visibility::Public,
                docs: String::default(),
                attrs: Attrs::default(),
            }),
        }
    }
}

impl Parser for OpenAPIV3Parser {
    fn parse(&self, input: &str) -> anyhow::Result<Module> {
        let spec: OpenAPI = if let Ok(spec) = serde_json::from_str(input) {
            spec
        } else if let Ok(spec) = serde_yaml::from_str(input) {
            spec
        } else {
            return Err(OpenAPIV3ParserError::FailedToParse.into());
        };

        let mut module = Module::new("main".into());

        if let Some(components) = spec.components {
            module.symbols.extend(
                components
                    .schemas
                    .iter()
                    .map(Self::to_symbol)
                    .collect::<Vec<_>>(),
            );
        }

        Ok(module)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use ir::{Primitive, Symbol, TypeKind};

    #[test]
    fn test_parse_openapi_spec_json_with_schema() {
        let spec_str = r#"
        {
            "openapi": "3.0.0",
            "info": {
                "title": "Test API",
                "version": "1.0.0"
            },
            "paths": {},
            "components": {
                "schemas": {
                    "Pet": {
                        "type": "object",
                        "properties": {
                            "id": {
                                "type": "integer"
                            },
                            "name": {
                                "type": "string"
                            }
                        },
                        "required": ["id", "name"]
                    }
                }
            }
        }
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser.parse(spec_str).expect("Should parse spec");

        assert_eq!(module.name, "main");
        assert_eq!(module.symbols.len(), 1);

        let Symbol::Struct(pet_type) = module.symbols.first().expect("Pet type found") else {
            panic!("Pet type should be a struct");
        };

        assert_eq!(&pet_type.name, "Pet");

        assert_eq!(pet_type.fields[0].name, "id");
        assert_eq!(
            pet_type.fields[0].ty.kind,
            TypeKind::Primitive(Primitive::I64)
        );
        assert_eq!(pet_type.fields[0].ty.optional, false);

        assert_eq!(pet_type.fields[1].name, "name");
        assert_eq!(
            pet_type.fields[1].ty.kind,
            TypeKind::Primitive(Primitive::String)
        );
        assert_eq!(pet_type.fields[1].ty.optional, false);
    }

    #[test]
    fn test_parse_openapi_spec_enum() {
        let spec_str = r#"
        {
            "openapi": "3.0.0",
            "info": {
                "title": "Test API",
                "version": "1.0.0"
            },
            "paths": {},
            "components": {
                "schemas": {
                    "PetType": {
                        "type": "string",
                        "enum": ["Dog", "Cat", "Bird"]
                    }
                }
            }
        }
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser.parse(spec_str).expect("Should parse enum spec");

        assert_eq!(module.name, "main");
        assert_eq!(module.symbols.len(), 1);

        let symbol = module.symbols.first().expect("Enum type found");
        let Symbol::Enum(enum_type) = symbol else {
            panic!("PetType should be an enum");
        };

        assert_eq!(&enum_type.name, "PetType");
        assert_eq!(enum_type.variants.len(), 3);

        assert_eq!(enum_type.variants[0].name, "Dog");
        assert_eq!(enum_type.variants[0].payload, VariantPayload::Unit);
        assert_eq!(enum_type.variants[1].name, "Cat");
        assert_eq!(enum_type.variants[1].payload, VariantPayload::Unit);
        assert_eq!(enum_type.variants[2].name, "Bird");
        assert_eq!(enum_type.variants[2].payload, VariantPayload::Unit);
    }

    #[test]
    fn test_parse_openapi_spec_yaml_with_schema() {
        let spec_str = r#"
        openapi: 3.0.0
        info:
          title: Test API
          version: 1.0.0
        paths: {}
        components:
          schemas:
            Pet:
              type: object
              properties:
                id:
                  type: integer
                name:
                  type: string
              required: [id, name]
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser.parse(spec_str).expect("Should parse spec");

        assert_eq!(module.name, "main");
        assert_eq!(module.symbols.len(), 1);

        let Symbol::Struct(pet_type) = module.symbols.first().expect("Pet type found") else {
            panic!("Pet type should be a struct");
        };

        assert_eq!(&pet_type.name, "Pet");
        assert_eq!(pet_type.fields.len(), 2);

        assert_eq!(pet_type.fields[0].name, "id");
        assert_eq!(
            pet_type.fields[0].ty.kind,
            TypeKind::Primitive(Primitive::I64)
        );
        assert_eq!(pet_type.fields[0].ty.optional, false);

        assert_eq!(pet_type.fields[1].name, "name");
        assert_eq!(
            pet_type.fields[1].ty.kind,
            TypeKind::Primitive(Primitive::String)
        );
        assert_eq!(pet_type.fields[1].ty.optional, false);
    }

    #[test]
    fn test_parse_openapi_spec_array_of_array_of_strings() {
        let spec_str = r#"
        openapi: 3.0.0
        info:
          title: Test API Array of Arrays
          version: 1.0.0
        paths: {}
        components:
          schemas:
            StringMatrix:
              type: array
              items:
                type: array
                items:
                  type: string
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser
            .parse(spec_str)
            .expect("Should parse array of array type spec");

        assert_eq!(module.name, "main");
        assert_eq!(module.symbols.len(), 1);

        let Symbol::TypeAlias(ty_alias) = module.symbols.first().expect("StringMatrix type found")
        else {
            panic!("Should find type alias for array of array of strings");
        };

        assert_eq!(&ty_alias.name, "StringMatrix");
        match &ty_alias.target.kind {
            TypeKind::Array(inner1) => match &inner1.kind {
                TypeKind::Array(inner2) => {
                    assert_eq!(inner2.kind, TypeKind::Primitive(Primitive::String));
                }
                _ => panic!("Expected nested array of strings"),
            },
            _ => panic!("Expected top level TypeKind::Array"),
        };
    }

    #[test]
    fn test_parse_openapi_spec_invalid() {
        let spec_str = "not a valid openapi spec";

        let parser = OpenAPIV3Parser;
        let result = parser.parse(spec_str);

        assert!(result.is_err());
    }
}
