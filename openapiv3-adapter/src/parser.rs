use std::collections::BTreeMap;

use inflector::Inflector;
use ir::{
    AttrValue, Attrs, Const, Effects, Enum, EnumRepr, Field, FnKind, Function, Interface, Literal,
    Module, Param, Parser, Primitive, QualifiedName, Struct, Symbol, TypeAlias, TypeKind, TypeRef,
    Variant, VariantPayload, Visibility,
};
use openapiv3::{
    OpenAPI, Operation, Parameter, ParameterSchemaOrContent, PathItem, ReferenceOr, Schema,
    SchemaKind, Server, Type,
};
use regex::Regex;

use crate::builders::interface_builder::InterfaceBuilder;

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

    fn from_ref_or_schema((name, ref_or_schema): (&String, &ReferenceOr<Schema>)) -> Symbol {
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

    fn from_server(server: &Server) -> Symbol {
        let regex = Regex::new(
            r"^(?<protocol>https?://)?(?:[^@/\n]+@)?(?<cname>www\.)?(?<domain>[^:/\n]+)",
        )
        .unwrap();

        let name = match regex.captures(&server.url) {
            Some(capture) if !capture["domain"].is_empty() => capture["domain"].to_string(),
            _ => server.url.clone(),
        }
        .to_pascal_case();

        let Some(variables) = &server.variables else {
            return Symbol::Const(Const {
                name,
                ty: TypeRef {
                    kind: TypeKind::Primitive(Primitive::String),
                    nullable: false,
                    optional: false,
                    mutable: false,
                    attrs: Attrs::default(),
                },
                value: Literal::String(server.url.clone()),
                vis: Visibility::Public,
                docs: server.description.clone().unwrap_or_default(),
                attrs: Attrs::default(),
            });
        };

        Symbol::Enum(Enum {
            name: name.clone(),
            generics: vec![],
            representation: EnumRepr::Stringy,
            variants: variables
                .iter()
                .flat_map(|(var_name, variable)| {
                    variable.enumeration.iter().map(|value| Variant {
                        name: format!(
                            "{name}{}{}",
                            var_name.to_pascal_case(),
                            value.to_pascal_case()
                        ),
                        payload: VariantPayload::Unit,
                        discriminant: None,
                        docs: String::default(),
                        attrs: if value == &variable.default {
                            Attrs::from([("default".to_string(), AttrValue::String(value.clone()))])
                        } else {
                            Attrs::default()
                        },
                    })
                })
                .collect::<Vec<_>>(),
            vis: Visibility::Public,
            docs: String::default(),
            attrs: Attrs::default(),
        })
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

        let mut module = Module::new("main".into()); // TODO: get name from spec
        let interface_builder = InterfaceBuilder::new();

        // parse severs
        module.symbols.extend(
            spec.servers
                .iter()
                .map(Self::from_server)
                .collect::<Vec<_>>(),
        );

        // parse paths
        module.symbols.extend(
            spec.paths
                .iter()
                .map(|(name, ref_or_path_item)| {
                    interface_builder.from_path((name, ref_or_path_item))
                })
                .collect::<Vec<_>>(),
        );

        // parse schemas
        if let Some(components) = spec.components {
            module.symbols.extend(
                components
                    .schemas
                    .iter()
                    .map(Self::from_ref_or_schema)
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

    #[test]
    fn test_parse_openapi_spec_servers_into_enum() {
        let spec_str = r#"
        openapi: 3.0.0
        info:
          title: Api Staging
          version: 1.0.0
        paths: {}
        servers:
          - url: https://api.staging.com/{version}
            description: Staging server
            variables:
              version:
                default: v1
                enum:
                  - v1
                  - v2
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser
            .parse(spec_str)
            .expect("Should parse spec with servers");

        assert_eq!(module.symbols.len(), 1);
        let Symbol::Enum(server_enum) = module
            .symbols
            .first()
            .expect("Server enum should be generated")
        else {
            panic!("Server enum should be a enum");
        };
        assert_eq!(&server_enum.name, "ApiStagingCom");
        assert_eq!(server_enum.variants.len(), 2);
        assert_eq!(server_enum.variants[0].name, "ApiStagingComVersionV1");
        assert_eq!(server_enum.variants[0].payload, VariantPayload::Unit);
        assert_eq!(server_enum.variants[1].name, "ApiStagingComVersionV2");
        assert_eq!(server_enum.variants[1].payload, VariantPayload::Unit);
    }

    #[test]
    fn test_parse_openapi_spec_servers_into_const() {
        let spec_str = r#"
        openapi: 3.0.0
        info:
          title: Server Enum API
          version: 1.0.0
        paths: {}
        servers:
          - url: https://api.production.com/v1
            description: Production server
          - url: https://api.staging.com/v1
            description: Staging server
        "#;

        let parser = OpenAPIV3Parser;
        let module = parser
            .parse(spec_str)
            .expect("Should parse spec with servers");

        assert_eq!(module.symbols.len(), 2);

        let Symbol::Const(server_const) = module
            .symbols
            .first()
            .expect("Server const should be generated")
        else {
            panic!("Server const should be a const");
        };

        assert_eq!(&server_const.name, "ApiProductionCom");
        assert_eq!(&server_const.docs, "Production server");
        assert_eq!(
            server_const.value,
            Literal::String("https://api.production.com/v1".to_string()),
        );

        let Symbol::Const(server_const) = module
            .symbols
            .last()
            .expect("Server const should be generated")
        else {
            panic!("Server const should be a const");
        };

        assert_eq!(&server_const.name, "ApiStagingCom");
        assert_eq!(&server_const.docs, "Staging server");
        assert_eq!(
            server_const.value,
            Literal::String("https://api.staging.com/v1".to_string()),
        );
    }

    #[test]
    fn test_parse_openapi_spec_functions_from_paths_with_query_and_schema_ref() {
        let spec_str = r###"
        openapi: 3.0.0
        info:
          title: Function API with Query Params and Schema Ref
          version: 1.0.0
        paths:
          /article:
            get:
              operationId: getArticle
              summary: Returns an article.
              parameters:
                - in: query
                  name: lang
                  schema:
                    type: string
                  required: false
                  description: Language of the message.
                - in: query
                  name: count
                  schema:
                    type: integer
                  required: false
                  description: Number of messages to retrieve
              responses:
                "200":
                  description: Successful response
                  content:
                    application/json:
                      schema:
                        type: string
            post:
              operationId: createArticle
              summary: Creates an article.
              requestBody:
                description: Article to be created
                required: true
                content:
                  application/json:
                    schema:
                      type: string
              responses:
                "201":
                  description: Created
                  content:
                    application/json:
                      schema:
                        type: string
          /user:
            get:
              operationId: getUserById
              summary: Returns user object.
              parameters:
                - in: query
                  name: user_id
                  schema:
                    type: string
                  required: true
                  description: ID of the user
              responses:
                "200":
                  description: User found
                  content:
                    application/json:
                      schema:
                        $ref: "#/components/schemas/User"
        components:
          schemas:
            User:
              type: object
              properties:
                id:
                  type: string
                name:
                  type: string
                age:
                  type: integer
              required: [id, name]
        "###;

        let parser = OpenAPIV3Parser;
        let module = parser
            .parse(spec_str)
            .expect("Should parse function API spec with query params and schema ref");

        // article
        let Some(Symbol::Interface(interface)) = &module.symbols.get(0) else {
            panic!("Interface should be generated");
        };

        assert_eq!(interface.methods.len(), 2);
        assert_eq!(interface.methods[0].name, "ReadArticle");
        assert_eq!(interface.methods[1].name, "CreateArticle");

        let method = &interface.methods[0];
        assert_eq!(method.docs, "Returns an article.");
        assert_eq!(method.params.len(), 2);

        let param_lang = method.params.get(0).expect("lang query param");
        assert_eq!(param_lang.ty.kind, TypeKind::Primitive(Primitive::String));

        let param_count = method.params.get(1).expect("count query param");
        assert_eq!(param_count.ty.kind, TypeKind::Primitive(Primitive::I64));

        let method = &interface.methods[1];
        assert_eq!(method.docs, "Creates an article.");
        assert_eq!(method.params.len(), 1);

        let param_article = method.params.get(0).expect("article param");
        assert_eq!(
            param_article.ty.kind,
            TypeKind::Primitive(Primitive::String)
        );

        // user
        let Some(Symbol::Interface(interface)) = &module.symbols.get(1) else {
            panic!("Interface should be generated");
        };

        assert_eq!(interface.methods.len(), 1);
        assert_eq!(interface.methods[0].name, "ReadUser");

        let method = &interface.methods[0];
        assert_eq!(method.docs, "Returns user object.");
        assert_eq!(method.params.len(), 1);

        let param_user_id = method.params.get(0).expect("user_id param");
        assert_eq!(
            param_user_id.ty.kind,
            TypeKind::Primitive(Primitive::String)
        );
    }
}
