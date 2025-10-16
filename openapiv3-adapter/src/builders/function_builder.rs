use inflector::Inflector;
use ir::{
    Attrs, Effects, FnKind, Function, Param, Primitive, QualifiedName, TypeKind, TypeRef,
    Visibility,
};
use openapiv3::{
    MediaType, Operation, Parameter, ParameterSchemaOrContent, ReferenceOr, RequestBody,
};

use crate::builders::schema_utils::to_type_kind;
pub struct FunctionBuilder;

impl FunctionBuilder {
    pub fn new() -> Self {
        Self
    }

    fn from_media_type(&self, media_type: &MediaType) -> TypeKind {
        match &media_type.schema {
            Some(ReferenceOr::Item(schema)) => to_type_kind(schema),
            Some(ReferenceOr::Reference { reference }) => {
                TypeKind::Named(QualifiedName(reference.clone()), vec![])
            }
            None => TypeKind::Primitive(Primitive::Any),
        }
    }

    fn from_parameter(&self, param: &ReferenceOr<Parameter>) -> Param {
        match param {
            ReferenceOr::Item(param) => {
                let param = param.parameter_data_ref();
                let ty = match &param.format {
                    ParameterSchemaOrContent::Schema(ref_or_schema) => match ref_or_schema {
                        ReferenceOr::Item(schema) => to_type_kind(schema),
                        ReferenceOr::Reference { reference } => {
                            TypeKind::Named(QualifiedName(reference.clone()), vec![])
                        }
                    },
                    ParameterSchemaOrContent::Content(content) => TypeKind::Tuple(
                        content
                            .iter()
                            .map(|(_, media_type)| self.from_media_type(media_type))
                            .map(|ty| TypeRef {
                                kind: ty,
                                nullable: false,
                                optional: !param.required,
                                mutable: false,
                                attrs: Attrs::default(),
                            })
                            .collect::<Vec<_>>(),
                    ),
                };

                Param {
                    name: param.name.clone(),
                    ty: TypeRef {
                        kind: ty,
                        nullable: false,
                        optional: !param.required,
                        mutable: false,
                        attrs: Attrs::default(),
                    },
                    by_ref: false,
                    mutable: false,
                    default: None,
                    attrs: Attrs::default(),
                    docs: param.example.as_ref().unwrap_or_default().to_string(),
                }
            }
            ReferenceOr::Reference { reference } => Param {
                name: reference.clone(),
                ty: TypeRef {
                    kind: TypeKind::Primitive(Primitive::String),
                    nullable: false,
                    optional: false,
                    mutable: false,
                    attrs: Attrs::default(),
                },
                by_ref: false,
                mutable: false,
                default: None,
                attrs: Attrs::default(),
                docs: String::default(),
            },
        }
    }

    fn from_request_body(&self, request_body: &ReferenceOr<RequestBody>) -> Param {
        let (name, ty) = match request_body {
            ReferenceOr::Item(request_body) => match request_body.content.len() {
                0 => (String::new(), TypeKind::Primitive(Primitive::Any)),
                1 => request_body
                    .content
                    .first()
                    .map(|(name, media_type)| (name.clone(), self.from_media_type(media_type)))
                    .unwrap(),
                _ => (
                    request_body.content.first().unwrap().0.clone(),
                    TypeKind::Tuple(
                        request_body
                            .content
                            .iter()
                            .map(|(_, media_type)| self.from_media_type(media_type))
                            .map(|ty| TypeRef {
                                kind: ty,
                                nullable: false,
                                optional: !request_body.required,
                                mutable: false,
                                attrs: Attrs::default(),
                            })
                            .collect::<Vec<_>>(),
                    ),
                ),
            },
            ReferenceOr::Reference { reference } => (
                reference.clone(),
                TypeKind::Named(QualifiedName(reference.clone()), vec![]),
            ),
        };

        Param {
            name: name.to_pascal_case(),
            ty: TypeRef {
                kind: ty,
                nullable: false,
                optional: if let ReferenceOr::Item(request_body) = request_body {
                    !request_body.required
                } else {
                    false
                },
                mutable: false,
                attrs: Attrs::default(),
            },
            by_ref: false,
            mutable: false,
            default: None,
            attrs: Attrs::default(),
            docs: if let ReferenceOr::Item(request_body) = request_body {
                request_body.description.clone().unwrap_or_default()
            } else {
                String::new()
            },
        }
    }

    pub fn from_operation(
        &self,
        name: &String,
        op: &Operation,
        default_effects: &Effects,
    ) -> Function {
        let mut params = op
            .parameters
            .iter()
            .map(|param| self.from_parameter(param))
            .collect::<Vec<_>>();

        if let Some(request_body) = &op.request_body {
            params.push(self.from_request_body(request_body));
        }

        Function {
            name: name.clone(),
            generics: vec![],
            params,
            ret: vec![],
            vis: Visibility::Public,
            docs: op.summary.clone().unwrap_or_default(),
            attrs: Attrs::default(),
            kind: FnKind::Instance,
            effects: default_effects.clone(),
            member_of: None,
        }
    }
}
