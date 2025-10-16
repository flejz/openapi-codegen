use inflector::Inflector;
use ir::{Attrs, Effects, FnKind, Function, Interface, Symbol, Visibility};
use openapiv3::{PathItem, ReferenceOr};

use crate::builders::function_builder::FunctionBuilder;
pub struct InterfaceBuilder;

impl InterfaceBuilder {
    pub fn new() -> Self {
        Self
    }

    pub fn from_path(&self, (name, ref_or_path_item): (&String, &ReferenceOr<PathItem>)) -> Symbol {
        let function_builder = FunctionBuilder::new();
        let methods = match ref_or_path_item {
            ReferenceOr::Item(path_item) => {
                let default_effects = Effects {
                    is_async: true,
                    throws: vec![],
                    is_unsafe: false,
                    is_pure: None,
                };

                path_item
                    .iter()
                    .map(|(method, op)| {
                        let name = format!(
                            "{}{}",
                            match method {
                                "post" => "create",
                                "get" => "read",
                                "put" => "update",
                                method => method,
                            }
                            .to_pascal_case(),
                            name.to_pascal_case()
                        );

                        function_builder.from_operation(&name, op, &default_effects)
                    })
                    .collect::<Vec<_>>()
            }
            ReferenceOr::Reference { reference } => vec![Function {
                name: reference.clone().to_pascal_case(),
                generics: vec![],
                params: vec![],
                ret: vec![],
                vis: Visibility::Public,
                docs: String::default(),
                attrs: Attrs::default(),
                kind: FnKind::Instance,
                effects: Effects::default(),
                member_of: None,
            }],
        };

        Symbol::Interface(Interface {
            name: name.clone(),
            generics: vec![],
            methods,
            vis: Visibility::Public,
            docs: if let ReferenceOr::Item(path_item) = ref_or_path_item {
                path_item.description.clone().unwrap_or_default()
            } else {
                String::default()
            },
            attrs: Attrs::default(),
        })
    }
}
