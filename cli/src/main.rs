mod error;
mod parser;
mod serializer;
mod serializers;

use std::io::Read;

use clap::Parser;
use clio::Input;
use error::Error;
use error::Result;
use openapiv3::OpenAPI;
use openapiv3::ReferenceOr;

use crate::serializer::TypeSerializer;
use crate::serializers::golang::GolangTypeSerializer;

#[derive(Debug, Parser)]
struct Args {
    #[arg(value_parser, default_value = "-")]
    input: Input,

    #[arg(short, long)]
    output: Option<String>,

    #[arg(short, default_value = "false")]
    verbose: Option<bool>,

    #[arg(short, long, default_value = "false")]
    golang: bool,
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();

    let spec = parse_openapi_spec(&mut args.input)?;

    let mut type_serializers: Vec<Box<dyn TypeSerializer>> = vec![];

    if args.golang {
        type_serializers.push(Box::new(GolangTypeSerializer));
    }

    let components = spec.components.unwrap();

    let types = components
        .schemas
        .iter()
        .filter(|(name, _)| name.contains("Model"))
        .flat_map(|(name, schema)| match schema {
            ReferenceOr::Item(schema) => {
                let r#type = parser::Type::try_from(name, &schema);
                r#type
            }
            ReferenceOr::Reference { reference } => todo!("reference: {reference}"),
        })
        .collect::<Vec<_>>();

    for type_serializer in type_serializers {
        for r#type in &types {
            let serialized = type_serializer.serialize(r#type)?;
            println!("{}", serialized);
        }
    }

    Ok(())
}

fn parse_openapi_spec(input: &mut Input) -> Result<OpenAPI> {
    if let Some(true) = input.is_empty() {
        return Err(Error::InputFileInvalid);
    } else if input.is_empty().is_none() {
        return Err(Error::InputNotSpecified);
    }

    let mut buf = Vec::new();
    input.read_to_end(&mut buf)?;

    let data = String::from_utf8(buf)?;
    if let Ok(spec) = serde_json::from_str(&data) {
        Ok(spec)
    } else if let Ok(spec) = serde_yaml::from_str(&data) {
        Ok(spec)
    } else {
        Err(Error::FailedToParseOpenAPISpec)
    }
}
