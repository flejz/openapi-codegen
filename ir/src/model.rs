use std::collections::{BTreeMap, BTreeSet};

/// Free-form attributes/metadata for per-language knobs.
pub type Attrs = BTreeMap<String, AttrValue>;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(untagged)]
pub enum AttrValue {
    Str(String),
    Num(f64),
    Bool(bool),
    Seq(Vec<AttrValue>),
    Map(BTreeMap<String, AttrValue>),
}

/// Fully-qualified reference to a symbol: name.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct QualifiedName(pub String);

/// A module is a namespace + container of symbols.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Module {
    pub name: String,         // e.g., "users"
    pub symbols: Vec<Symbol>, // types, fns, etc.
    pub attrs: Attrs,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            symbols: vec![],
            attrs: Attrs::default(),
        }
    }
}

/// Any top-level declaration in a module.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(tag = "kind")]
pub enum Symbol {
    Struct(Struct),
    Enum(Enum),
    Interface(Interface),
    Function(Function),
    TypeAlias(TypeAlias),
    Const(Const),
}

/// Record/object type with named fields (and optional methods).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<GenericType>,
    pub fields: Vec<Field>,
    pub inherits: Vec<TypeRef>, // OO inheritance (avoid multiple unless needed)
    pub implements: Vec<TypeRef>, // traits/interfaces
    pub vis: Visibility,
    pub docs: String,
    pub attrs: Attrs,
}

/// Sum type with variants and a representation hint (for wire/encoding).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<GenericType>,
    pub representation: EnumRepr,
    pub variants: Vec<Variant>,
    pub vis: Visibility,
    pub docs: String,
    pub attrs: Attrs,
}

/// Method-only contract (no fields/impls).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<GenericType>,
    pub methods: Vec<Function>, // signatures only
    pub vis: Visibility,
    pub docs: String,
    pub attrs: Attrs,
}

/// Free or member function signature.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Function {
    pub name: String,
    pub generics: Vec<GenericType>,
    pub params: Vec<Param>,
    pub ret: TypeRef,
    pub vis: Visibility,
    pub member_of: Option<QualifiedName>, // if a method, which type
    pub kind: FnKind,                     // Free | Static | Instance | Constructor
    pub effects: Effects,                 // async/throws/unsafe/pure
    pub docs: String,
    pub attrs: Attrs,
}

/// A named field in a struct or struct-like variant.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: TypeRef,
    pub mutable: bool,
    pub optional: bool, // may be **absent** (serialization concern)
    pub default: Option<Literal>,
    pub docs: String,
    pub attrs: Attrs,
}

/// One case of an enum.
#[derive(Clone, Debug, Default, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Variant {
    pub name: String,
    pub payload: VariantPayload,   // Unit | Tuple | Struct
    pub discriminant: Option<i64>, // for numbered enums
    pub docs: String,
    pub attrs: Attrs,
}

#[derive(Clone, Debug, Default, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(tag = "payload")]
pub enum VariantPayload {
    #[default]
    Unit,
    Tuple {
        elems: Vec<TypeRef>,
    },
    Struct {
        fields: Vec<Field>,
    },
}

/// A simple alias for an existing type.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct TypeAlias {
    pub name: String,
    pub target: TypeRef,
    pub vis: Visibility,
    pub docs: String,
    pub attrs: Attrs,
}

/// A named constant.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Const {
    pub name: String,
    pub ty: TypeRef,
    pub value: Literal,
    pub vis: Visibility,
    pub docs: String,
    pub attrs: Attrs,
}

/// A function parameter.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeRef,
    pub by_ref: bool, // hint for ref/borrow/out
    pub mutable: bool,
    pub default: Option<Literal>,
    pub attrs: Attrs,
    pub docs: String,
}

/// Generic type parameter (with optional bounds and variance).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct GenericType {
    pub name: String,
    pub bounds: Vec<TypeBound>,
    pub variance: Variance,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub enum TypeBound {
    Implements(TypeRef), // aka "extends" in OO
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub enum Variance {
    Invariant,
    Covariant,
    Contravariant,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub enum Visibility {
    Public,
    Package,
    Private,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq, Default)]
pub struct Effects {
    pub is_async: bool,
    pub throws: Vec<TypeRef>,
    pub is_unsafe: bool,
    pub is_pure: Option<bool>,
}

/// A *use* of a type (not a definition).
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct TypeRef {
    pub kind: TypeKind,
    pub nullable: bool, // present but can be null
    pub optional: bool, // may be absent entirely
    pub mutable: bool,  // qualifier hint
    pub attrs: Attrs,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(tag = "kind", content = "data")]
pub enum TypeKind {
    Primitive(Primitive),
    Named(QualifiedName, Vec<TypeRef>), // generic instantiation
    Array(Box<TypeRef>),
    Map(Box<TypeRef>, Box<TypeRef>),
    Tuple(Vec<TypeRef>),
    Union(Vec<TypeRef>),
}

/// Portable scalar/core set.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Primitive {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    String,
    Bytes,
    Decimal,
    Date,
    Time,
    DateTime,
    Duration,
    Uuid,
    Object,
    Any,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
#[serde(tag = "lit", content = "value")]
pub enum Literal {
    Bool(bool),
    Integer(i128),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    Null,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub enum EnumRepr {
    Tagged { tag: String },
    Numbered,
    Stringy,
    External,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq)]
pub enum FnKind {
    Free,
    Static,
    Instance,
    Constructor,
}
