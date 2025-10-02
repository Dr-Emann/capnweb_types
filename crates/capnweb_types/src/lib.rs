//! Rust type definitions for the [Cap'n Web RPC protocol][capnweb]
//!
//! This crate provies rust types for the low-level Cap'n Web protocol, which can be serialized and
//! deserialized using Serde into the JSON format used by the protocol.
//!
//! The top-level type is [`Message`], which represents a single message in the protocol.
//! The main type used within messages is [`Expression`], which describes values and references
//!
//! [capnweb]: https://github.com/cloudflare/capnweb
#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
#![deny(unnameable_types)]
#![deny(unsafe_code)]
#![warn(unsafe_op_in_unsafe_fn)]
#![warn(variant_size_differences)]

use serde::de::{SeqAccess, Unexpected};
use serde::{Deserializer, Serializer};
use serde_core as serde;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Formatter;

type Str<'a> = Cow<'a, str>;

/// An Identifier for imports and exports
///
/// Each side of an RPC session maintains two tables: imports and exports.
/// One side's exports correspond to the other side's imports.
/// Imports and exports are assigned sequential numeric IDs.
/// However, in some cases an ID needs to be chosen by the importing side,
/// and in some cases by the exporting side. In order to avoid conflicts:
///
/// * When the importing side chooses the ID, it chooses the next positive ID
///   (starting from 1 and going up).
/// * When the exporting side chooses the ID, it chooses the next negative ID
///   (starting from -1 and going down).
/// * ID zero is automatically assigned to the "main" interface.
///
/// To be more specific:
///
/// * The importing side chooses the ID when it initiates a call: the ID
///   represents the result of the call.
/// * The exporting side chooses the ID when it sends a message containing
///   a stub: the ID represents the target of the stub.
pub type Id = i64;

/// A segment in a property path, either a property name or an array index.
#[derive(Debug)]
pub enum PathSegment<'a> {
    /// A property name
    Name(Str<'a>),
    /// An array index
    Index(u64),
}

impl serde::Serialize for PathSegment<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            PathSegment::Name(name) => serializer.serialize_str(name),
            PathSegment::Index(index) => serializer.serialize_u64(*index),
        }
    }
}

impl<'de> serde::Deserialize<'de> for PathSegment<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = PathSegment<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a string or a number")
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(PathSegment::Index(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_string(String::from(v))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(PathSegment::Name(Cow::Borrowed(v)))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(PathSegment::Name(Cow::Owned(v)))
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

/// A list of property names (strings or numbers) leading to a specific property of the import's target
pub type Path<'a> = Box<[PathSegment<'a>]>;

/// The type of Resolution for an import or export
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Resolution {
    /// A promise must be replaced by their resolution before being delivered to the application
    Promise,
    /// A stub will be delivered as a stub to the application without waiting for resolution
    Stub,
}

/// An operation to perform on an import
#[derive(Debug)]
pub enum ImportOperation<'a> {
    /// Access a (possibly nested) property on an import
    Prop {
        /// The path to the property
        path: Path<'a>,
    },
    /// Call a (possibly nested) method on an import
    Call {
        /// The path to the property to call
        path: Path<'a>,
        /// The arguments to the call
        args: Box<[Expression<'a>]>,
    },
}

/// A JavaScript Error value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<'a> {
    /// The name of the error type (e.g. "TypeError")
    ///
    /// This _should_ be one of a small set of "well-known" error types
    pub ty: Str<'a>,

    /// A string containing the error message
    pub message: Str<'a>,

    /// An optional string containing the stack trace
    ///
    /// This should often be omitted, as it may contain sensitive information
    pub stack: Option<Str<'a>>,
}

/// A `map()` operation on an import
#[derive(Debug)]
pub struct Remap<'a> {
    /// The ID of the import being remapped
    pub import_id: Id,
    /// The path to the property of the import being remapped
    pub property_path: Path<'a>,
    /// The captured imports/exports used in the remap instructions
    pub captures: Box<[Capture]>,
    /// A list of expressions to apply to each item of the import being remapped
    ///
    /// Expressions which reference imports and exports have special semantics:
    ///
    /// For the purpose of the instructions in a mapper, there is no export table.
    /// The import table, meanwhile, is defined as follows:
    /// * Negative values refer to the `captures` list, starting from -1. So, -1
    ///   is `captures[0]`, -2 is `captures[1]`, and so on.
    /// * Zero refers to the input value of the map function.
    /// * Positive values refer to the results of previous instructions,
    ///   starting from 1. So, 1 is the result of evaluating `instructions[0]`,
    ///   2 is the result of evaluating `instructions[1]`, and so on.
    ///
    /// The instructions are always evaluated in order. Each instruction may only import
    /// results of instructions that came before it. The last instruction evaluates to
    /// the return value of the map function.
    pub instructions: Box<[Expression<'a>]>,
}

/// The main expression type, representing values and references
#[derive(Debug)]
pub enum Expression<'a> {
    /// JSON null
    Null,
    /// A 64-bit signed integer
    Int(i64),
    /// A 64-bit floating point number
    Float(f64),
    /// A UTF-8 string
    String(Str<'a>),
    /// A boolean
    Bool(bool),
    /// A JSON object, with Expression values
    Object(BTreeMap<Str<'a>, Expression<'a>>),
    /// A JSON array, with Expression values
    Array(Box<[Expression<'a>]>),

    /// A Javascript Error value
    Error(Box<Error<'a>>),

    /// A JavaScript Date value, represented as a number of milliseconds since the Unix epoch
    Date(f64),

    // The following are not (yet?) documented in the protocol, is it safe to rely on them?
    /// A byte array, encoded as a base64 string
    Bytes(Str<'a>),
    /// A BigInt value, encoded as a string
    BigInt(Str<'a>),
    /// The JavaScript `undefined` value
    Undefined,

    /// Reference an imported value, possibly performing actions on it
    Import {
        /// The ID of the import being referenced
        id: Id,
        /// An optional operation to perform on the import
        operation: Option<ImportOperation<'a>>,
        /// The type of resolution (promise or stub)
        resolution: Resolution,
    },

    /// A `map()` operation on an import
    Remap(Box<Remap<'a>>),

    /// The sender is exporting a new stub (or re-exporting a stub that was exported before)
    ///
    /// The expression evaluates to a stub or promise
    ///
    /// If this resolves to a promise, the id should always be a newly allocated ID, and
    /// The sender will proactively send a "resolve" (or "reject") message for this ID when the
    /// promise resolves (unless it is released first). The recipient does not need to "pull" the
    /// promise explicitly; it is assumed that the recipient always wants the resolution.
    Export {
        /// The ID of the export
        id: Id,
        /// The type of resolution (promise or stub)
        resolution: Resolution,
    },
}

/// An import or export captured for use in a [`Expression::Remap`] instruction
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Capture {
    /// The type of the captured import or export
    pub ty: CaptureType,
    /// The ID of the captured import or export
    pub id: Id,
}

/// The type of a captured import or export for a [`Expression::Remap`] instruction
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CaptureType {
    /// Capture an import for the remap instructions
    Import,
    /// Capture an export for the remap instructions
    Export,
}

impl serde::Serialize for CaptureType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            CaptureType::Import => serializer.serialize_str("import"),
            CaptureType::Export => serializer.serialize_str("export"),
        }
    }
}

impl<'de> serde::Deserialize<'de> for CaptureType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = CaptureType;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("import or export string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde_core::de::Error,
            {
                match v {
                    "import" => Ok(CaptureType::Import),
                    "export" => Ok(CaptureType::Export),
                    other => Err(E::invalid_value(Unexpected::Str(other), &self)),
                }
            }
        }
        deserializer.deserialize_str(Visitor)
    }
}

impl serde::Serialize for Capture {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (self.ty, self.id).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Capture {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (ty, id) = <(CaptureType, Id)>::deserialize(deserializer)?;
        Ok(Capture { ty, id })
    }
}

impl serde::Serialize for Expression<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Expression::Null => serializer.serialize_unit(),
            Expression::Int(i) => serializer.serialize_i64(*i),
            Expression::Float(f) => serializer.serialize_f64(*f),
            Expression::String(s) => serializer.serialize_str(s),
            Expression::Bool(b) => serializer.serialize_bool(*b),
            Expression::Object(o) => o.serialize(serializer),

            Expression::Error(e) => {
                if let Some(stack) = e.stack.as_deref() {
                    ("error", &*e.ty, &*e.message, stack).serialize(serializer)
                } else {
                    ("error", &*e.ty, &*e.message).serialize(serializer)
                }
            }
            Expression::Date(i) => ("date", i).serialize(serializer),
            Expression::Bytes(data) => ("bytes", data).serialize(serializer),
            Expression::Undefined => ("undefined",).serialize(serializer),
            Expression::BigInt(data) => ("bigint", data).serialize(serializer),

            Expression::Import {
                resolution,
                id,
                operation,
            } => {
                let ty_str = match resolution {
                    Resolution::Promise => "pipeline",
                    Resolution::Stub => "import",
                };
                match operation {
                    None => (ty_str, id).serialize(serializer),
                    Some(ImportOperation::Prop { path }) => {
                        (ty_str, id, path).serialize(serializer)
                    }
                    Some(ImportOperation::Call { path, args }) => {
                        (ty_str, id, path, args).serialize(serializer)
                    }
                }
            }
            Expression::Remap(remap) => {
                let value = (
                    "remap",
                    remap.import_id,
                    &*remap.property_path,
                    &*remap.captures,
                    &*remap.instructions,
                );
                value.serialize(serializer)
            }
            Expression::Export { resolution, id } => {
                let ty_str = match resolution {
                    Resolution::Promise => "promise",
                    Resolution::Stub => "export",
                };
                (ty_str, id).serialize(serializer)
            }
            Expression::Array(inner) => (inner,).serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for Expression<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Expression<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Cap'n Web expression")
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::Bool(v))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::Int(v))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if let Ok(v) = i64::try_from(v) {
                    Ok(Expression::Int(v))
                } else {
                    Err(E::invalid_value(
                        serde::de::Unexpected::Unsigned(v),
                        &"a 64 bit signed integer",
                    ))
                }
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::Float(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_string(String::from(v))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::String(Cow::Borrowed(v)))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::String(Cow::Owned(v)))
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Expression::Null)
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut obj = BTreeMap::new();
                while let Some((key, value)) = map.next_entry()? {
                    obj.insert(key, value);
                }
                Ok(Expression::Object(obj))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let first_value: StringOrArray = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
                let kind = match first_value {
                    StringOrArray::String(kind) => kind,
                    StringOrArray::Array(arr) => {
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(2, &self));
                        }
                        return Ok(Expression::Array(arr));
                    }
                };
                match &*kind {
                    "date" => {
                        let expected = "a date expression";
                        let timestamp: f64 = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Expression::Date(timestamp))
                    }
                    "error" => {
                        let expected = "an error expression";
                        let ty: Str<'de> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let message: Str<'de> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(2, &expected))?;
                        let stack: Option<Str<'de>> = seq.next_element()?;
                        // Intentionally allow extra elements, for forward compatibility
                        Ok(Expression::Error(Box::new(Error { ty, message, stack })))
                    }
                    "undefined" => {
                        let expected = "an undefined expression";
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(2, &expected));
                        }
                        Ok(Expression::Undefined)
                    }
                    "bytes" => {
                        let expected = "a bytes expression";
                        let data: Str<'de> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Expression::Bytes(data))
                    }
                    "bigint" => {
                        let expected = "a bigint expression";
                        let data: Str<'de> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Expression::BigInt(data))
                    }
                    "import" | "pipeline" => {
                        let expected = "an import or pipeline expression";
                        let resolution = if kind == "import" {
                            Resolution::Stub
                        } else {
                            Resolution::Promise
                        };
                        let id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let operation = if let Some(path) = seq.next_element::<Path<'de>>()? {
                            if let Some(args) = seq.next_element::<Box<[Expression<'de>]>>()? {
                                if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                                    return Err(serde::de::Error::invalid_length(4, &expected));
                                }
                                Some(ImportOperation::Call { path, args })
                            } else {
                                Some(ImportOperation::Prop { path })
                            }
                        } else {
                            None
                        };
                        Ok(Expression::Import {
                            id,
                            operation,
                            resolution,
                        })
                    }
                    "remap" => {
                        let expected = "a remap expression";
                        let import_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let property_path: Path<'de> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(2, &expected))?;
                        let captures: Box<[Capture]> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(3, &expected))?;
                        let instructions: Box<[Expression<'de>]> = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(4, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(5, &"a remap expression"));
                        }
                        Ok(Expression::Remap(Box::new(Remap {
                            import_id,
                            property_path,
                            captures,
                            instructions,
                        })))
                    }
                    "export" | "promise" => {
                        let expected = "an export or promise expression";
                        let resolution = if kind == "export" {
                            Resolution::Stub
                        } else {
                            Resolution::Promise
                        };
                        let id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Expression::Export { id, resolution })
                    }
                    other_kind => Err(serde::de::Error::invalid_value(
                        Unexpected::Str(other_kind),
                        &"a valid Cap'n Web expression kind",
                    )),
                }
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

/// A top-level message in the Cap'n Web protocol
#[derive(Debug)]
pub enum Message<'a> {
    /// Asks the recipient to evaluate the given expression
    ///
    /// The expression is implicitly assigned the next sequential import ID
    /// (in the positive direction). The recipient will evaluate the expression, delivering
    /// any calls therein to the application. The final result can be pulled, or used in
    /// promise pipelining.
    Push(Expression<'a>),
    /// Signals that the sender would like to receive a "resolve" message for the resolution
    /// of the given import, which must refer to a promise.
    ///
    /// This is normally only used for imports created by a "push", as exported promises
    /// are pulled automatically.
    Pull(Id),
    /// Instructs the recipient to evaluate the given expression and use it as the resolution of
    /// the given promise export
    Resolve {
        /// The ID of the export to resolve
        export_id: Id,
        /// The value to resolve the promise to
        value: Expression<'a>,
    },
    /// Instructs the recipient to evaluate the given expression and then use it to reject the
    /// given promise export.
    ///
    /// The expression is not permitted to contain stubs. It typically evaluates to an `Error`,
    /// although technically JavaScript does not require that thrown values are `Error`s.
    Reject {
        /// The ID of the export to reject
        export_id: Id,
        /// The value to reject the promise with
        ///
        /// Usually evaluates to an [`Expression::Error`].
        value: Expression<'a>,
    },
    /// Instructs the recipient to release the given entry in the import table, disposing whatever
    /// it is connected to.
    ///
    /// If the import is a promise, the recipient is no longer obliged to send a "resolve" message
    /// for it, though it is still permitted to do so.
    Release {
        /// The ID of the import to release
        import_id: Id,
        /// The total number of times this import ID has been "introduced"
        ///
        /// i.e. the number of times it has been the subject of an "export" or "promise"
        /// expression, plus 1 if it was created by a "push". The refcount must be sent to avoid
        /// a race condition if the receiving side has recently exported the same ID again.
        /// The exporter remembers how many times they have exported this ID, decrementing it by
        /// the refcount of any release messages received, and only actually releases the ID when
        /// this count reaches zero.
        refcount: u64,
    },
    /// Indicates that the sender has experienced an error causing it to terminate the session
    ///
    /// The expression evaluates to the error which caused the abort. No further messages
    /// will be sent nor received.
    Abort(Expression<'a>),
}

impl serde::Serialize for Message<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Message::Push(expr) => ("push", expr).serialize(serializer),
            Message::Pull(id) => ("pull", id).serialize(serializer),
            Message::Resolve { export_id, value } => {
                ("resolve", export_id, value).serialize(serializer)
            }
            Message::Reject { export_id, value } => {
                ("reject", export_id, value).serialize(serializer)
            }
            Message::Release {
                import_id,
                refcount,
            } => ("release", import_id, refcount).serialize(serializer),
            Message::Abort(expr) => ("abort", expr).serialize(serializer),
        }
    }
}

impl<'de> serde::de::Deserialize<'de> for Message<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Message<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Cap'n Web top-level message")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let kind: Str<'de> = seq.next_element()?.ok_or_else(|| {
                    serde::de::Error::invalid_length(1, &"an array starting with a string")
                })?;
                match &*kind {
                    "push" => {
                        let expected = "a push message";
                        let expr: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Push(expr))
                    }
                    "pull" => {
                        let expected = "a pull message";
                        let id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Pull(id))
                    }
                    "resolve" => {
                        let expected = "a resolve message";
                        let export_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let value: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(4, &expected));
                        }
                        Ok(Message::Resolve { export_id, value })
                    }
                    "reject" => {
                        let expected = "a reject message";
                        let export_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let value: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(4, &expected));
                        }
                        Ok(Message::Reject { export_id, value })
                    }
                    "release" => {
                        let expected = "a release message";
                        let import_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        let refcount: u64 = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(4, &expected));
                        }
                        Ok(Message::Release {
                            import_id,
                            refcount,
                        })
                    }
                    "abort" => {
                        let expected = "an abort message";
                        let expr: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde::de::IgnoredAny>()?.is_some() {
                            return Err(serde::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Abort(expr))
                    }
                    other_kind => Err(serde::de::Error::invalid_value(
                        Unexpected::Str(other_kind),
                        &"a valid Cap'n Web top-level message kind",
                    )),
                }
            }
        }

        deserializer.deserialize_seq(Visitor)
    }
}

#[derive(Debug)]
enum StringOrArray<'a> {
    String(Str<'a>),
    Array(Box<[Expression<'a>]>),
}

impl<'de> serde::Deserialize<'de> for StringOrArray<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = StringOrArray<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a string or an array")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_string(String::from(v))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(StringOrArray::String(Cow::Borrowed(v)))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(StringOrArray::String(Cow::Owned(v)))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut vec = Vec::<Expression>::with_capacity(seq.size_hint().unwrap_or(0));
                while let Some(element) = seq.next_element()? {
                    vec.push(element);
                }
                Ok(StringOrArray::Array(vec.into_boxed_slice()))
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}
