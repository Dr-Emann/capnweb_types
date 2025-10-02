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

pub mod expression;

pub use expression::Expression;

use serde_core::de::{SeqAccess, Unexpected};
use serde_core::{Deserialize, Deserializer, Serialize, Serializer};
use std::borrow::Cow;
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

impl Serialize for Message<'_> {
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

impl<'de> serde_core::de::Deserialize<'de> for Message<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde_core::de::Visitor<'de> for Visitor {
            type Value = Message<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Cap'n Web top-level message")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let kind: Str<'de> = seq.next_element()?.ok_or_else(|| {
                    serde_core::de::Error::invalid_length(1, &"an array starting with a string")
                })?;
                match &*kind {
                    "push" => {
                        let expected = "a push message";
                        let expr: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Push(expr))
                    }
                    "pull" => {
                        let expected = "a pull message";
                        let id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Pull(id))
                    }
                    "resolve" => {
                        let expected = "a resolve message";
                        let export_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        let value: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(4, &expected));
                        }
                        Ok(Message::Resolve { export_id, value })
                    }
                    "reject" => {
                        let expected = "a reject message";
                        let export_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        let value: Expression = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(4, &expected));
                        }
                        Ok(Message::Reject { export_id, value })
                    }
                    "release" => {
                        let expected = "a release message";
                        let import_id: Id = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        let refcount: u64 = seq
                            .next_element()?
                            .ok_or_else(|| serde_core::de::Error::invalid_length(2, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(4, &expected));
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
                            .ok_or_else(|| serde_core::de::Error::invalid_length(1, &expected))?;
                        if seq.next_element::<serde_core::de::IgnoredAny>()?.is_some() {
                            return Err(serde_core::de::Error::invalid_length(3, &expected));
                        }
                        Ok(Message::Abort(expr))
                    }
                    other_kind => Err(serde_core::de::Error::invalid_value(
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

impl<'de> Deserialize<'de> for StringOrArray<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde_core::de::Visitor<'de> for Visitor {
            type Value = StringOrArray<'de>;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("a string or an array")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde_core::de::Error,
            {
                self.visit_string(String::from(v))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde_core::de::Error,
            {
                Ok(StringOrArray::String(Cow::Borrowed(v)))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde_core::de::Error,
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
