# Cap'n Web Protocol Type Definitions

This crate provides type definitions for the [Cap'n Web Protocol][capnweb], which is an object-capability RPC protocol
designed for use in web applications.

The types defined in this crate can be serialized and deserialized using serde. Cap'n Web messages are encoded as JSON,
use the `serde_json` crate for serialization and deserialization matching the Cap'n Web specification.

This is not an official Cap'n Web project.

[capnweb]: https://github.com/cloudflare/capnweb