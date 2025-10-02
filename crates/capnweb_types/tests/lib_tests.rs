use capnweb_types::{Expression, Message};
use capnweb_types::expression::*;
use serde_json;
use std::collections::BTreeMap;

#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_basic_expression_serialization() {
        // Test null
        let null_expr = Expression::Null;
        insta::assert_json_snapshot!(null_expr, @"null");

        // Test integer
        let int_expr = Expression::Int(42);
        insta::assert_json_snapshot!(int_expr, @"42");

        // Test float
        let float_expr = Expression::Float(3.14159);
        insta::assert_json_snapshot!(float_expr, @"3.14159");

        // Test string
        let str_expr = Expression::String("hello world".into());
        insta::assert_json_snapshot!(str_expr, @r#""hello world""#);

        // Test boolean true
        let bool_true_expr = Expression::Bool(true);
        insta::assert_json_snapshot!(bool_true_expr, @"true");

        // Test boolean false
        let bool_false_expr = Expression::Bool(false);
        insta::assert_json_snapshot!(bool_false_expr, @"false");
    }

    #[test]
    fn test_array_expression_serialization() {
        // Empty array
        let empty_array = Expression::Array(Box::new([]));
        insta::assert_json_snapshot!(empty_array, @r"
        [
          []
        ]
        ");

        // Simple array
        let array_expr = Expression::Array(Box::new([
            Expression::Int(1),
            Expression::Int(2),
            Expression::Int(3),
        ]));
        insta::assert_json_snapshot!(array_expr, @r"
        [
          [
            1,
            2,
            3
          ]
        ]
        ");

        // Nested array
        let nested_array = Expression::Array(Box::new([
            Expression::Array(Box::new([Expression::Int(1), Expression::Int(2)])),
            Expression::Array(Box::new([Expression::Int(3), Expression::Int(4)])),
        ]));
        insta::assert_json_snapshot!(nested_array, @r"
        [
          [
            [
              [
                1,
                2
              ]
            ],
            [
              [
                3,
                4
              ]
            ]
          ]
        ]
        ");
    }

    #[test]
    fn test_object_expression_serialization() {
        // Empty object
        let empty_obj = Expression::Object(BTreeMap::new());
        insta::assert_json_snapshot!(empty_obj, @"{}");

        // Simple object
        let mut obj = BTreeMap::new();
        obj.insert("foo".into(), Expression::Int(123));
        obj.insert("bar".into(), Expression::String("baz".into()));
        let obj_expr = Expression::Object(obj);
        insta::assert_json_snapshot!(obj_expr, @r#"
        {
          "bar": "baz",
          "foo": 123
        }
        "#);

        // Nested object
        let mut nested_obj = BTreeMap::new();
        nested_obj.insert(
            "outer".into(),
            Expression::Object({
                let mut inner = BTreeMap::new();
                inner.insert("inner".into(), Expression::Bool(true));
                inner
            }),
        );
        let nested_expr = Expression::Object(nested_obj);
        insta::assert_json_snapshot!(nested_expr, @r#"
        {
          "outer": {
            "inner": true
          }
        }
        "#);
    }

    #[test]
    fn test_error_expression_serialization() {
        // Error without stack
        let error_expr = Expression::Error(Box::new(Error {
            ty: "TypeError".into(),
            message: "test message".into(),
            stack: None,
        }));
        insta::assert_json_snapshot!(error_expr, @r#"
        [
          "error",
          "TypeError",
          "test message"
        ]
        "#);

        // Error with stack
        let error_with_stack = Expression::Error(Box::new(Error {
            ty: "RangeError".into(),
            message: "out of range".into(),
            stack: Some("at line 42".into()),
        }));
        insta::assert_json_snapshot!(error_with_stack, @r#"
        [
          "error",
          "RangeError",
          "out of range",
          "at line 42"
        ]
        "#);

        // Different error types
        let syntax_error = Expression::Error(Box::new(Error {
            ty: "SyntaxError".into(),
            message: "unexpected token".into(),
            stack: None,
        }));
        insta::assert_json_snapshot!(syntax_error, @r#"
        [
          "error",
          "SyntaxError",
          "unexpected token"
        ]
        "#);
    }

    #[test]
    fn test_date_expression_serialization() {
        let date_expr = Expression::Date(1234567890.123);
        insta::assert_json_snapshot!(date_expr, @r#"
        [
          "date",
          1234567890.123
        ]
        "#);

        // Zero timestamp
        let zero_date = Expression::Date(0.0);
        insta::assert_json_snapshot!(zero_date, @r#"
        [
          "date",
          0.0
        ]
        "#);

        // Negative timestamp
        let negative_date = Expression::Date(-1000.5);
        insta::assert_json_snapshot!(negative_date, @r#"
        [
          "date",
          -1000.5
        ]
        "#);
    }

    #[test]
    fn test_bytes_expression_serialization() {
        let bytes_expr = Expression::Bytes("aGVsbG8h".into()); // base64 for "hello!"
        insta::assert_json_snapshot!(bytes_expr, @r#"
        [
          "bytes",
          "aGVsbG8h"
        ]
        "#);

        // Empty bytes
        let empty_bytes = Expression::Bytes("".into());
        insta::assert_json_snapshot!(empty_bytes, @r#"
        [
          "bytes",
          ""
        ]
        "#);
    }

    #[test]
    fn test_bigint_expression_serialization() {
        let bigint_expr = Expression::BigInt("123456789012345678901234567890".into());
        insta::assert_json_snapshot!(bigint_expr, @r#"
        [
          "bigint",
          "123456789012345678901234567890"
        ]
        "#);

        // Small bigint
        let small_bigint = Expression::BigInt("42".into());
        insta::assert_json_snapshot!(small_bigint, @r#"
        [
          "bigint",
          "42"
        ]
        "#);

        // Negative bigint
        let negative_bigint = Expression::BigInt("-999999999999999999999".into());
        insta::assert_json_snapshot!(negative_bigint, @r#"
        [
          "bigint",
          "-999999999999999999999"
        ]
        "#);
    }

    #[test]
    fn test_undefined_expression_serialization() {
        let undefined_expr = Expression::Undefined;
        insta::assert_json_snapshot!(undefined_expr, @r#"
        [
          "undefined"
        ]
        "#);
    }

    #[test]
    fn test_import_expression_serialization() {
        // Basic import without operation (stub)
        let import_stub = Expression::Import {
            id: 42,
            operation: None,
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(import_stub, @r#"
        [
          "import",
          42
        ]
        "#);

        // Basic pipeline without operation (promise)
        let import_promise = Expression::Import {
            id: -1,
            operation: None,
            resolution: Resolution::Promise,
        };
        insta::assert_json_snapshot!(import_promise, @r#"
        [
          "pipeline",
          -1
        ]
        "#);

        // Zero ID
        let import_zero = Expression::Import {
            id: 0,
            operation: None,
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(import_zero, @r#"
        [
          "import",
          0
        ]
        "#);
    }

    #[test]
    fn test_import_with_property_operation() {
        // Simple property access
        let import_prop = Expression::Import {
            id: 1,
            operation: Some(ImportOperation::Prop {
                path: Box::new([PathSegment::Name("foo".into())]),
            }),
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(import_prop, @r#"
        [
          "import",
          1,
          [
            "foo"
          ]
        ]
        "#);

        // Nested property access
        let nested_prop = Expression::Import {
            id: 2,
            operation: Some(ImportOperation::Prop {
                path: Box::new([
                    PathSegment::Name("obj".into()),
                    PathSegment::Name("nested".into()),
                    PathSegment::Name("prop".into()),
                ]),
            }),
            resolution: Resolution::Promise,
        };
        insta::assert_json_snapshot!(nested_prop, @r#"
        [
          "pipeline",
          2,
          [
            "obj",
            "nested",
            "prop"
          ]
        ]
        "#);

        // Array index access
        let array_access = Expression::Import {
            id: 3,
            operation: Some(ImportOperation::Prop {
                path: Box::new([PathSegment::Name("array".into()), PathSegment::Index(5)]),
            }),
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(array_access, @r#"
        [
          "import",
          3,
          [
            "array",
            5
          ]
        ]
        "#);
    }

    #[test]
    fn test_import_with_call_operation() {
        // Simple method call
        let method_call = Expression::Import {
            id: 10,
            operation: Some(ImportOperation::Call {
                path: Box::new([PathSegment::Name("method".into())]),
                args: Box::new([Expression::Int(42)]),
            }),
            resolution: Resolution::Promise,
        };
        insta::assert_json_snapshot!(method_call, @r#"
        [
          "pipeline",
          10,
          [
            "method"
          ],
          [
            42
          ]
        ]
        "#);

        // Method call with multiple arguments
        let multi_arg_call = Expression::Import {
            id: 11,
            operation: Some(ImportOperation::Call {
                path: Box::new([PathSegment::Name("calculate".into())]),
                args: Box::new([
                    Expression::Int(10),
                    Expression::String("test".into()),
                    Expression::Bool(true),
                ]),
            }),
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(multi_arg_call, @r#"
        [
          "import",
          11,
          [
            "calculate"
          ],
          [
            10,
            "test",
            true
          ]
        ]
        "#);

        // Nested method call
        let nested_call = Expression::Import {
            id: 12,
            operation: Some(ImportOperation::Call {
                path: Box::new([
                    PathSegment::Name("obj".into()),
                    PathSegment::Index(0),
                    PathSegment::Name("method".into()),
                ]),
                args: Box::new([]),
            }),
            resolution: Resolution::Promise,
        };
        insta::assert_json_snapshot!(nested_call, @r#"
        [
          "pipeline",
          12,
          [
            "obj",
            0,
            "method"
          ],
          []
        ]
        "#);
    }

    #[test]
    fn test_export_expression_serialization() {
        // Export stub
        let export_stub = Expression::Export {
            id: -10,
            resolution: Resolution::Stub,
        };
        insta::assert_json_snapshot!(export_stub, @r#"
        [
          "export",
          -10
        ]
        "#);

        // Export promise
        let export_promise = Expression::Export {
            id: -5,
            resolution: Resolution::Promise,
        };
        insta::assert_json_snapshot!(export_promise, @r#"
        [
          "promise",
          -5
        ]
        "#);
    }

    #[test]
    fn test_remap_expression_serialization() {
        // Simple remap
        let simple_remap = Expression::Remap(Box::new(Remap {
            import_id: 1,
            property_path: Box::new([PathSegment::Name("items".into())]),
            captures: Box::new([Capture {
                ty: CaptureType::Import,
                id: 2,
            }]),
            instructions: Box::new([Expression::Int(42)]),
        }));
        insta::assert_json_snapshot!(simple_remap, @r#"
        [
          "remap",
          1,
          [
            "items"
          ],
          [
            [
              "import",
              2
            ]
          ],
          [
            42
          ]
        ]
        "#);

        // Complex remap with multiple captures and instructions
        let complex_remap = Expression::Remap(Box::new(Remap {
            import_id: 5,
            property_path: Box::new([PathSegment::Name("data".into()), PathSegment::Index(0)]),
            captures: Box::new([
                Capture {
                    ty: CaptureType::Import,
                    id: 10,
                },
                Capture {
                    ty: CaptureType::Export,
                    id: -3,
                },
            ]),
            instructions: Box::new([
                Expression::Import {
                    id: -1,
                    operation: None,
                    resolution: Resolution::Promise,
                },
                Expression::String("processed".into()),
            ]),
        }));
        insta::assert_json_snapshot!(complex_remap, @r#"
        [
          "remap",
          5,
          [
            "data",
            0
          ],
          [
            [
              "import",
              10
            ],
            [
              "export",
              -3
            ]
          ],
          [
            [
              "pipeline",
              -1
            ],
            "processed"
          ]
        ]
        "#);
    }

    #[test]
    fn test_path_segment_serialization() {
        // String path segment
        let name_segment = PathSegment::Name("property".into());
        insta::assert_json_snapshot!(name_segment, @r#""property""#);

        // Index path segment
        let index_segment = PathSegment::Index(42);
        insta::assert_json_snapshot!(index_segment, @"42");

        // Zero index
        let zero_index = PathSegment::Index(0);
        insta::assert_json_snapshot!(zero_index, @"0");
    }

    #[test]
    fn test_capture_serialization() {
        // Import capture
        let import_capture = Capture {
            ty: CaptureType::Import,
            id: 5,
        };
        insta::assert_json_snapshot!(import_capture, @r#"
        [
          "import",
          5
        ]
        "#);

        // Export capture
        let export_capture = Capture {
            ty: CaptureType::Export,
            id: -3,
        };
        insta::assert_json_snapshot!(export_capture, @r#"
        [
          "export",
          -3
        ]
        "#);
    }

    #[test]
    fn test_complex_nested_structures() {
        // Complex nested object similar to the JavaScript reference tests
        let mut obj = BTreeMap::new();
        obj.insert(
            "level1".into(),
            Expression::Object({
                let mut inner = BTreeMap::new();
                inner.insert(
                    "level2".into(),
                    Expression::Object({
                        let mut level2 = BTreeMap::new();
                        level2.insert(
                            "level3".into(),
                            Expression::Object({
                                let mut level3 = BTreeMap::new();
                                level3.insert(
                                    "array".into(),
                                    Expression::Array(Box::new([
                                        Expression::Int(1),
                                        Expression::Int(2),
                                        Expression::Object({
                                            let mut nested = BTreeMap::new();
                                            nested.insert(
                                                "nested".into(),
                                                Expression::String("deep".into()),
                                            );
                                            nested
                                        }),
                                    ])),
                                );
                                level3.insert("date".into(), Expression::Date(5678.0));
                                level3.insert("nullVal".into(), Expression::Null);
                                level3.insert("undefinedVal".into(), Expression::Undefined);
                                level3
                            }),
                        );
                        level2
                    }),
                );
                inner
            }),
        );
        obj.insert(
            "top_array".into(),
            Expression::Array(Box::new([
                Expression::Array(Box::new([Expression::Int(1), Expression::Int(2)])),
                Expression::Array(Box::new([Expression::Int(3), Expression::Int(4)])),
            ])),
        );

        let complex_expr = Expression::Object(obj);
        insta::assert_json_snapshot!(complex_expr);
    }
}

#[cfg(test)]
mod message_tests {
    use super::*;

    #[test]
    fn test_push_message_serialization() {
        let push_int = Message::Push(Expression::Int(42));
        insta::assert_json_snapshot!(push_int, @r#"
        [
          "push",
          42
        ]
        "#);

        let push_string = Message::Push(Expression::String("hello".into()));
        insta::assert_json_snapshot!(push_string, @r#"
        [
          "push",
          "hello"
        ]
        "#);

        let push_complex = Message::Push(Expression::Object({
            let mut obj = BTreeMap::new();
            obj.insert("key".into(), Expression::Bool(true));
            obj
        }));
        insta::assert_json_snapshot!(push_complex, @r#"
        [
          "push",
          {
            "key": true
          }
        ]
        "#);
    }

    #[test]
    fn test_pull_message_serialization() {
        let pull_positive = Message::Pull(123);
        insta::assert_json_snapshot!(pull_positive, @r#"
        [
          "pull",
          123
        ]
        "#);

        let pull_negative = Message::Pull(-456);
        insta::assert_json_snapshot!(pull_negative, @r#"
        [
          "pull",
          -456
        ]
        "#);

        let pull_zero = Message::Pull(0);
        insta::assert_json_snapshot!(pull_zero, @r#"
        [
          "pull",
          0
        ]
        "#);
    }

    #[test]
    fn test_resolve_message_serialization() {
        let resolve_simple = Message::Resolve {
            export_id: -5,
            value: Expression::String("resolved".into()),
        };
        insta::assert_json_snapshot!(resolve_simple, @r#"
        [
          "resolve",
          -5,
          "resolved"
        ]
        "#);

        let resolve_complex = Message::Resolve {
            export_id: -10,
            value: Expression::Array(Box::new([
                Expression::Int(1),
                Expression::Int(2),
                Expression::Int(3),
            ])),
        };
        insta::assert_json_snapshot!(resolve_complex, @r#"
        [
          "resolve",
          -10,
          [
            [
              1,
              2,
              3
            ]
          ]
        ]
        "#);
    }

    #[test]
    fn test_reject_message_serialization() {
        let reject_error = Message::Reject {
            export_id: -10,
            value: Expression::Error(Box::new(Error {
                ty: "Error".into(),
                message: "rejected".into(),
                stack: None,
            })),
        };
        insta::assert_json_snapshot!(reject_error, @r#"
        [
          "reject",
          -10,
          [
            "error",
            "Error",
            "rejected"
          ]
        ]
        "#);

        let reject_with_stack = Message::Reject {
            export_id: -20,
            value: Expression::Error(Box::new(Error {
                ty: "TimeoutError".into(),
                message: "operation timed out".into(),
                stack: Some("at timeout handler".into()),
            })),
        };
        insta::assert_json_snapshot!(reject_with_stack, @r#"
        [
          "reject",
          -20,
          [
            "error",
            "TimeoutError",
            "operation timed out",
            "at timeout handler"
          ]
        ]
        "#);
    }

    #[test]
    fn test_release_message_serialization() {
        let release_msg = Message::Release {
            import_id: 100,
            refcount: 5,
        };
        insta::assert_json_snapshot!(release_msg, @r#"
        [
          "release",
          100,
          5
        ]
        "#);

        let release_zero_refcount = Message::Release {
            import_id: 50,
            refcount: 0,
        };
        insta::assert_json_snapshot!(release_zero_refcount, @r#"
        [
          "release",
          50,
          0
        ]
        "#);

        let release_high_refcount = Message::Release {
            import_id: 1,
            refcount: 999,
        };
        insta::assert_json_snapshot!(release_high_refcount, @r#"
        [
          "release",
          1,
          999
        ]
        "#);
    }

    #[test]
    fn test_abort_message_serialization() {
        let abort_simple = Message::Abort(Expression::String("Connection lost".into()));
        insta::assert_json_snapshot!(abort_simple, @r#"
        [
          "abort",
          "Connection lost"
        ]
        "#);

        let abort_error = Message::Abort(Expression::Error(Box::new(Error {
            ty: "FatalError".into(),
            message: "Connection terminated".into(),
            stack: Some("stack trace".into()),
        })));
        insta::assert_json_snapshot!(abort_error, @r#"
        [
          "abort",
          [
            "error",
            "FatalError",
            "Connection terminated",
            "stack trace"
          ]
        ]
        "#);
    }
}

#[cfg(test)]
mod roundtrip_tests {
    use super::*;

    #[test]
    fn test_expression_roundtrip_serialization() {
        let test_cases = vec![
            Expression::Null,
            Expression::Int(42),
            Expression::Int(-100),
            Expression::Float(3.14159),
            Expression::Float(-2.718),
            Expression::String("test string".into()),
            Expression::String("".into()),
            Expression::Bool(true),
            Expression::Bool(false),
            Expression::Undefined,
            Expression::Date(1640995200000.0),    // Jan 1, 2022
            Expression::Bytes("dGVzdA==".into()), // base64 "test"
            Expression::BigInt("12345678901234567890".into()),
            Expression::Error(Box::new(Error {
                ty: "TypeError".into(),
                message: "test error".into(),
                stack: None,
            })),
            Expression::Array(Box::new([
                Expression::Int(1),
                Expression::String("two".into()),
                Expression::Bool(true),
            ])),
            Expression::Object({
                BTreeMap::from([
                    ("a".into(), Expression::Int(1)),
                    ("b".into(), Expression::String("text".into())),
                ])
            }),
            Expression::Import {
                id: 42,
                operation: None,
                resolution: Resolution::Stub,
            },
            Expression::Export {
                id: -10,
                resolution: Resolution::Promise,
            },
        ];

        for (i, original) in test_cases.into_iter().enumerate() {
            let json = serde_json::to_string(&original).unwrap();
            let deserialized: Expression = serde_json::from_str(&json).unwrap();

            // Re-serialize to ensure consistency
            let json2 = serde_json::to_string(&deserialized).unwrap();
            assert_eq!(json, json2, "Roundtrip failed for test case {}", i);

            insta::assert_json_snapshot!(format!("roundtrip_case_{}", i), original);
        }
    }

    #[test]
    fn test_message_roundtrip_serialization() {
        let test_messages = vec![
            Message::Push(Expression::Int(42)),
            Message::Pull(123),
            Message::Resolve {
                export_id: -5,
                value: Expression::String("success".into()),
            },
            Message::Reject {
                export_id: -10,
                value: Expression::Error(Box::new(Error {
                    ty: "Error".into(),
                    message: "failed".into(),
                    stack: None,
                })),
            },
            Message::Release {
                import_id: 100,
                refcount: 3,
            },
            Message::Abort(Expression::String("shutdown".into())),
        ];

        for (i, original) in test_messages.into_iter().enumerate() {
            let json = serde_json::to_string(&original).unwrap();
            let deserialized: Message = serde_json::from_str(&json).unwrap();

            // Re-serialize to ensure consistency
            let json2 = serde_json::to_string(&deserialized).unwrap();
            assert_eq!(json, json2, "Message roundtrip failed for test case {}", i);

            insta::assert_json_snapshot!(format!("message_roundtrip_case_{}", i), original);
        }
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_edge_case_values() {
        // Maximum and minimum integer values
        let max_int = Expression::Int(i64::MAX);
        insta::assert_json_snapshot!(max_int);

        let min_int = Expression::Int(i64::MIN);
        insta::assert_json_snapshot!(min_int);

        // Special float values
        let positive_infinity = Expression::Float(f64::INFINITY);
        insta::assert_json_snapshot!(positive_infinity);

        let negative_infinity = Expression::Float(f64::NEG_INFINITY);
        insta::assert_json_snapshot!(negative_infinity);

        let nan_value = Expression::Float(f64::NAN);
        insta::assert_json_snapshot!(nan_value);

        // Empty containers
        let empty_array = Expression::Array(Box::new([]));
        insta::assert_json_snapshot!(empty_array, @r"
        [
          []
        ]
        ");

        let empty_object = Expression::Object(BTreeMap::new());
        insta::assert_json_snapshot!(empty_object, @"{}");

        // Empty strings in various contexts
        let empty_string = Expression::String("".into());
        insta::assert_json_snapshot!(empty_string, @r#""""#);

        let empty_bigint = Expression::BigInt("".into());
        insta::assert_json_snapshot!(empty_bigint, @r#"
        [
          "bigint",
          ""
        ]
        "#);

        let empty_bytes = Expression::Bytes("".into());
        insta::assert_json_snapshot!(empty_bytes, @r#"
        [
          "bytes",
          ""
        ]
        "#);
    }

    #[test]
    fn test_unicode_and_special_characters() {
        // Unicode strings
        let unicode_string = Expression::String("Hello, 世界! 🌍".into());
        insta::assert_json_snapshot!(unicode_string);

        // Strings with escape characters
        let escaped_string = Expression::String("Line 1\nLine 2\tTabbed".into());
        insta::assert_json_snapshot!(escaped_string);

        // Object keys with special characters
        let mut special_obj = BTreeMap::new();
        special_obj.insert("key with spaces".into(), Expression::Int(1));
        special_obj.insert("key-with-dashes".into(), Expression::Int(2));
        special_obj.insert("key_with_underscores".into(), Expression::Int(3));
        special_obj.insert("keyWithCamelCase".into(), Expression::Int(4));
        let special_key_obj = Expression::Object(special_obj);
        insta::assert_json_snapshot!(special_key_obj);
    }

    #[test]
    fn test_deeply_nested_structures() {
        // Create a deeply nested structure to test recursion limits
        let mut current = Expression::Int(42);
        for i in 0..10 {
            let mut obj = BTreeMap::new();
            obj.insert(format!("level_{}", i).into(), current);
            current = Expression::Object(obj);
        }
        insta::assert_json_snapshot!(current);
    }
}
