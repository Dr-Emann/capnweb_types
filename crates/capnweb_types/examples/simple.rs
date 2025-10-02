use capnweb_types::expression::{ImportOperation, PathSegment, Resolution};
use capnweb_types::{Expression, Message};

fn main() {
    let expr = Expression::Import {
        resolution: Resolution::Promise,
        id: 0,
        operation: Some(ImportOperation::Call {
            path: vec![PathSegment::Name("authenticate".into())].into_boxed_slice(),
            args: vec![Expression::String("cookie-123".into())].into_boxed_slice(),
        }),
    };

    let msg = Message::Push(expr);

    let res = serde_json::to_string(&msg).unwrap();
    println!("{res}");

    let serialized = r#"["push",["pipeline",0,["authenticate"],["cookie-123"]]]"#;
    assert_eq!(serialized, res);

    let deserialized: Message = serde_json::from_str(serialized).unwrap();
    println!("{deserialized:?}");
}
