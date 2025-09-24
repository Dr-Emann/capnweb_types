use capnweb_types::{Expression, Message, PathSegment, ImportOperation, Resolution};

fn main() {
    let expr = Expression::Import {
        resolution: Resolution::Promise,
        id: 0,
        operation: Some(ImportOperation::Call {
            path: vec![PathSegment::Name("authenticate".into())].into_boxed_slice(),
            args: vec![Expression::String("cookie-123".into())].into_boxed_slice(),
        })
    };

    let msg = Message::Push(expr);
    println!("{} {}", size_of::<Expression>(), size_of::<Message>());

    let res = serde_json::to_string(&msg).unwrap();
    println!("{}", res);
}