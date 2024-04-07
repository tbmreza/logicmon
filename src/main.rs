use tonic::{transport::Server, Request, Response, Status};
use tree_sitter::{Parser, InputEdit, Point};

pub mod ts_proto {
    tonic::include_proto!("ts");
}
use ts_proto::zer_server::{Zer, ZerServer};

#[derive(Debug, Default)]
pub struct Z {}

#[tonic::async_trait]
impl Zer for Z {
    async fn replace(
        &self,
        _request: Request<ts_proto::Source>,
    ) -> Result<Response<ts_proto::Reply>, Status> {
        let reply = ts_proto::Reply { val: 200 };
        Ok(Response::new(reply))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
// fn main() {
    // let lang = tree_sitter_utlc::language();
    let lang = tree_sitter_rust::language();

    let mut parser = Parser::new();
    parser.set_language(lang).expect("unfit language provided");

    // let source_code = "(lambda (x) (x x))";
    let source_code = "fn test() {}";
    let mut tree = parser.parse(source_code, None).expect("todo");
    let root_node = tree.root_node();
    println!("{:?}", root_node);

    let new_source_code = "fn test(a: u32) {}";

    tree.edit(&InputEdit {
      start_byte: 8,
      old_end_byte: 8,
      new_end_byte: 14,
      start_position: Point::new(0, 8),
      old_end_position: Point::new(0, 8),
      new_end_position: Point::new(0, 14),
    });

    let new_tree = parser.parse(new_source_code, Some(&tree)).expect("todo");
    println!("{:?}", new_tree.root_node());



    // tonic
    let addr = "[::1]:50051".parse()?;

    Server::builder()
        // .add_service(GreeterServer::new(greeter))
        .add_service(ZerServer::new(Z::default()))
        .serve(addr)
        .await?;

    Ok(())
}
