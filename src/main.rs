// use tree_sitter::{Parser, InputEdit, Point};
use tree_sitter::Parser;

use tonic::{transport::Server, Request, Response, Status};

use ts_proto::zer_server::{Zer, ZerServer};
// use ts_proto::greeter_server::{Greeter, GreeterServer};
// use ts_proto::{HelloReply, HelloRequest};
// use ts_proto::HelloReply;

pub mod ts_proto {
    tonic::include_proto!("ts");
}

#[derive(Debug, Default)]
pub struct Z {}

#[tonic::async_trait]
impl Zer for Z {
    async fn replace(
        &self,
        request: Request<ts_proto::Source>,
    ) -> Result<Response<ts_proto::Reply>, Status> {
        let reply = ts_proto::Reply { val: 200 };
        Ok(Response::new(reply))
    }
}

#[derive(Debug, Default)]
pub struct G {}

// #[tonic::async_trait]
// impl Greeter for G {
//     async fn say_hello(
//         &self,
//         request: Request<ts_proto::InputEdit>,
//     ) -> Result<Response<HelloReply>, Status> {
//         println!("Got a request: {:?}", request);
//
//         let reply = ts_proto::HelloReply {
//             message: format!("Hello {}!", request.into_inner().message),
//         };
//
//         Ok(Response::new(reply))
//     }
// }

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let lang = tree_sitter_utlc::language();
    let lang = tree_sitter_rust::language();

    let mut parser = Parser::new();
    parser.set_language(lang).expect("unfit language provided");

    // let source_code = "(lambda (x) (x x))";
    let source_code = "fn test() {}";
    let mut tree = parser.parse(source_code, None).expect("todo");
    let root_node = tree.root_node();
    println!("{:?}", root_node);


    // tonic
    let addr = "[::1]:50051".parse()?;
    // let greeter = G::default();

    Server::builder()
        // .add_service(GreeterServer::new(greeter))
        .add_service(ZerServer::new(Z::default()))
        .serve(addr)
        .await?;

    Ok(())
}
