use std::sync::{Arc, Mutex};
use tonic::{transport::Server, Request, Response, Status};
use tree_sitter::{Parser, Tree};

pub mod ts_proto {
    tonic::include_proto!("ts");
}
use ts_proto::zer_server::{Zer, ZerServer};


pub struct Z {
    // std::cell::RefCell<Option<Tree>> here wouldn't suffice because
    // tonic::async_trait needs Sync trait to run on multiple threads. Consult
    // [the book] for a refresher on this concurrency topic.
    //
    // [the book]: https://doc.rust-lang.org/book/ch16-03-shared-state.html
    ast: Arc<Mutex<Option<Tree>>>,
    parser: Parser
    // parser: Arc<Mutex<Parser>>  // ??
}

impl Z {
    fn new() -> Self {
        let mut parser = Parser::new();
        let lang = tree_sitter_rust::language();

        parser.set_language(lang).expect("unfit language provided");

        Z { ast: Arc::new(Mutex::new(None)), parser }
    }

    // Number of mutation sites: 2
    fn parse(&self, text: impl AsRef<[u8]>) {
//  fn parse(&mut self, text: impl AsRef<[u8]>) {  // compiles
        // Rust dance to get concurrent-safe mutable state.
        let ast = Arc::clone(&self.ast);
        let mut binding = ast.lock().unwrap();
        let ast_edit = binding.as_mut();

        // ??:
        // let new_ast = match self.parser.parse(text, ast_edit.as_deref()) {  // mutation
        //     None => panic!("https://docs.rs/tree-sitter/0.20.10/tree_sitter/struct.Parser.html#method.parse"),
        //     v => v
        // };
        // *binding = new_ast;  // mutation
    }
}

#[tonic::async_trait]
impl Zer for Z {
    // fn new() -> Self {
    // }

    async fn replace(
        &self,
        // &mut self,
        _request: Request<ts_proto::Source>,
    ) -> Result<Response<ts_proto::Reply>, Status> {
        let _ast = Arc::clone(&self.ast);
        // let mut edit_tree = ast.lock().expect("todo").as_mut();

        let mut parser = Parser::new();
        let lang = tree_sitter_rust::language();
        parser.set_language(lang).expect("unfit language provided");
        // ?? using it for something, then use request's
        let _new_source_code = "fn test(a: u32) {}";
        // self.parse( "fn test(a: u32) {}");

        let reply = ts_proto::Reply { val: 200 };
        Ok(Response::new(reply))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:50051".parse()?;

    let mut z = Z::new();
    z.parse("fn test() {}");

    Server::builder()
        .add_service(ZerServer::new(z))
        .serve(addr)
        .await?;

    Ok(())
}
