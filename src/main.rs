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
    parser: Arc<Mutex<Parser>>
}

impl Z {
    fn new() -> Self {
        let mut parser = Parser::new();
        let lang = tree_sitter_rust::language();
        parser.set_language(lang).expect("unfit language provided");

        Z {
            ast: Arc::new(Mutex::new(None)),
            parser: Arc::new(Mutex::new(parser))
        }
    }

    // Number of mutation sites: 2
    fn parse(&self, text: impl AsRef<[u8]>) {
        // Rust dance to interact with concurrent-safe mutable state.
        let parser_ptr = Arc::clone(&self.parser);
        let mut parser = parser_ptr.lock().unwrap();

        let ast_ptr = Arc::clone(&self.ast);
        let mut ast = ast_ptr.lock().unwrap();
        let ast_edit: Option<&mut Tree> = ast.as_mut();

        // mutation:
        *ast = match /* mutation: */ parser.parse(text, ast_edit.as_deref()) {
            None => panic!("https://docs.rs/tree-sitter/0.20.10/tree_sitter/struct.Parser.html#method.parse"),
            new_ast => {
                // println!("\there:\n{:?}", new_ast);
                new_ast
            }
        };
    }
}

#[tonic::async_trait]
impl Zer for Z {
    async fn replace(
        &self,
        _request: Request<ts_proto::Source>,
    ) -> Result<Response<ts_proto::Reply>, Status> {
        self.parse( "fn really_really_long(a: u32) {}");

        let reply = ts_proto::Reply { val: 200 };
        Ok(Response::new(reply))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:50051".parse()?;

    Server::builder()
        .add_service(ZerServer::new(Z::new()))
        .serve(addr)
        .await?;

    Ok(())
}
