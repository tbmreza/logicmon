# poster-lang
- [ ] Write the core interpreter in haskell (with step+state) and compile it to WASM.
- [ ] Set up a JS/HTML UI to talk to the interpreter.
- [ ] Define the message format for StepResult and StateSnapshot.
- [ ] Create the playback loop + UI update logic.

## See also
- https://visualgo.net/en



# logicmon
Logic program application that monitors subject language source to analyze it incrementally.




- tree-sitter-utlc: ast util and syntax highlight
- incremental-analyzer-web: monaco editor web frontend app
- logicmon: logic program as an rpc service that monitors subject language source to analyze it incrementally
- logicmon-nvim: 
- logicmon-dx-web: 
dioxus web
Render directly to the DOM using WebAssembly
Pre-render with SSR and rehydrate on the client

## Testing
```
cargo r                   # start rpc server in one terminal session/pane...
cargo r --example client  # ...fire request from example client
cargo r --example dl
```

?? cargo workspace
https://github.com/tokio-rs/axum/blob/main/Cargo.toml

// example/client.rs
use ts_proto::zer_client::ZerClient;
use ts_proto::*;

pub mod ts_proto {
    tonic::include_proto!("ts");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = ZerClient::connect("http://[::1]:50051").await?;

    let request = tonic::Request::new(Source {
        val: String::from("fn test(a: u32) {}"),
        // val: String::from("fn testttttttttttttt(a: u32) {}"),
    });

    let response = client.replace(request).await?;

    println!("RESPONSE={:?}", response);

    Ok(())
}


??
disable tonic transport feature
leptos debug, name it logicmon-play
logicmon-play borked trying the following:
if https://github.com/devashishdxt/tonic-web-wasm-client doesn't work, use ace editor and axum (rest http)
