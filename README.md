- tree-sitter-utlc: ast util and syntax highlight
- incremental-analyzer-web: monaco editor web frontend app
- logicmon: logic program as an rpc service that monitors subject language source to analyze it incrementally

## Testing
```
cargo r                   # start rpc server in one terminal session/pane...
cargo r --example client  # ...fire request from example client
cargo r --example dl
```
