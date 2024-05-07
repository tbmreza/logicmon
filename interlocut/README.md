# interlocut

- [X] ascent rule for basic lambda calculus
- [X] websocket server
- [ ] tree-sitter match expr
- [ ] tokio concurrency
- [ ] tree-sitter-utlc

## frontend (monaco react app)
- [X] initialize editor in jsx
- [X] websocket send whole editor value
- [ ] websocket recv StdOut, InternalErrors,


# dev trivia
- If InputEdit contains value (haven't tested which ones; at least start_byte) that doesn't agree with new source, tree.edit will silently ignore the new source it's inputted.
- WebSocket is a global js class (lib not required) that MDN has docs for.
