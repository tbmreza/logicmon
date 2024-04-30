# interlocut

- [X] ascent omega comb in tree
- [ ] listener for editor outputs (InputEdit, whole src,)

## frontend  vite, esm, 
- [ ] send InputEdit and whole src
- [ ] recv StdOut, InternalErrors,


# dev trivia
- If InputEdit contains value (haven't tested which ones; at least start_byte) that doesn't agree with new source, tree.edit will silently ignore the new source it's inputted.
