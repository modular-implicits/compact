# compact

## Build instructions
The `HACK` directory contains a shim to convince Dune to use `metaocamlc` instead of `ocamlc`.
You need to add it to your path. For example:

```bash
PATH="$(pwd)/HACK:$PATH" dune build
```

There is probably a better way of doing this.
It appears to be unnecessary in modern versions of Dune, but those don't support this old version of OCaml.
