# compact
Compact derives basic binary encodings for arbitrary data types,
using [staged generics](https://github.com/modular-implicits/staged-generics).

Compact is a bit like the [serde](https://serde.rs/) library in Rust
(which also provides generic serialisation and deserialisation for arbitrary types).
Serde uses procedural macros to derive implementations for user-defined data types,
but compact uses the staged generics interface, which is more declarative.

Unlike serde, which requires just one line, you still have to manually write instances of Sum or Product to use compact.
However, these instances could then be used for all kinds of generic code.
In future, the OCaml compiler could automatically derive these generic instances,
like how the Haskell compiler can derive [Generic](https://hackage.haskell.org/package/base/docs/GHC-Generics.html).

**Warning:** (unlike serde, which is a fully-featured framework) compact is not remotely production-ready.
Its encodings are pretty stupid, and cannot be relied upon to be the same across different versions of the OCaml
compiler, or of the compact or staged-generics libraries.
Furthermore, it doesn't work for types that can be an arbitrary length (like lists), and can't even encode things that
take more than 63 bits.

## Build instructions
Compact requires OCaml with both the [modular implicits](https://github.com/ocamllabs/ocaml-modular-implicits)
and [BER MetaOCaml](https://okmij.org/ftp/meta-programming/ber-design.pdf) extensions.

You can install that variant of OCaml using `opam`:

```bash
opam switch add modular-implicits-ber 4.02.1+modular-implicits-ber
```

Getting this library to build is a bit tricky.
The `HACK` directory contains a shim to convince Dune to use `metaocamlc` instead of `ocamlc`.
You need to add it to your path. For example:

```bash
PATH="$(pwd)/HACK:$PATH" dune build
```

There is probably a better way of doing this.
It appears to be unnecessary in modern versions of Dune, but those don't support this old version of OCaml.
