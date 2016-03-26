# Egg!

## Overview

Egg is a toy language, meant for me to learn about LLVM IR generation in
particular, and more broadly about compilation and optimization techniques. It's
currently capable of very little; it understands boolean and integer types,
if-else expressions, and function definitions (including recursive ones, thanks
  mostly to LLVM's automagic). The syntax is in extreme flux while I decide what
  is pleasant to work with, so I won't bother explicating it here yet.

## Details

- The AST is represented as an open-ended functor; this allows annotated
  expressions to be represented with the cofree comonad (via a self-implemented
  kludge) and in particular "pure" expressions to be represented by the comonad
  over `()`.

- Typechecking is (surprise!) also a messy hack. Currently a "typechecker
  failure" really just means a hard failure during compilation using `error`.
  For now, this is enough to at least warn me that something isn't right with
  compilation (or with the code I've written).

## Next steps

- [ ] Next in line (and already partially underway) is implementing lambdas,
  which I've been procrastinating on since they require a significant amount of
  restructuring. Currently, code generation assumes a program defined by a
  sequential list of function definitions, which are compiled into LLVM `define`
  representations. Lambdas throw this off, since we can no longer assume that
  the product of code generation for a function will be a list of simple body
  blocks (instead, we should expect the body blocks along with a representation
  of any closures defined within the function body).

  This probably indicates that privileging top-level function definitions over
  lambdas is an inelegant way to move forward. Since the current representation
  lends itself very easily to pure FP, and much less easily to mutable state, it
  seems natural to represent a program as a list of lambdas instead. This will
  probably also make typechecking simpler; rather than checking the type of
  function definitions separately, we can just check them as lambdas with the
  appropriate function type.

- [ ] FFI support, or at least hardwiring `puts` into the language. It's
  frustrating to test by shell return value, and will only get more frustrating
  as my types get more intricate.

- [ ] A barebones type system would make this feel like a real language. Record
  and union types, then recursive definitions to allow linked lists, seems like
  enough.

- [ ] Real typechecking seems like a reasonable next step. I can imagine the
  current annotation step growing more or less organically out of the current
  machinery; for the foreseeable future, hard `error`s seem fine if I'm
  convinced enough that a real typechecker must logically make them impossible
  in the course of compilation.

- [ ] Finally, a directory of real examples, with expected output/return values,
  would be valuable once syntax becomes more stable. Currently I just use
  `example.egg` to try out new features as I write them; eventually it would be
  nice to turn these individual cases into example files, and then into
  automated tests.
