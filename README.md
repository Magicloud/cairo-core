# Cairo Haskell binding - Core

This is a library for Haskell to use Cairo function straightforwardly. This package only contains non-optional functions (which is not controlled by building flags). Optional functions like surface supporting and font supporting are in "cairo-opts".

The source is inspired by [garetxe](https://github.com/garetxe) and [cohomology](https://github.com/cohomology).

## Memory safe

All the pointers should be reference/destroy -ed as manual required, if possible. Hopefully I did not miss any part or misunderstand the document.

## Type

When making the library, I tried to use more meaningful type name. So one looking at the type signature would know what to do.

## Render

A sugar monad wrapping all functions use `Context` as first parameter, like cairo from gtk2hs does.

## Status check

Checking the status of most Cairo objects is necessary, after creation, or a sequence of actions on it. A sugar method `with` is given for `Render`. And a method `use` is given for regular IO monad.

# ALERT

The library cannot be built by `stack`. Some conflicts on code generation. Still working on.
