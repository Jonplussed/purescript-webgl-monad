# Graphics.WebGL

A monad wrapping raw WebGL methods and common sets of interactions, providing
access to the WebGL context and error checking and handling.

See the [module documentation][1] for all available types and methods.

  [1]: docs/README.md

## Intention

A good library should educate the user about the subject matter and assist them
in writing readable, error-free code without forcing them into an opinionated
programming flow. Meanwhile, WebGL is raging chimera of poor interaction
models:

- the WebGL context is an impenetrable state machine
- types are represented as constants and passed as method arguments
- shader _programs_ are passed as strings and compiled ad-hoc
- errors must be checked via method call or null value inspection
- the list goes on...

As much as is possible, this library attempts to help authors avoid the most
common pitfalls of WebGL **without** restricting access to low-level
polymorphic methods like `vertexAttribPointer`, which are desirable for
high-performance cases.

## Implementation

The `WebGL a` monad is an alias for `ReaderT WebGLContext (ErrorT WebGLError
(Eff (canvas :: Canvas))) a`, providing the WebGL context and error-handling
without restricting arbitrary function usage.

Modules are divided as follows:

- `Graphics.WebGL.Context` provides functions for obtaining and using the
  WebGL canvas context
- `Graphics.WebGL.Methods` provides all wrapped raw WebGL methods with nominal
  type-checking
- `Graphics.WebGL.Types` provides the `WebGL` monad and represents various WebGL
  enums/constants as datatypes
- `Graphics.WebGL.Shaders` provides helpers for common shader interactions,
  such as obtaining the active bindings or compiling sets of shaders into a
  WebGL program

Any complex behaviors are (as much as is possible) constructed by composing
methods and types, simultaneously allowing authors convenience and the ability
to perform lower-level operations (usually the expense of some type-safety).

## Future Plans

I'd like to wrap any complex sets of behaviors, such as
loading/compiling/retrieving-bindings-of shaders, binding/drawing arrays, etc.
with helper functions. I'm a relative newcomer to graphics programming,
however, so I don't know what all of these common uses _are_.

That's where you come in. Help me implement common patterns by raising Github
issues or providing pull requests! Together we can reign this chimeric state
machine into some semblance of respectability.

## Differences from [purescript-webgl][2]

  [2]: https://github.com/jutaro/purescript-webgl

This library contains some foundational differences that currently render it
incompatible with `purescript-webgl`. For example, `purescript-webgl`:

- replies upon a hidden `gl` global variable for all WebGL methods calls :(
- doesn't provide direct access to raw WebGL methods, meaning...
- complex behaviors aren't composed from otherwise provided functionality
- contains its own `WebGL` effect, meaning canvas methods from
  [`purescript-canvas`][3] must be reimplemented (this library maintains
  type-safety via the provided _context_ rather than the type of _effect_)

  [3]: https://github.com/purescript-contrib/purescript-canvas

It's my sincere hope that this library and `purescript-webgl` can merge
sometime in the future, so that our efforts can become cumulative.

## Credits

This library is inspired and informed by the hard work done by [Jurgen
Nicklisch-Franken][4], the author of `purescript-webgl`. Also, I'd never have
figured out the (simple in retrospect) `WebGL` monad without [Phil Freeman][5]
handing it to me on a silver platter. Thanks for getting me to ask for help
sooner!

  [4]: https://github.com/jutaro
  [5]: https://github.com/paf31
