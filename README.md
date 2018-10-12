# Lazy Lists for Elm

Note that as of Elm version 0.19, the memoizing `elm-lang/lazy` package
was dropped. This lazy list implementation does not memoize. This type of lazy
list is often called a sequence, and it will be less lazy than a true lazy list
but is a reasonable compromise without memoizing support from the runtime.
