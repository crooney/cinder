These pages only exist as a place to host the examples.  What follows on this page
is only a (possibly out of date) copy of the project's README.

## Cinder
Cinder is a markup and DOM manipulation DSL for the excellent [Fay](http://fay-lang.org) subset of Haskell.

It may be simpler to look at the [examples](http://crooney.github.io/cinder) to get a feel for the lib.

### DSL
Cinder has combinators defined for all elements and attributes of its modeled markups (currently HTML and SVG), as well as inline combinators for them. For example, the body of a simple web page could be specified like this:

```haskell
myBody = markup ! body ! bgcolor "lightblue"
                    !> h1 !: "Cheers"
                    !< p !: "Visit Margarita!"
                        !+> imgSA "i.png" "La Isla" ! Complete
                !< Complete
```

A single `!` (or `!>`) signals that the following element or attribute is a child of the current element. `!:` signifies that content follows. `!` followed by the constructor `Complete` closes the current element and causes following markup to belong to the immediate parent. `!<` is equivalent to `! Complete !`, that is, it closes the current element and starts right in on a sibling. As a special case `!< Complete` closes all pending elements. So, in the example,the `h1` is closed by the `!<` before `p`, the `img` closed explicitly and the `p` and `body` are closed by the `!< Complete`. The `! Complete` after the `img` tag is superfluous, but harmless.

Further, complete sections of markup may be added by `!+` (or `!+<` or `!+>`). Convenience functions are available to add complete tags with commonly used attributes in this manner.  For instance `imgSA "i.png" "La Isla"` is exactly equivalent to `markup ! img ! src "i.png" ! alt "La Isla"`.

Attribute constructors which may reasonably expect numeric arguments may be postfixed by N as a convenience: `heightN 100` as opposed to `height (show 100)`.

### DOM
DOM manipulation happens mostly through `insert` which takes markup and a DOM node and does the obvious. Various functionality is built on top of it, for zipping lists of markup into lists of nodes, etc. In addition, various DOM manipulations not directly related to the DSL are supported.

The following, from one of the examples, deletes all animations, inserts a special effect animation into each circle tag and staggers the animations to start 3/4s of a second apart.

```haskell
grower = do root >>= byTag "animate" >>= mapM_ deleteSelf
            root >>= byTag "circle" >>= zipInsert an
            root >>= byTag "animate" >>= stagger "begin" 9 0.75
            return ()
    where an = zipWith (!+) (repeat (aADR "r" 0.75 1 ! fill "freeze"))
                            [bounce 0.5 5 25 50, settle 0.45 5 25 50,
                             bounce 0.85 6 25 50, settle 0.75 6 25 50]
```
The convenience `aADR "r" 0.75 1` is equal to `markup ! animate ! attributeName "r" ! durN 0.75 ! repeatCountN 1`.

###SVG
Cinder started out as an SVG library, before it was realized that it could be generally useful.  All the examples but one are in SVG and almost no convenience funcs beyond constructors that accept numbers have been yet written for HTML.
