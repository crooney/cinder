## Cinder
Cinder is a markup and DOM manipulation DSL for the excellent [Fay]
(//fay-lang.org) subset of Haskell.

It would probably be better to look at <//crooney.github.io/cinder>, at least to start.

### DSL
Cinder has combinators defined for all elements and attributes of its modeled
markups (currently HTML and SVG), as well as inline combinators for them. For 
example, the body of a simple web page could be specified like this:

```haskell
myBody = markup ! body ! bgcolor "lightblue"
                    !> h1 !: "Cheers"
                    !< p !: "Visit Margarita!" ! Complete
                !< Complete
```

A single `!` (or `!>`)signals that the following element or attribute is a child pf the
current element. `!:` signifies content follows. `!` followed by the constructor
`Complete` closes the current element and causes following markup to belong to the 
immediate parent. `!<` is equivalent to `! Complete !`, that is it closes the 
current element and takes starts right in on new markup. As a special case 
`!< Complete` closes all pending elements.

Further, complete sections of markup may be added by `!+` (or `!+<` or `!+>`).
Convenience functions are available to add complete tags with commonly used attributes
in this manner.  For instance `markup ! img ! src "f.png" ! alt "f"` is exactly
equivalent to `imgSA "f.png" "f"`.

Attribute constructors which may reasonably expect numeric arguments may be postfixed
by N as a convenience: `heightN 100` as opposed to `height (show 100)`.

### DOM
DOM manipulation happens mostly through `insert` which takes markup and a DOM
node and does the obvious. Various functionality is built on top of it, for zipping
lists of markup into lists of nodes, staggering the valyes of attributes common to
many nodes, etc.

The following, from one of the examples, deletes all animations, inserts a
special effect animation into each circle tag and staggers the animations to start
3/4s of a second apart. (`aADR` stands for "animate an Attribute for a Duration
for a certain number of Repetitions", while bounce and settle are FX).

```haskell
grower = do
            root >>= byTag "animate" >>= mapM_ deleteSelf
            root >>= byTag "circle" >>= zipInsert an
            root >>= byTag "animate" >>= stagger "begin" 9 0.75
            return ()
    where
        an = zipWith (!+) (repeat (aADR "r" 0.75 1 ! fill "freeze"))
                            [bounce 0.5 5 25 50, settle 0.45 5 25 50,
                             bounce 0.85 6 25 50, settle 0.75 6 25 50]
```

###SVG
Cinder started out as an SVG library, before it was realized that it could be generally useful.
All the examples but one are in SVG and almost no convenience funcs beyond combinators
that accept numbers have been yet written for HTML.
