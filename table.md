## HTML Table

The table at the bottom of the page (Welcome to 1997!) was created on the fly by the following Fay:

````haskell
colors :: [[String]]
colors = go [ (r,g,b) | r <- s, g <- s, b <- s]
    where s = [0, 0x33 .. 0xff]
          go [] = []
          go xs = map rgb (take 24 xs) : go (drop 24 xs)

tableMU :: Markup
tableMU = markup ! table !+ foldl1 (!+) (map row colors) ! Complete
    where row x = markup ! tr !+ foldl1 (!+) (map box x) ! Complete
          box x = markup ! td ! sty x ! inner "&#955;" ! Complete
          sty x = style ("height:20px;width:20px;background-color:" ++ x)

htex :: Fay ()
htex = root >>= byTag "body" >>= zipWithM_ insert [tableMU]
````

_(If you don't understand the `!` and `!+` stuff, look at the SVG examples and the README.
This example just demonstrates the applicability to HTML.)_

I won't bother to explain the HTML or the Haskell to you, as, if you're reading this,
you're probably more expert than I am at both.

The are only a few points of note as regards Cinder:

  + The &#955; is injected via `inner`, which is a convenience for
    `Property "innerHTML"`, as opposed to just adding the lambda
    as content using `!:` or `Content`. `inner` adds the unescaped entity while
    `!:` would insert the string "\&#955;".<br><br>
  + `zipWithM_ insert` is used because `byTag` returns a `Node` list. Here, we bump our
    one element list of `Markup` against what we hope to be the only
    'body' element. Note that `zipWithM_` is not in the (Fay) `Prelude`, but is
    provided in Cinder's `Control.Fay`.<br><br>
  + The &#955; could have been injected into all the `td`s by several different
    methods, even after the initial `Markup` insertion, e.g.:
    `root >>= byTag "td" >>= mapM_ (setProperty "innerHTML" "&#955;")`

And, after the footer, behold our amazing technicolor dreamtable:

<script src="examples/Table.js"></script>
