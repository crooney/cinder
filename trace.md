###Trace Example

<embed height="125" width="400" id="EXAMPLE" name="EXAMPLE" src="examples/Nifty.svg"/>

*Note: This example misbehaves under Chromium for understood reasons, which I
believe constitute a bug in Chromium. A workaround is known. but would be very complex 
for this piece and render it unfit as an example. Please view in Firefox for correct behaviour*

This is the only example that doesn't start with an empty SVG file. I made it in
Inkscape, converting the text to paths. The animation code is here:

```haskell
nifty = root >>= byTag "path" >>= mapM (tracePath 0.5 1)
        >>= stagger "begin" 0 0.5 >> return ()
```

After retrieving the document's root `Node`, it asks for a list of path Nodes and
calls `tracePath` for each of them, with a duration of a half-second. `tracePath` is impure, because it has to query
the node for its length. The nodes are then scheduled to start
a half second apart, and that's it.

Note `mapM` is not part of the (Fay) `Prelude` but is one of several functions
from `Control.Monad` for which Cinder provides monomorphic equivalents in Control.Fay.

Reload to restart.
