###Bounce Example

Explained below.

<embed height="625" width="400" id="EXAMPLE" name="EXAMPLE" src="examples/Bouncer.svg"/>

This example demonstrates how markup may be generated to make short work of fairly
complex SVG.

```haskell
bouncer = do
    g <- node "g" >>= insert circ
    byTag "circle" g >>= stagger "cx" 50 100
    byClass "drop" g >>= zipInsert fxd >>= stagger "begin" 0 2
    byClass "grow" g >>= zipInsert fxg >>= stagger "begin" 9 0.75
    root >>= setChild g
    return ()
    where
        circ = foldl1 (!+) $ map go colors
        go x = cRXY 25 50 100 ! fill x
                    !+ aADR "cy" 2.0 1 ! fill "freeze" ! classA "drop"
                    !<+ aADR "r" 0.75 1 ! fill "freeze" ! classA "grow"
               !< Complete
        fxd = [bounce 0.4 3 100 400, settle 0.35 3 100 400,
               bounce 0.75 4 100 400, settle 0.65 4 100 400]
        fxg = [bounce 0.5 5 25 50, settle 0.45 5 25 50,
               bounce 0.85 6 25 50, settle 0.75 6 25 50]
        colors = ["lightpink", "lightblue", "lightsalmon", "lightgreen"]
```

First a `g` (group) SVG node is created and four circles of pleasing colors added to it,
along with animations of their radii and y coordinates, with only duration and repeat count specified.
Note the second call to `aADR` follows a `!<+` which closes the previous one.
The circles are then taken from the DOM into a list and their x coordinates staggered.
Next previously unspecified portions of the animations (time, splice, and value arrays),
are inserted directly into their respective `animate` nodes via `zipInsert`.
Their start times are staggered, and the `g` node containing the whole shmear inserted
into the document root, and happy animation obtains.

It may be edifying to look at the DOM in Firebug or Developer Tools.

Note that the use of the grouping node is only necessary for Chromium, which I
believe has a bug.  This is the same thing that affects the trace example, but
may be more easily worked around here.
