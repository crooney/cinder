###Bounce Example

Explained below.

<embed height="625" width="400" id="EXAMPLE" name="EXAMPLE" src="examples/Bouncer.svg"/>

This example drmonstrates how markup may be generated to make short work of fairly
complex SVG.

```haskell
bouncer = do
    root >>= insert circ
    root >>= byTag "circle" >>= stagger "cx" 50 100
    root >>= byTag "animate" >>= zipInsert fx >>= stagger "begin" 0 2
    return ()
    where
        circ = foldl1 (!+) $ map go colors
        go x = cRXY 25 50 100 ! fill x !+ aADR "cy" 2.0 1 ! fill "freeze"
                    ! begin "indefinite" !< Complete
        fx = [bounce 0.4 3 100 400, settle 0.35 3 100 400,
              bounce 0.75 4 100 400, settle 0.65 4 100 400]
        colors = ["lightpink", "lightblue", "lightsalmon", "lightgreen"]
```

`go` marries a simple circle definition to a color and then sets it to animate its vertical
position (`cy`), without specifying how it should animate. The resulting four
circles are then inserted into the document's root `Node`. A list of the circle
`Node`s is retrieved and their horizontal positions (`cx`) staggered to make a row.
A list of `Markup`, created by effects functions bounce and settle, is applied to
the animation `Node`s and their start times staggered. And that's it.

The expansion is similar but works on the radius (`r`) attribute:

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
