###Morph Example

Explained below.

<embed height="500" width="500" id="EXAMPLE" name="EXAMPLE" src="examples/Morpher.svg"/>

This is the complete code for this example:

```haskell
morpher :: Fay ()
morpher = do
    root >>= insert mu
    return ()
    where
        mu = markup !+ pathD dFrom ! fill "cyan" !+ aADR "d" 5.5 9
            ! vs [showD dFrom,showD dTo,showD dFrom] !< Complete
        dFrom = [M 200 200, Q 510 10 320 200, Q 510 510 320 320,
                Q 10 510 200 320, Q 10 10 200 200]
        dTo = [M 10 10, Q 125 75 190 10, Q 125 125 190 190,
                Q 75 125 10 190, Q 75 75 10 10]
```
This simple example doesn't really have to be scripted, as it has no dynamic
portion that couldn't have been written in pure SVG. The markup is quite simple.
`pathD` creates a path element and populate its `d` attribute with a list of `Segment`s.
Using a list of `Segment` provides type safety and computability that the raw `d`
attribute, a string, lacks.

`aADR` creates an animation for the attribute `d` of 5.5 seconds that repeats 9 times
(ADR = attributeName, dur, repeatCount). `vs` makes a values attribute out of a list.
Finally, the `!< Complete` closes all open Elements, in this case `path` and `animate`.
