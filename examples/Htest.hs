module Htest (main) where

import Prelude hiding (id,div)
import FFI
import Cinder.HTML
import Cinder.HTML.Elements
import Cinder.HTML.Attributes hiding (style)

main :: Fay ()
main = addEventListener "load" htest False

htest :: Fay ()
htest = byId "BODY" >>= insert mu
        >> byId "HEAD" >>= insert st
        >> putStrLn (pretty st)
        >> putStrLn (pretty mu)
    where mu = markup
                ! div ! id "foo"
                    ! p !+ iC "hello " !+ bC "there" !< Complete
                !< h1 !: "someshit" !< h4 !: "othershit"
                !<+ dCC "classy" "some lovely content in a div"
                !<+ pCC "classy" "and some in a classed p"
                !< Complete
          st = markup ! style ! typeA "text/css"
                !: ".classy {border: solid blue 2px;}" ! Complete

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
