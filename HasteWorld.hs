
--run with $ hastec --output-html HasteWorld.hs

import Haste
import Haste.DOM
import Haste.Events
import Pages
import Base
import View
import Maps hiding (main)

-- run with:
{-
hastec --output-html HasteWorld.hs && xdg-open HasteWorld.html
-}

main = do
    text1     <- newTextElem "this "
    text2     <- newTextElem "is "
    hist      <- newTextElem . show $ mkP True-- (show map2_GS)
    text3     <- newTextElem "the first "
    text4     <- newTextElem "try"
    topRow    <- newElem "div"  -- Container for the top row
    bottomRow <- newElem "div"  -- Container for the bottom row
    row topRow [text1,text2]
    row bottomRow [text3,hist]
    column documentBody [topRow,bottomRow]
    {-
    input  <- newElem "input"
    button <- newElem "button"
    set input
        [ prop "type"  =: "text"
        , prop "size"  =: "30"  -- Width of box
        , prop "value" =: "Type your answer here..."
        ]
    set button [ prop "innerHTML" =: "Submit answer" ]
    column documentBody [input,button]
    
    onEvent button Click $ \_ -> do
      set input [ prop "value" =: "You clicked!" ]
      -}
    
    

