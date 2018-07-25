
--run with $ hastec --output-html HasteWorld.hs

import Haste hiding (fromString)
import Haste.DOM
import Haste.Events
import Pages
import Base
import View()
import qualified View as View
import Maps hiding (main)
import qualified Data.Map as M
import System.IO.Unsafe
import qualified Data.MultiSet as MS
-- run with:
{-
hastec --output-html HasteWorld.hs && xdg-open HasteWorld.html
-}

tell :: Show a => a -> a
tell x = (unsafePerformIO $ newTextElem ("Trace:"++ show x++"  ") >>= appendChild documentBody) `seq` x

-- testing string output
myoutput = show $ map2_GS

main = do
{-
    text1     <- newTextElem "this "
    text2     <- newTextElem "is "
    text3     <- newTextElem "the first "
    text4     <- newTextElem "try"
    topRow    <- newElem "div"  -- Container for the top row
    bottomRow <- newElem "div"  -- Container for the bottom row
    row topRow [text1,text2]
    row bottomRow [text3,text4]
    column documentBody [topRow,bottomRow]
    
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
    hist      <- newElem "textarea" `with`
      [prop "innerHTML" =: myoutput,
       prop "rows" =: "50", prop "cols" =: "330"]
    cont <- newElem "div"
    appendChild cont hist
    appendChild documentBody cont
    

