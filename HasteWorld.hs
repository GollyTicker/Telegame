
--run with $ hastec --output-html HasteWorld.hs

import Haste
import Haste.DOM
import Haste.Events
import Pages
import Base
import View (fromString)
import Maps hiding (main)
import qualified Data.Map as M
import System.IO.Unsafe
import qualified Data.MultiSet as MS
-- run with:
{-
hastec --output-html HasteWorld.hs && xdg-open HasteWorld.html
-}

tell :: Show a => a -> a
tell x = (unsafePerformIO $ newTextElem ("Trace:"++ show x) >>= appendChild documentBody) `seq` x

-- testing string output
ged = BCT (Blank,EnvStays,Blank) M.empty  (one (Initiated  JumpUR,mkP True))
bla = show $ View.fromString (tell ",,") -- \nS,S,S,S"
-- ERROR occurs when adding a third cell.... :/

mkwP = (BC (MS.singleton (mkP True)) MS.empty Solid)
bla2 = M.fromList [((0,'A'),mkwP)] --,((0,'B'),),((0,'C'),S),((0,'D'),S),((1,'A'),),((1,'B'),),((1,'C'),S),((1,'D'),S),((2,'A'),_),((2,'B'),),((2,'C'),),((2,'D'),S),((3,'A'),),((3,'B'),),((3,'C'),),((3,'D'),S),((4,'A'),),((4,'B'),),((4,'C'),),((4,'D'),S),((5,'A'),tx0 tx1),((5,'B'),S),((5,'C'),S),((5,'D'),S),((6,'A'),D00),((6,'B'),S),((6,'C'),S),((6,'D'),S),((7,'A'),),((7,'B'),S),((7,'C'),S),((7,'D'),S)]


-- main =  alert $ Haste.toJSString bla

main = do
    text1     <- newTextElem "this "
    text2     <- newTextElem "is "
    hist      <- newTextElem . show $ bla
    text3     <- newTextElem "the first "
--    text4     <- newTextElem "try"
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
    
    

