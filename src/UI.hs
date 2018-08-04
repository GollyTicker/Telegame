
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Foldable
import qualified Data.MultiSet as MS
import qualified Data.Map as M

import Base
import View()
import GameState
import Maps hiding (main)

import GraphicalView


-- run with:
{-
in folder src:
$ hastec --output-html UI.hs && ./upload.sh && xdg-open "https://n.ethz.ch/~ssahoo/Telegame/UI.html"

What to do, when you need some Haste functionality:
. Is it mentioned in the ovewview? https://github.com/valderman/haste-compiler
. Then local Haste-Documentation:
  file:///home/swaneet/haste-install/docs/index.html
. Then Examples: https://github.com/valderman/haste-compiler/tree/master/examples
. as well as pong.hs: https://github.com/iffyio/pong.hs/blob/master/pong.hs

. perhaps use twinch in X,Y or diagonal direction to scroll trhough spacetime?
that would be simple but powerful enough!
twinching inwards means going back in time.
twinching outwards means going into the future.


. perhaps use haste-perch or haste-markup?
. http://hackage.haskell.org/package/haste-perch
. https://github.com/ajnsit/haste-markup

. use shakespear templates? https://www.yesodweb.com/book/shakespearean-templates

SVG Basics on MDN: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Getting_Started
. using SVG due to tracking of elements. then we have everything
  as an Haste.DOM.Elem, and thus we can attach onEvent triggers to them.
-}

{- 0 -> "00", 221 -> "DD", 255 -> "FF"-}
intToHexStr :: Int -> String
intToHexStr n = let (l,r) = divMod n 16 in toHex l ++ toHex r

main :: IO ()
main = do
    title <- newElem "h4" `with` [prop "innerHTML" =: "Telegame Prototype (under construction)"]
    appendChild documentBody title
    
    css <- newElem "link" `with` [ attr "href" =: "UI.css",
        attr "rel" =: "stylesheet", attr "type" =: "text/css"]
    appendChild documentHead css

    svg <- mkSVGenv documentBody
    
    holder <- newSVGElem "g"
    
    let size = 75
        blockInf = Info{parent=holder,sc=(size,size),pd=(0,0),brd=Just 1,tr=undefined}
        f i j = (i*(size+5)+15,j*(size+5)+15)
    
    _ <- Key            `drawWith` blockInf{tr=f 0 0}
    _ <- TOrb 'a' 0     `drawWith` blockInf{tr=f 0 1}
    _ <- TOrb 'a' 1     `drawWith` blockInf{tr=f 0 2}
    
    _ <- Solid          `drawWith` blockInf{tr=f 1 0}
    _ <- Blank          `drawWith` blockInf{tr=f 1 1}
    _ <- Platform       `drawWith` blockInf{tr=f 1 2}
    _ <- Door 2 1       `drawWith` blockInf{tr=f 1 3}
    _ <- Door 2 2       `drawWith` blockInf{tr=f 1 4}
    _ <- Switch True    `drawWith` blockInf{tr=f 1 5}
    _ <- Switch False   `drawWith` blockInf{tr=f 1 6}
    
    let players = [
            Player "L" True MS.empty,
            Player "N" False (MS.fromList [Key,TOrb 'b' 1,TOrb 'b' 0]),
            Player "K" True (MS.fromList [TOrb 'c' 0, Key])
          ]
    
    _ <- players!!0 `drawWith` blockInf{tr=f 2 0}
          
    _ <- players!!1 `drawWith` blockInf{tr=f 2 1}
          
    _ <- players!!2 `drawWith` blockInf{tr=f 2 2}
    
    _ <- BC (MS.fromList (take 2 players)) (MS.fromList [Key,TOrb 'a' 0]) Platform
          `drawWith` blockInf{tr= f 3 0}
    
    _ <- BC (MS.fromList (init players)) (MS.empty) (Switch True)
          `drawWith` blockInf{tr= f 3 1}

    _ <- BC (MS.singleton (players!!1)) (MS.fromList [Key,TOrb 'd' 0,TOrb 'd' 1]) (Door 2 1)
          `drawWith` blockInf{tr= f 3 2}
    
    _ <- sobservations map2_P0_t0
          `drawWith` blockInf{tr=(f 4 0),sc=(800,800)}
              {- sc = bounds on the size of the drawn spacetime -}
    
    appendChild svg holder
    
    _ <- onEvent documentBody KeyDown $
      ( \_ -> {- todo: scroll by A/D or left/right arrow through spacetime. -}
              return () )
    
    return ()
    -- runCanvasExample
;

mkSVGenv docBody = do
  svgEl <- newSVGElem "svg" `with`
    [attr "version" =: "1.1", attr "width" =: "1500", attr "height" =: "850",
    attr "display" =: "block", attr "xmlns" =: svgNS]
  
  appendChild docBody svgEl
  
  r <- newSVGElem "rect" `with` -- border
    [attr "width" =: "100%", attr "height" =: "100%",
     attr "stroke-width" =: "3", attr "fill-opacity" =: "1",
     attr "stroke" =: rgbStr (rgb 0.1 0.1 0.1),
     attr "fill"   =: rgbStr (rgb 0.99 0.96 0.99)
    ]
  appendChild svgEl r
  return svgEl
;
{-
runCanvasExample = do
    cnvEl <- mkCanvas 200 100
    appendChild documentBody cnvEl

    Just cnv <- fromElem cnvEl
    let pic = do
        setFillColor (RGB 128 128 128)
        fill $ circle (10,10) 40
        text (60,60) "Hello you!"
    render cnv pic
;

mkCanvas :: Double -> Double -> IO Elem
mkCanvas w h = do
  canvas <- newElem "canvas"
  setProp canvas "width" (show w)
  setProp canvas "height" (show h)
  setStyle canvas "display" "block"
  setStyle canvas "border" "1px solid #524F52"
  return canvas
;-}

toHex :: Int -> String
toHex n = if n <= 9 then show n else [['A'..'F'] !! (n-10)]

rgbStr :: Color -> String
rgbStr (RGB r g b) = "#" ++ concatMap intToHexStr [r,g,b]
rgbStr (RGBA _r _g _b _) = undefined {- todo: support -}

rgb :: Double -> Double -> Double -> Color
rgb r g b
  | valid = let [r',g',b'] = map (round . (*255)) [r,g,b] in RGB r' g' b'
  | otherwise = RGB 0 0 0
  where valid = all (\d -> 0 <= d && d <= 1) [r,g,b]
;


