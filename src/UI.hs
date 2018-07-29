
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Base
import View()
import GameState
import Maps hiding (main)

-- to add SVG elements with the proper namespace
import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class

import System.IO.Unsafe -- debug tracing

svgNS = "http://www.w3.org/2000/svg"

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

. perhaps use haste-perch or haste-markup?
. http://hackage.haskell.org/package/haste-perch
. https://github.com/ajnsit/haste-markup

SVG Basics on MDN: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Getting_Started
. using SVG due to tracking of elements. then we have everything
  as an Haste.DOM.Elem, and thus we can attach onEvent triggers to them.
-}
example :: String
example = show (map2_GS) ++ "\n\nWith contradictions: " ++ show (contradictions $ gsch map2_GS)

-- Function useful for debugging print-stmts.
tell :: Show a => a -> a
tell x = (unsafePerformIO $ (newTextElem ("Trace:"++ show x++"<br>") `with` [prop "display" =: "block"]) >>= appendChild documentBody) `seq` x

-- haste doesn't support Elements from namesapces different than HTML.
-- here I simply reproduce 'Haste.DOM.newElem' with this functionality
newSVGElem :: MonadIO m => String -> m Elem
newSVGElem = liftIO . ffi (toJSStr ffiCode)
  where
    ffiCode = "(function(t){return document.createElementNS("++show svgNS++",t);})"
;


{- 0 -> "00", 221 -> "DD", 255 -> "FF"-}
intToHexStr :: Int -> String
intToHexStr n = let (l,r) = divMod n 16 in toHex l ++ toHex r

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

main :: IO ()
main = do
    title <- newElem "h4" `with` [prop "innerHTML" =: "Telegame Prototype"]
    appendChild documentBody title

    svgEl <- newSVGElem "svg" `with`
      [attr "version" =: "1.1",
      attr "width" =: "400",
      attr "height" =: "400",
      attr "display" =: "block",
      attr "xmlns" =: svgNS]
    
    r <- newSVGElem "rect" `with`
      [attr "width" =: "100%",
       attr "height" =: "100%",
       attr "fill" =: rgbStr (rgb 0.2 0.3 0.8)
      ]
    
    appendChild svgEl r
    appendChild documentBody svgEl
    
    onEvent r Click $
      ( \_ -> set r [attr "fill" =: rgbStr (rgb 0.1 0.9 0.4)] )
    
    runCanvasExample
;

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
;
