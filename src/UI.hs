{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import Haste.DOM
import Haste.JSString
import Haste.Events
import Haste.Graphics.Canvas
import Base
import View()
import GameState
import Maps hiding (main)

import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class
import System.IO.Unsafe

svgNS = "http://www.w3.org/2000/svg"
-- run with:
{-
hastec UI.hs
generates UI.html

Haste-API reference https://haste-lang.org/docs/haddock/0.5.5/index.html

Examples: https://github.com/valderman/haste-compiler/tree/master/examples

SVG on MDN: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Getting_Started
. has good support
-}

mkCanvas :: Double -> Double -> IO Elem
mkCanvas w h = do
  canvas <- newElem "canvas"
  setProp canvas "width" (show w)
  setProp canvas "height" (show h)
  setStyle canvas "display" "block"
  setStyle canvas "border" "1px solid #524F52"
  return canvas
;

-- testing string output
myoutput = show (map2_GS) ++ "\n\nWith contradictions: " ++ show (contradictions $ gsch map2_GS)


    
{- from Haste-Compiter Source (https://github.com/valderman/haste-compiler/blob/2614e1d112495a611bb27d88123616e50f66428f/libraries/haste-lib/src/Haste/DOM/JSString.hs )
new know that: Haste.DOM will be shortened as H.D

H.D.newElem = H.D.JSString.newElem . toJSStr
H.D.JSString = liftIO . jsCreateElem
jsCreateElem = ffi "(function(t){return document.createElement(t);})"


thus, we can now define newSVGElem as:
-}
tell :: Show a => a -> a
tell x = (unsafePerformIO $ (newTextElem ("Trace:"++ show x++"<br>") `with` [prop "display" =: "block"]) >>= appendChild documentBody) `seq` x

newSVGElem :: MonadIO m => JSString -> m Elem
newSVGElem s = liftIO $ ffi (tell (toJSStr $ ffiCode)) $ tell s
  where
    ffiCode = "(function(t){alert(\"Called with: \"+t); return document.createElementNS("++show svgNS++",t);})"
   
main = do
    title <- newElem "h4" `with` [prop "innerHTML" =: "Telegame Prototype"]
    appendChild documentBody title
    
    cnvEl <- mkCanvas 400 600
    --appendChild documentBody cnvEl

    --Just cnv <- fromElem cnvEl
    let pic = do
        setFillColor (RGB 128 128 128)
        fill $ circle (100,100) 40
        text (10,10) "Hello you!"
    --render cnv pic
    
    {- or perhaps use SVG? they are good for detecting mouse-clicks etc -}
    svgEl <- newSVGElem "svg" `with`
      [attr "version" =: "1.1",
      attr "width" =: "400",
      attr "height" =: "400",
      attr "display" =: "block",
      attr "xmlns" =: svgNS]
      
    -- todo: doesn't render, because haste uses html namespace which
    -- doesn't work for svg.
    
    -- use FFI in Haste to call createElementNS?
    
    rect <- newSVGElem "rect" `with`
      [attr "width" =: "100%", attr "height" =: "100%", attr "fill" =: "red" ]
    
    appendChild svgEl rect
      
    appendChild documentBody svgEl
;

