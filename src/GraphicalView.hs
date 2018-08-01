{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module GraphicalView (
     svgNS
    ,newSVGElem
    ,Info(..)
    ,Draw(..)
  ) where

import Base
import ViewBase()
import Data.Foldable

import Control.Monad (zipWithM_)
import Haste.DOM

-- to add SVG elements with the proper namespace
import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class

-- appends first element as child of second element
-- and returns the first element
under :: IO Elem -> Elem -> IO Elem
x `under` a = do xe <- x; appendChild a xe >> return xe

-- Reference: SVG scripting: https://developer.mozilla.org/en-US/docs/Web/SVG/Scripting

-- https://stackoverflow.com/questions/6893391/adding-image-namespace-in-svg-through-js-still-doesnt-show-me-the-picture

svgNS = "http://www.w3.org/2000/svg"

-- haste doesn't support Elements from namesapces different than HTML.
-- here I simply reproduce 'Haste.DOM.newElem' with this functionality
newSVGElem :: MonadIO m => String -> m Elem
newSVGElem = liftIO . ffi (toJSStr ffiCode)
  where
    ffiCode = "(function(t){return document.createElementNS("++show svgNS++",t);})"
;

data Info = Info {
    parent :: Elem {- element to attach it as a child -}
   ,tr     :: (Double,Double)
   ,pd     :: (Double,Double)
   ,sc     :: (Double,Double)
} 
{- style information? -}

transform :: IO Elem -> Info -> IO Elem
transform me Info{tr=(tx,ty),sc=(sx,sy)} = do
  e <- me
  set e [attr "transform" =: (concat
    ["translate(",show tx,",",show ty,"),",
     "scale(",show sx,",",show sy,")"])]
  return e
;

class Draw a where
  drawWith :: a -> Info -> IO Elem {- transforms the standardized implementaiton -}
  drawWith x inf = transform (draw inf x) inf
  
  draw :: Info -> a -> IO Elem {- simpler version to implement -}
    {- an svg elem in the coordinate-domain {0..1} x {0..1}.
  it is expected to be scaled and transformed by the caller.
  only uses parent node to attach itself
  -}
;

unitBB = unitBBf 0 0

unitBBf :: Double -> Double -> [Attribute]
unitBBf x y = [attr "x" =: show x, attr "y" =: show y,
    attr "width" =: "1", attr "height" =: "1"]
;

addSVG prnt fp = do
    e <- newSVGElem "use" `with` unitBB
    set e [attr "href" =: (fp++"#layer1")]
    appendChild prnt e
    return e
;
{- it seems VERY difficult to access the inner SVGs contents
though javascript of css. thus, instead I will be building
the images bottom up from parts -}
instance Draw PhyObj where
  draw info Key = parent info `addSVG` "key.svg"
  draw info (TOrb c i) = do
    g <- newSVGElem "g"
    _ <-  g`addSVG` ("teleorb-"++show i++".svg")
    txt <- newSVGElem "text" `with` (unitBBf 0.39 0.61 ++ [
        attr "font-size" =: "0.43px", attr "fill" =: "black"
      ])
    set txt [prop "innerHTML" =: (init.tail) (show c)]
    appendChild g txt
    -- todo: make eventually the torb change color depending on t-char.
    appendChild (parent info) g
    return g
;

{- stacks all drawings in the list.
uses size sc(...) for the drawings and
start at top-left corner tr(...).
the (dx,dy) denotes, how successive elements are stacked.
e.g. (1,0) for horizontal stacking.
(0,2) for vertical with 1-elmt-space inbetween.
f is the drawing function-}
stack :: Int {- starting index -} -> (Double,Double) -> Info -> (a -> Info -> IO b) -> [a] -> IO ()
stack i0 (dx,dy) inf@Info{tr=(tx,ty),sc=(sx,sy),pd=(px,py)} f xs =
  zipWithM_ (\i x -> let i' = fromIntegral i in
    f x inf{tr=(tx + i'*dx*(sx+px), ty + i'*dy*(sy+py)),sc=(sx,sy)} )
    [i0..] xs

instance Draw Env where
  draw info Solid = parent info `addSVG` "solid.svg"
  draw info Blank = newSVGElem "g" `under` parent info
  draw info Platform = parent info `addSVG` "platform.svg"
  draw info (Door n h) = do
    g <- newSVGElem "g" `under` parent info
    let opened = h >= n
    _ <- g `addSVG` if opened then "door-opened.svg" else "door-closed.svg"
    
    let stk = Info{parent=g,tr=(0.1,0.63),sc=(0.25,0.25),pd=(0.0,0.02)}
    stack 0 (0,-1) stk (\_ inf -> addSVG g "keyinside.svg" `transform` inf) [1  ..h]
    stack h (0,-1) stk (\_ inf -> addSVG g "keyhole.svg"   `transform` inf) [h+1..n]
    
    return g
  
  draw info (Switch b) = parent info `addSVG`
                if b then "switch-on.svg" else "switch-off.svg"
;

instance Draw Player where
  draw info (Player s op inv) = do
    g <- newSVGElem "g"
    _ <- addSVG g (if op then "player-opened.svg" else "player-closed.svg")
      `transform` Info{tr=(0,0.3),sc=(0.7,0.7)}
    let stk = Info{parent=g,tr=(0.7,0.05),sc=(0.33,0.33),pd=(0.1,0.0)}
    stack 0 (0,1) stk drawWith (toList inv)
    parent info `appendChild` g
    txt <- newSVGElem "text" `with` (unitBBf 0.15 0.3 ++ [
        attr "font-size" =: "0.24px", attr "fill" =: "black"
      ])
    set txt [prop "innerHTML" =: s]
    appendChild g txt
    return g
;


{- todo: generate color coding of player names and teleorb-chars.
more easily recognized in game.-}





