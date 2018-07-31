
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module GraphicalView (
     svgNS
    ,newSVGElem
    ,Info(..)
    ,Draw(..)
  ) where

import Base
import Haste.DOM

-- to add SVG elements with the proper namespace
import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class

-- Reference: SVG scripting: https://developer.mozilla.org/en-US/docs/Web/SVG/Scripting


-- https://stackoverflow.com/questions/6893391/adding-image-namespace-in-svg-through-js-still-doesnt-show-me-the-picture
xlinkNS = "http://www.w3.org/1999/xlink"
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
   ,sc     :: (Double,Double)
} 
{- style information? -}

class Draw a where
  drawWith :: a -> Info -> IO Elem {- transforms the standardized implementaiton -}
  drawWith x i@(Info _ (tx,ty) (sx,sy)) = do
    e <- draw i x
    set e [attr "transform" =: (concat
      ["translate(",show tx,",",show ty,"),",
       "scale(",show sx,",",show sy,")"])]
    return e
  
  draw :: Info -> a -> IO Elem {- simpler version to implement -}
    {- an svg elem in the coordinate-domain {0..1} x {0..1}.
  it is expected to be scaled and transformed by the caller
  -}
;

unitBB = [attr "x" =: "0", attr "y" =: "0",
    attr "width" =: "1", attr "height" =: "1"]

fromFile prnt fp = do
    e <- newSVGElem "use" `with` unitBB
    set e [attr "href" =: (fp++"#layer1")]
    appendChild prnt e
    return e
;
{- it seems VERYdifficult to access the inner SVGs contents
though javascript of css. thus, instead I will be building
the images bottom up from parts -}
instance Draw PhyObj where
  draw info Key = fromFile (parent info) "key.svg"
  draw info (TOrb _c i) = do
    g <- newSVGElem "g"
    _ <- fromFile g ("teleorb-"++show i++".svg")
    --ffi (toJSStr "(funciton())"); change character and inner+middle circle for torb visualization
    appendChild (parent info) g
    return g









