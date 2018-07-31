
module GraphicalView (
     svgNS
    ,newSVGElem
    ,Info(..)
    ,Draw(..)
  ) where

import Base
import Haste.DOM
import Haste.Events
import Control.Monad

-- to add SVG elements with the proper namespace
import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class

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

setNS :: String -> Elem -> String -> String -> IO Elem
setNS ns = ffi . toJSStr $ "(function(e,p,v){e.setAttributeNS("++show xlinkNS++",p, v);})"

data Info = Info { parent :: Elem } {- element to attach it as a child -}
{- style information? -}

class Draw a where
  drawsvg :: Info -> a -> IO Elem
  {- an svg elem in the coordinate-domain {0..1} x {0..1}.
  it is expected to be scaled and transformed by the caller
  -}
;

unitBB = [attr "x" =: "0", attr "y" =: "0",
    attr "width" =: "1", attr "height" =: "1"]

instance Draw PhyObj where
  drawsvg info Key = do
    e <- newSVGElem "image" `with` unitBB
    setNS xlinkNS e "xlink:href" "key.svg"
    appendChild (parent info) e
    return e
;









