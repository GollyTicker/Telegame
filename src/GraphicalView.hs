{-# LANGUAGE NamedFieldPuns,FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module GraphicalView (
     svgNS
    ,newSVGElem
    ,Info(..)
    ,dflt
    ,Draw(..)
    ,under
    ,tell
    ,tellW
    
  ) where

import BaseBlock
import ViewBase()
import Data.Foldable
import qualified Data.Map as M

import Control.Monad (zipWithM_,void)
import Haste.DOM hiding (with)
import qualified Haste.DOM as HD

-- to add SVG elements with the proper namespace
import Haste.Prim (toJSStr)
import Haste.Foreign (ffi)
import Control.Monad.IO.Class

import System.IO.Unsafe -- debug tracing


tellW :: Show a => a -> b -> b
tellW x z = (unsafePerformIO $
  do p <- newElem "p" `under` documentBody
     newTextElem ("Trace:"++ show x) `under` p) `seq` z

tell :: Show a => a -> a
tell x = tellW x x

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

dflt = Info{parent=undefined,tr=(0,0),pd=(0,0),sc=(1,1),brd=Nothing,t0=0,t=0,t1=1}

data Info = Info {
    parent :: Elem {- element to attach it as a child -}
   ,tr     :: (Double,Double)
   ,pd     :: (Double,Double)
   ,sc     :: (Double,Double)
   ,brd    :: Maybe Double
   {- the following attrs. are only animation relevant -}
   ,t      :: Double {- 0 <= t0 <= t <= t1 <= 1. draw frame t of transition object t. -}
   ,t0     :: Double
   ,t1     :: Double
   {- start and end of animation. e.g. used,
   when an object is thrown through multiple blocks -}
}  deriving (Show)
{- style information? -}
instance Show Elem where show _ = "<elem>"
{- orphan instanve only for convinience -}

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
  drawWith x info@Info{tr,sc,brd} = do
    e <- draw info x `transform` info
    maybe (return ()) (\b -> void $ newSVGElem "rect"
      `with` ([attr "width" =: px (fst sc), attr "height" =: px (snd sc),
        attr "x" =: px (fst tr), attr "y" =: px (snd tr),
        attr "stroke-width" =: show b,attr "stroke" =: "#333",
        attr "fill-opacity" =: "0",attr "stroke-opacity" =: "0.5"])
      `under` parent info) brd
    return e
    where px = (++"px") . show
  
  draw :: Info -> a -> IO Elem {- simpler version to implement -}
    {- an svg elem in the coordinate-domain {0..1} x {0..1}.
    IMPORTANT: WE MEAN (1,1) TO BE BOMTTOM-RIGHT OF (0,0) !!
    it is expected to be scaled and transformed by the caller.
    only uses parent node to attach itself
  -}
;

with = HD.with
infixl 9 `with`
infixl 8 `under`

unitBB = unitBBf 0 0

unitBBf :: Double -> Double -> [Attribute]
unitBBf x y = [attr "x" =: show x, attr "y" =: show y,
    attr "width" =: "1", attr "height" =: "1"]
;

addSVG prnt fp = do
    e <- newSVGElem "use" `with` unitBB `under` prnt
    set e [attr "href" =: (fp++"#layer1")]
    return e
;
{- it seems VERY difficult to access the inner SVGs contents
though javascript of css. thus, instead I will be building
the images bottom up from parts -}
instance Draw PhyObj where
  draw info Key = parent info `addSVG` "key.svg"
  draw info (TOrb c i) = do
    g <- newSVGElem "g" `under` parent info
    _ <-  g`addSVG` ("teleorb-"++show i++".svg")
    _ <- newSVGElem "circle" `with` [
        attr "cx" =: "0.5", attr "cy" =: "0.5", attr "r" =: "0.25",
        attr "fill" =: maybe "black" id (M.lookup c teleOrbCMap)
      ] `under` g
    return g
;

teleOrbCMap = M.fromList $ zip
  "abcdef"
  ["blue","yellow","red","green","purple","orange"]

{- stacks all drawings in the list.
uses size sc(...) for the drawings and
start at top-left corner tr(...).
the (dx,dy) denotes, how successive elements are stacked.
e.g. (1,0) for horizontal stacking.
(0,2) for vertical with 1-elmt-space inbetween.
f is the drawing function-}
stack :: Int {- starting index -} -> (Double,Double) -> Info -> [a] -> (a -> Info -> IO b) -> IO ()
stack i0 (dx,dy) inf@Info{tr=(tx,ty),sc=(sx,sy),pd=(px,py)} xs f =
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
    
    let stk = dflt{parent=g,tr=(0.1,0.63),sc=(0.25,0.25),pd=(0.0,0.02),brd=Nothing}
    stack 0 (0,-1) stk [1  ..h] (\_ inf -> addSVG g "keyinside.svg" `transform` inf)
    stack h (0,-1) stk [h+1..n] (\_ inf -> addSVG g "keyhole.svg"   `transform` inf)
    
    return g
  
  draw info (Switch b) = parent info `addSVG`
                if b then "switch-on.svg" else "switch-off.svg"
;

instance Draw Player where
  draw info (Player s op inv) = do
    g <- newSVGElem "g" `under` parent info
    _ <- addSVG g (if op then "player-opened.svg" else "player-closed.svg")
      `transform` dflt{tr=(0,0.3),sc=(0.7,0.7),brd=Nothing}
    let stk = dflt{parent=g,tr=(0.7,0.1),sc=(0.28,0.28),pd=(0.0,0.03),brd=Nothing}
    stack 0 (0,1) stk (toList inv) drawWith
    txt <- newSVGElem "text" `with` (unitBBf 0.15 0.3 ++ [
        attr "font-size" =: "0.24", attr "fill" =: "black"
      ]) `under` g
    set txt [prop "innerHTML" =: s]
    return g
;

instance Draw BlockSt where
  draw info (BC e os ps) = do
    g <- newSVGElem "g" `under` parent info
    
    _ <- drawWith e dflt{parent=g,tr=(0.5,0),sc=(0.5,0.5),pd=(0,0),brd=Nothing}
    
    let osStk = dflt{parent=g,tr=(0.05,0.05),sc=(0.28,0.28),pd=(-0.15,-0.2),brd=Nothing}
    stack 0 (1,1) osStk (toList os) drawWith
    
    let psStk = dflt{parent=g,tr=(0.02,0.5),sc=(0.43,0.43),pd=(0.05,0.0),brd=Nothing}
    stack 0 (1,0) psStk (toList ps) drawWith
    
    return g
;

{- todo: add Draw String for text instance. -}

instance Draw a => Draw (Space a) where
  draw info mp = do
    g <- newSVGElem "g" `under` parent info
    if null mp
    then
      do t <- newSVGElem "text" `with` [
           attr "x" =: "0.2px", attr "y" =: "0.45px",
           attr "font-size" =: "0.4", attr "fill" =: "#777777",
           attr "textLength" =: "0.7"
           ] `under` g
         set t [prop "innerHTML" =: "empty"]
    else
      let {- 0 based -}
          nX = fromIntegral $ (1+) . maximum . map fst $ M.keys mp
          nY = fromIntegral $ (1+) . subtract (fromEnum 'A') . fromEnum . maximum . map snd $ M.keys mp
          pd = (0.005::Double,0.005::Double)
          mp'  = nestMap mp
          {-size availible for blocks-} 
          szX  = ( 1 - nX*fst pd ) / nX
          szY  = ( 1 - nY*snd pd ) / nY
          blkSz = min szX szY
          b = 2 / uncurry max (sc info) {- bordersize -}
          hrzStk = info{parent=g,tr=(0,0),sc=(nX*blkSz,blkSz),pd,brd=Just b}
          vrtStk y = info{parent=g,tr=(0,y),sc=(blkSz,blkSz),pd,brd=Just b}
      in  stack 0 (0,1) hrzStk (M.toAscList mp')
            $ \(_y,ln) hinf -> stack 0 (1,0)
              (vrtStk (snd . tr $ hinf))
              (M.toAscList ln)
              $ \(_x,blk) vinf -> blk `drawWith` vinf
    return g
;

nestMap :: (Ord ka, Ord kb) => M.Map (ka,kb) a -> M.Map kb (M.Map ka a)
nestMap = M.foldlWithKey' f M.empty where
  f m (x,y) a
    | y == y    = M.insertWith M.union y (M.singleton x a) m {- todo -}
    | otherwise = m

{- todo: generate color coding of player names.
more easily recognized in game.-}

instance Draw (PhyObj,PhyObjT) where
  draw Info{t0,t,t1} (_o,_ot) = undefined
;

instance Draw EnvT where
  draw _info _envt = return undefined
;

instance Draw PlayerAction where
  draw _info _pa = return undefined
;

instance Draw PlayerT where
  draw _info _pt = return undefined
;

instance Draw BlockTr where
  draw _info _bct = return undefined

instance Draw ConsHistory where
  draw info _ch = newSVGElem "g" `under` parent info {- todo -}

{-
class DrawT a where
  drawt :: a -> Info -> Double -> IO Elem
  x
;

-}



