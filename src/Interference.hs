{-# LANGUAGE CPP,TupleSections,NamedFieldPuns,TypeFamilies,StandaloneDeriving,DeriveDataTypeable,FlexibleContexts,ScopedTypeVariables #-}
-- GHC CPP macros: https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/phases.html#standard-cpp-macros
-- https://guide.aelve.com/haskell/cpp-vww0qd72
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
#endif

module Interference(
     module BaseBlock
    {-
    ,interferesWith
    ,interferesWithT
    ,inferMinimal
    ,inferMinimalT
    ,adjustGlobalInfo
    ,isContradictionc-}
    
    ,This(..)
    ,is_a
    ,okc
    --,notokc
    ,Cons(..)
  )
  where

import BaseBlock
import Data.Data
import Control.Monad (foldM)
import ViewBase()
import qualified Data.Set as S
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Monoid
-- import Debug.Trace
-- import Control.Applicative

instance Block BlockSt where
  type OpenObs BlockSt = Specific (Space BlockSt)
  type ClosedObs BlockSt = Specific (Pos,BlockSt)
  data Cons BlockSt = BCC {
       bccps  :: MultiSet Player
      ,bccos  :: MultiSet PhyObj
      ,bccenv :: Maybe Env
    } deriving (Ord,Eq)
    -- OneP Player | OneO PhyObj | Bgrd Env
    -- => e.g. OneP p1 & OneP p1 & OneP p2 & Bgrd (Door 0 0) <=>
    -- BSCons MultiSet(p1,p1,p2) + Door 0 0 ==> ".P1. .P1. .P2. D00"
  type Antcpt BlockSt = Space (Maybe BlockSt)
  type Other BlockSt = BlockTr
  data This BlockSt = St
  this = St
  getter St = fst
  setter St cb = (cb,leastc)
  leastc = BCC MS.empty MS.empty Nothing
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos (BCC MS.empty MS.empty <$> envOnStandables)
  in_standable ths tpos = mkSTConsFromChoice ths tpos (BCC MS.empty MS.empty <$> envInStandables)
  permeable    ths tpos = mkSTConsFromChoice ths tpos (BCC MS.empty MS.empty <$> envPermeables  )
  
  selfconsistent _ = todo -- TODO: implement. required counting ability => BC_Cons and BCT_Cons
  interferesWithBlock = todo -- interferesWith
  
  -- find the block where the player is and reduce all to that block.
  reduceToClosed Proxy spo@(Specific _ player _ mp) = spo {sobservations = (pos, mp M.! pos)}
    where pos = case (M.toList . M.filter (any (player==) . bcps) $ mp) of ((p,_):_) -> p ; [] -> error "reduceToClosed: Player not found"
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

instance Block BlockTr where
  type OpenObs BlockTr = Specific (Space BlockTr)
  type ClosedObs BlockTr = Specific (Space BlockTr)
  data Cons BlockTr =
      BCTC {
        bctcEnv  :: (Maybe Env, Maybe EnvT, Maybe Env)
       ,bctcps :: MultiSet (Maybe Player,Maybe PlayerT,Maybe Player)
       ,bctcos :: MultiSet (Maybe PhyObj,Maybe PhyObjT,Maybe PhyObj)
    } deriving (Eq,Ord)
  type Antcpt BlockTr = Space (Maybe BlockTr)
  type Other BlockTr = BlockSt
  data This BlockTr = Tr
  this = Tr
  getter Tr = snd
  setter Tr cb = (leastc,cb)
  leastc = BCTC (Nothing,Nothing,Nothing) MS.empty MS.empty
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcEnv=(l,t,r)}) <$> envOnStandables <*> envOnStandablesT <*> envOnStandables
  
  in_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcEnv=(l,t,r)}) <$> envInStandables <*> envInStandablesT <*> envInStandables
    
  permeable    ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcEnv=(l,t,r)}) <$> envPermeables   <*> envPermeablesT   <*> envPermeables
  
    --fromBoolc (show (curr,Tr) ++ "requires remaining permeable for" ++ show reason) $
  selfconsistent _ = todo -- TODO: implement
  interferesWithBlock = todo -- interferesWithT
  
  -- get all block where the player is identified
  reduceToClosed Proxy spo@(Specific _ plyr _ mp) =   spo { sobservations = M.filter (any (\(p1,_,p2) -> p1 == plyr || p2 == plyr) . MS.toList . bctps) mp}
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

deriving instance Data Player
deriving instance Typeable Player
deriving instance Data PlayerAction
deriving instance Typeable PlayerAction
deriving instance Data PlayerT
deriving instance Typeable PlayerT
deriving instance Data ConsHistory
deriving instance Typeable ConsHistory
deriving instance Data a => Data (Specific a)
deriving instance Typeable a => Typeable (Specific a)
deriving instance Data BlockSt
deriving instance Typeable BlockSt
deriving instance Data BlockTr
deriving instance Typeable BlockTr
deriving instance Data GameState
deriving instance Typeable GameState

is_a :: (Block b,Block c) => This b -> This c -> Bool
ths `is_a` oth = typeOf ths == typeOf oth

envOnStandables = [Just Solid]
envOnStandablesT = [Nothing]

envInStandables = [Just Platform]
envInStandablesT = [Nothing]

envPermeables = Just <$> [Blank,Platform,Switch False,Switch True] ++ (do n <- [0..4]; h <- [0..n]; return (Door n h))
envPermeablesT = [Nothing]

mkGlobalCC :: CH_Global -> ConsDesc -> STCons
mkGlobalCC cchg str = STC [Leaf (cchg,M.empty,str)]

mkSTConsFromChoice :: Block b => This b -> TimePos -> [Cons b] -> ConsDesc -> STCons
mkSTConsFromChoice ths tpos bcs str = STC $ 
    do bc <- bcs
       let mp = M.singleton tpos $ setter ths bc
       return $ Leaf (unknownGlobal,mp,str)

mkSimpleSTConsWithGlobal :: Block b => This b -> TimePos -> CH_Global -> Cons b -> ConsDesc -> STCons
mkSimpleSTConsWithGlobal ths tpos cchg cb str =
  STC [Leaf (cchg,M.singleton tpos (setter ths cb),str)]

mkSimpleSTCons :: Block b => This b -> TimePos -> Cons b -> ConsDesc -> STCons
mkSimpleSTCons ths tpos cb str = mkSimpleSTConsWithGlobal ths tpos unknownGlobal cb str

{- y-axis is pointed downwards -}
-- is1 ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: (Show a,Block b) => This b -> TimePos -> a -> STCons
isGrounded ths curr@(t,(x,y)) reason =
  let below = (t,(x,succ y))
  in  in_standable ths curr (show (curr,ths) ++ " needs standable (cause: "++show reason++") in "++ show  curr)
      `orElse` on_standable ths below (show (curr,ths) ++ " needs standable (cause: "++show reason++") on "++ show below)
;

isPermeable :: (Block b,Show a) => This b -> TimePos -> a -> STCons
isPermeable ths curr reason = permeable ths curr (show (curr,ths) ++ " requires permeability (cause: " ++ show reason ++ ")")

{- connectives and neutral-elem for building STCons from others. -}
{- appended `c` for Cons b and ConsRes b related functions 
--leastc :: TimePos -> Cons a
--concretec :: TimePos -> Cons a -> Either ConsFail a
andc :: Cons a -> Cons a -> Either ConsFail (Cons a)
satisfiesc :: a -> Cons a -> Maybe ConsFail

concretec BCC{bccps,bccos,bccenv} =
  maybeToEither "No minimal env possible dueto unknown env." $ (\env -> BC {bcps = bccps, bcos=bccos, bcenv = env}) <$> bccenv 

concretec = todo
-}

-- notokc :: ConsFail -> ConsRes ConsHistoryP
-- notokc = Left

-- fromBoolc :: String -> Bool -> ConsRes ConsHistoryP
-- fromBoolc s b = if b then okc todo else Left s

--isContradictionc :: ConsHistoryP -> Bool
--isContradictionc = not . null

--unknownOkAndc :: (a -> ConsRes ConsHistoryP) -> Maybe a -> ConsRes ConsHistoryP
--unknownOkAndc = maybe (okc todo)

{-
alwaysOk <~> leastc
also <~> andc
orElse <~> choice via [...]
inferMinimal <~> concretec
-}


okc :: ConsHistoryP
okc = Leaf (unknownGlobal,M.empty,"ok")

alwaysOk :: STCons
alwaysOk = STC [okc]

orElse :: STCons -> STCons -> STCons
orElse a b = STC $ getSTCons a ++ getSTCons b

also :: STCons -> STCons -> STCons
also a b = STC $ And <$> getSTCons a <*> getSTCons b

-- all interferesWith functions check at the current BlockSt.
-- all interferesWithT functions check at the current BlockTr.
{-
interferesWith :: TimePos -> BlockSt -> STCons
interferesWith curr bc =
  ( playerInterferesWith curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjInterferesWith curr `foldMap` MS.toList (bcos bc))
  `also` ( envInterferesWith curr (bcenv bc))
;

interferesWithT :: TimePos -> BlockTr -> STCons
interferesWithT curr bct =
  ( playerInterferesWithT curr `foldMap` MS.toList (bctps bct) )
  `also` ( (\(o,ot) -> phyObjInterferesWithT curr o `foldMap` ot) `foldMap` M.toList (bctos bct))
  `also` ( envInterferesWithT curr (bctenv bct))
;

playerInterferesWithT :: TimePos -> (Player,PlayerT,Player) -> STCons
playerInterferesWithT curr p =
  let needsGroundCheck = runpat (const True) (\_ _ -> False) (const True) True (snd3 p)
      {- case (Completed pa): currently, all completed actions require a grounded place as dest. -}
      {- physical movement: movingNext = Nothing, if player doesn't move out of current block.
         Just Dir, if the player continues in direction Dir. time-inverse with Next<->From. teleport is not included here -}
      movingNext   = runpat toDirpa (\_ y->Just y) (const Nothing) Nothing (snd3 p)
      nextReqStr d = show (curr,Tr) ++ " requires a continuation of movement "     ++ show (snd3 p) ++ " to " ++ show (applyDir d curr,Tr)
      movingFrom   = runpat (const Nothing) (\x _->Just x) fromDirpa (Just U) (snd3 p)
      fromReqStr d = show (curr,Tr) ++ " requires a preceding action to continue " ++ show (snd3 p) ++ " from " ++ show (applyDir d curr,Tr)
      leavesPuzzle = case snd3 p of
        (Completed (UseEnvMult es)) -> case es of [] -> False; _non_empty -> last es == TraverseDoor
        _ -> False
      -- globalMismatch :: PlayerAction -> PlayerAction -> ConsRes ?
      -- globalMismatch tpl tpg = show (curr,Tr) ++ " contains " ++ show tpl ++ " which should be consistent to global information " ++ show tpg
  in (if needsGroundCheck then isGrounded Tr curr p else alwaysOk)
     `also` isPermeable Tr curr p
     `also` (case snd3 p of {- teleport cases (todo) -}
        Initiated _tp@(Teleport {}) -> alwaysOk
          -- mkGlobalCC (\g -> unknownOkAnd (\tp' -> fromBool (globalMismatch tp tp') (tp==tp')) $ M.lookup (tpch tp) g)
          -- this actually belongs to the self-consistency tests.
          -- why not simply do self-consistency check here as well?
          -- todo: a check for landing on the other side is not nesessary, as each will know,
          -- that it is itself consistent with global information when the check runs.
          -- or perhaps, we need idenpendent consistency checks for the global-infos.
          -- because, if the partner tp-palce has no tp-action, this would not be recognized currently.
        Completed _tp@(Teleport {}) -> alwaysOk
          -- mkGlobalCC (\g -> unknownOkAnd (\tp' -> fromBool (globalMismatch tp tp') (tp==tp')) $ M.lookup (tpch tp) g)
        _ {- non-teleport cases -}  ->
          (if leavesPuzzle then alwaysOk else case movingNext of
              Nothing -> hasFutureIf Tr St curr (thd3 p) (any ((thd3 p)==) . bcps)
              Just d  -> mkSimpleCC (applyDir d curr) $ unknownOkAndc (fromBoolc (nextReqStr d) . any (p `canBePredOf`) . bctps) . getter Tr
          ) `also` case movingFrom of
              Nothing -> hasPastIf Tr St curr (fst3 p) (any ((fst3 p)==) . bcps);
              Just d  -> mkSimpleCC (applyDir d curr) $ unknownOkAndc (fromBoolc (fromReqStr d) . any (`canBePredOf` p) . bctps) . getter Tr
      )
{- todo: higher-prototype: check, what all these items do.
  handle teleportation processes by using a Reader which holds information
  on current global things like tp[char], switch etc.-}

canBePredOf :: (Player,PlayerT,Player) -> (Player,PlayerT,Player) -> Bool
(_,_act1,p1) `canBePredOf ` (p2,_act2,_) = p1 == p2 {- todo: -}
-}
-- auto-check type on object and insert it into the appropriate place
-- in the Cons x.

futureWith :: (Show a, Block b, Block c) => This b -> This c
              -> TimePos -> a -> Cons c -> STCons
futureWith ths oth (t,pos) o cb =
  let dest = if ths `is_a` St then (t,pos) else (t+1,pos)
  in  mkSimpleSTCons oth dest cb
      $ show ((t,pos),ths) ++" requires future for "++show o ++ " at " ++ show (dest,oth)

pastWith :: (Show a, Block b, Block c) => This b -> This c
              -> TimePos -> a -> Cons c -> STCons
pastWith ths oth (t,pos) o cb =
  let (dest,atBoundary) = if ths `is_a` St then ((t-1,pos),t <= 0) else ((t+1,pos),False)
  in  if atBoundary then alwaysOk
      else mkSimpleSTCons oth dest cb
        $ show ((t,pos),ths) ++" requires past for "++show o ++ " at " ++ show (dest,oth)

{-
hasPastIf :: (Show a,Block b,Block c) => This b -> This c -> TimePos -> a -> (c -> Bool) -> STCons
hasPastIf ths oth (t,pos) o f =
  let (dest,atBoundary) = if ths `is_a` St then ((t-1,pos),t <= 0) else ((t,pos),False)
  in  if atBoundary then alwaysOk
      else mkSimpleCC dest $ unknownOkAndc (fromBoolc (show ((t,pos),ths) ++" requires past for "++show o ++" at " ++ show (dest,oth)) . f) . getter oth
;

phyObjInterferesWithT :: TimePos -> PhyObj -> PhyObjT -> STCons
phyObjInterferesWithT _ _ _ = todo

envInterferesWithT :: TimePos -> (Env,EnvT,Env) -> STCons
envInterferesWithT _ _ = todo
-}
playerInterferesWith :: TimePos -> Player -> STCons
playerInterferesWith curr p =
  isGrounded St curr p
  `also` isPermeable St curr p
  `also` futureWith  St Tr curr p leastc{bctcps = MS.singleton (j p,n,  n)}
  `also` pastWith    St Tr curr p leastc{bctcps = MS.singleton (n  ,n,j p)}
;-- we don't require grounded-ness in the transition phase, because that will be handled by the check at the BlockTr.

phyObjInterferesWith :: TimePos -> PhyObj -> STCons
phyObjInterferesWith curr o =
  isGrounded St curr o
  `also` isPermeable St curr o
  `also` futureWith St Tr curr o leastc{bctcos = MS.singleton (n,n,j o)}
  `also` pastWith   St Tr curr o leastc{bctcos = MS.singleton (j o,n,n)}
-- physical objects are only checked, when they are on ground.
-- they are not checked, when they are in doors or in player's inventories.
;
{-
envInterferesWith :: TimePos -> Env -> STCons
envInterferesWith curr env =
  {- is grounded -}
  (if (case env of Door _ _ -> True; Switch _ -> True; _ -> False) then isGrounded St curr env else alwaysOk)
  `also` hasFutureIf St Tr curr env ((env==) . fst3 . bctenv)
  `also` hasPastIf   St Tr curr env ((env==) . thd3 . bctenv)
;

adjustGlobalInfo :: BlockTr -> ConsHistory -> MayContra ConsHistory
adjustGlobalInfo bct ch0 = foldM f ch0 (bctps bct) where
  insertable t ch = maybe True (t==) $ M.lookup (tpch t) (chglobal ch)
  inserttp   t ch = ch {chglobal = M.insert (tpch t) t (chglobal ch)}
  failStr    t ch = "Teleport observation "++show t++" contradicts established global-information " ++ show (chglobal ch)
  f ch (_,Completed (tp@Teleport{}),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch (_,Initiated (tp@Teleport{}),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch _ = success ch
;
--}
-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockSt that satisfies it.
{- perhaps needs tree-search? perhaps needs explicit encoding
of choice, such that in the last step, all combinations can be tried? -}
inferMinimal :: Cons BlockSt -> MayContra BlockSt
inferMinimal _ = failing "TODO: implement inferMinimal using STCons."

inferMinimalT :: Cons BlockSt -> MayContra BlockSt
inferMinimalT _ = failing "TODO: implement inferMinimalT using STCons."

