{-# LANGUAGE CPP,TupleSections,NamedFieldPuns,TypeFamilies,StandaloneDeriving,DeriveDataTypeable,FlexibleContexts,FlexibleInstances,TypeSynonymInstances,ScopedTypeVariables #-}
-- GHC CPP macros: https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/phases.html#standard-cpp-macros
-- https://guide.aelve.com/haskell/cpp-vww0qd72
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Interference(
     module BaseBlock
    ,module Interference -- for ghci.
    {-
    ,allBlockConstraints
    ,inferMinimal
    ,inferMinimalT
    ,runSTCons
    ,asPartialCH
    ,adjustGlobalInfo
    
    ,This(..)
    ,is_a
    ,Cons(..)-}
  )
  where

import BaseBlock
import Semantics
import Data.Data
import Control.Monad (foldM)
import ViewBase(enclosing,dot)
import qualified Data.Map as M
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Control.Arrow (second)
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
  data This BlockSt = St
  this = St
  getter St = fst
  setter St cb = (cb,leastc)
  leastc = BCC MS.empty MS.empty Nothing
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envOnStandables)
  in_standable ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envInStandables)
  permeable    ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envPermeables  )
  
  selfconsistent _ = alwaysOk -- TODO: implement. required counting ability => BC_Cons and BCT_Cons
  interferesWithBlock = interferesWith
  
  -- find the block where the player is and reduce all to that block.
  {- todo: user interferences of player to reduce observations during closed eyes -}
  reduceToClosed Proxy spo@(Specific _ player _ mp) = spo {sobservations = (pos, mp M.! pos)}
    where pos = case (M.toList . M.filter (any (player==) . bcps) $ mp) of ((p,_):_) -> p ; [] -> error "reduceToClosed: Player not found"
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

instance Block BlockTr where
  type OpenObs BlockTr = Specific (Space BlockTr)
  type ClosedObs BlockTr = Specific (Space BlockTr)
  data Cons BlockTr =
      BCTC {
        bctcenv  :: (Maybe Env, Maybe EnvT, Maybe Env)
       ,bctcps :: MultiSet (Maybe Player,Maybe PlayerT,Maybe Player)
       ,bctcos :: MultiSet (Maybe PhyObj,Maybe PhyObjT,Maybe PhyObj)
    } deriving (Eq,Ord)
  type Antcpt BlockTr = Space (Maybe BlockTr)
  data This BlockTr = Tr
  this = Tr
  getter Tr = snd
  setter Tr cb = (leastc,cb)
  leastc = BCTC (Nothing,Nothing,Nothing) MS.empty MS.empty
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envOnStandables <*> envOnStandablesT <*> envOnStandables
  
  in_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envInStandables <*> envInStandablesT <*> envInStandables
    
  permeable    ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envPermeables   <*> envPermeablesT   <*> envPermeables
  
  selfconsistent _ = alwaysOk -- TODO: implement
  interferesWithBlock = interferesWithT
  
  -- get all block where the player is identified
  {- todo: user interferences of player to reduce observations during closed eyes -}
  reduceToClosed Proxy spo@(Specific _ plyr _ mp) =   spo { sobservations = M.filter (any (\(p1,_,p2) -> p1 == plyr || p2 == plyr) . MS.toList . bctps) mp}
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

allBlockConstraints :: Block b => TimePos -> b -> STCons
allBlockConstraints curr b =
  selfconsistent b `also` interferesWithBlock curr b

deriving instance Data PhyObjT
deriving instance Typeable PhyObjT
deriving instance Data Player
deriving instance Typeable Player
deriving instance Data PlayerAction
deriving instance Typeable PlayerAction
deriving instance Data Teleport
deriving instance Typeable Teleport
deriving instance Data a => Data (Partial a)
deriving instance Typeable a => Typeable (Partial a)
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

{- used for teleport-check in self-consistency -}
--mkGlobalCC :: CH_Global -> ConsDesc -> STCons
--mkGlobalCC cchg str = STC [Leaf (cchg,M.empty,str)]

mkSTConsFromChoice :: Block b => This b -> TimePos -> [Cons b] -> ConsDesc -> STCons
mkSTConsFromChoice ths tpos bcs str = eAny $ 
    do bc <- bcs
       let mp = M.singleton tpos $ setter ths bc
       return $ eLeaf ((mp,unknownGlobalP),str)

mkSimpleSTConsWithGlobal :: Block b => This b -> TimePos -> CH_GlobalP -> Cons b -> ConsDesc -> STCons
mkSimpleSTConsWithGlobal ths tpos cchg cb str =
  eLeaf ((M.singleton tpos (setter ths cb),cchg),str)

mkSimpleSTCons :: Block b => This b -> TimePos -> Cons b -> ConsDesc -> STCons
mkSimpleSTCons ths tpos cb str = mkSimpleSTConsWithGlobal ths tpos unknownGlobalP cb str

{- y-axis is pointed downwards -}
-- isGrounded ensures, that current position is on ground.
-- either through looking at below, or through a platform at same position
isGrounded :: (Show a,Block b) => This b -> TimePos -> a -> STCons
isGrounded ths curr@(t,(x,y)) reason =
  let below = (t,(x,succ y))
  in  in_standable ths curr (show (curr,ths) ++ " needs standable (cause: "++show reason++") in "++ show  curr)
      `orElse` on_standable ths below (show (curr,ths) ++ " needs standable (cause: "++show reason++") on "++ show below)
;

isPermeable :: (Block b,Show a) => This b -> TimePos -> a -> STCons
isPermeable ths curr reason = permeable ths curr (show (curr,ths) ++ " requires permeability (cause: " ++ show reason ++ ")")

{- connectives and neutral-elem for building STCons from others.
--concretec :: TimePos -> Cons a -> Either ConsFail a
andc :: Cons a -> Cons a -> Either ConsFail (Cons a)
satisfiesc :: a -> Cons a -> Maybe ConsFail
concretec BCC{bccps,bccos,bccenv} =
  maybeToEither "No minimal env possible dueto unknown env." $ (\env -> BC {bcps = bccps, bcos=bccos, bcenv = env}) <$> bccenv 
concretec = todo
-}

okc :: (ConsHistoryP,ConsDesc)
okc = ((M.empty,unknownGlobalP),"ok")

alwaysOk :: STCons
alwaysOk = eLeaf okc

orElse :: Expr a -> Expr a -> Expr a
orElse a b = eAny [a,b]

also :: Expr a -> Expr a -> Expr a 
also a b = eAll [a,b]

instance Monoid STCons where
  mempty = alwaysOk
  mappend = also
;

{- returns True iff all known blocks of left cons-history
  satisfy everything in right partial cons-history-}
satisfies :: ConsHistory -> ConsHistoryP -> Bool
satisfies = todo
{- true iff, the multi-constraints can be seens as a special
case of the concrete-history. -}

concretize :: ConsHistoryP -> MayContra ConsHistory
concretize = todo {- use inferMinimal(T) -}

{- IMPORTANT FUNCTION:
  runs a space-time constraint on a partial constraint history.
  either the constraint history is consistent with it, then
  a new constraint-history with minimal consistency requirements
  applied is returned. otherwise a description for the contradiction
  is given.
  for each viable alternative in 
  Uses: (a) check for inconsistencies in ConsHistory
        (b) building bock for concretizing the ConsHistory.
            this needs to walk time-by-time from t=0 to tMax to work properly
            the concrete representation needs to be called from
            outside, once, all constraints for a block have been checked.
            (concretize >>= satisfies)
  Implementation:
    an STCons is violated, if the same contradiction is
    found in all possible solutions (e.g. all elements of the list).
    only then, will it become a Left ConsDesc.
    it works by traversing through the tree of constraints.
    at each `Leaf`, the current ConsHistory is applied to the constraint
    - either succceeding or failing.
    at each `And`, the current Conshistory is applied to both
    and checked, whether both work independently and jointly.
    If jointly, then success.
    If only independently, then failure with 2 message contradiction.
    Failed constraints are always propagated forward and not processed further.
    Each branch (or and'ed nodes) is kept separate from other branches.
    Each element in the result list corresponds to a branch.
    Thus, the whole constraint was not satisfied, if every branch
    ended up as Left.
-}
runSTCons :: ConsHistoryP -> STCons -> [ConsRes]
runSTCons chp0 stcons = foldExpr leaf allExpr anyExpr stcons $ chp0
  where
    {- apply constraint x. create singleton branch -}
    leaf x chp = [applyCons x chp]    :: [ConsRes]
    
    {- fold through conjunction. try all alternatives, that arise. -}
    allExpr fs chp =
      let {- list of alternatives with a possible result (Maybe) and a stack of the applied constraints descriptions.
             the head of each [ConsDesc] describes the first contradiction-leading constraint. -}
          joints :: [(Maybe ConsHistoryP, [ConsDesc])]
          joints       = foldr f [(Just chp,[]{-accum-})] fs
            where f g tps =
                    do (chp' ,ss)   <- tps
                       case chp' of
                         Nothing    -> return (chp',ss)
                         Just chp'' -> second (:ss) <$> g chp''
          {- non-empty ds ensured by non-empty fs. which is ensured by construction. -}
      in  do (mchp,ds) <- joints
             case mchp of
               Nothing   -> return $ (Nothing ,"Joint requirements: {"++head ds++"} needs to be consistent with " ++ enclosing "{" "," "}" (tail ds))
               Just chp' -> return $ (Just chp',"Joint requirements: " ++ enclosing "{" "," "}" ds)
    
    {- apply disjunction. keep branches separate. -}
    anyExpr fs chp = fs >>= ($chp) -- didnt expect this impl. to be that short...
;


--listFailReferenceOutOfBounds :: TimePos -> TimePos -> ConsRes
--listFailReferenceOutOfBounds curr other = maybeLeft $ show curr ++ " references out-of-bounds "++show other

applyCons :: (ConsHistoryP,ConsDesc) -> ConsHistoryP -> (Maybe ConsHistoryP,ConsDesc)
applyCons (chp,desc) chp' = (chp `merge` chp',desc)
;

{- elements which support conjunctive merging. -}
class Merge a where
  merge :: a -> a -> Maybe a
;

defaultMerge :: Eq a => a -> a -> Maybe a
defaultMerge a b = if a == b then Just a else Nothing

instance Merge (Cons BlockSt) where
  merge b1 b2 = BCC
    <$> bccps  b1 `merge` bccps  b2
    <*> bccos  b1 `merge` bccos  b2
    <*> bccenv b1 `merge` bccenv b2
;

instance Merge (Cons BlockTr) where
  merge = todo
;

{- outer Maybe represents merge success/failure.
inner maybe represents knowning-ness of value. -}
instance Merge a => Merge (Maybe a) where
  merge Nothing mb = Just mb
  merge ma Nothing = Just ma
  merge (Just a) (Just b) = Just <$> merge a b
;
instance (Merge a, Merge b) => Merge (a,b) where
  merge (a1,b1) (a2,b2) = (,) <$> a1 `merge` a2 <*> b1 `merge` b2
;

instance (Merge a, Merge b, Merge c) => Merge (a,b,c) where
  merge (a1,b1,c1) (a2,b2,c2) = (,,) <$> a1 `merge` a2 <*> b1 `merge` b2 <*> c1 `merge` c2
;

{- merge elements with same key. fail, if any single merge fails -}
instance (Ord k, Merge a) => Merge (M.Map k a) where
  merge ma = M.traverseWithKey
    (\k a -> maybe (Just a) (merge a) $ M.lookup k ma) 
;

instance Merge PhyObj     where merge = defaultMerge
instance Merge Dir        where merge = defaultMerge
instance Merge Env        where merge = defaultMerge
instance Merge Player     where merge = defaultMerge
instance Merge PhyObjT    where merge = defaultMerge
instance Merge EnvT       where merge = defaultMerge
instance Merge EAOnce     where merge = defaultMerge
instance Merge EARep      where merge = defaultMerge
instance Merge Teleport   where merge = defaultMerge

{- elements of a multiset are not recursively merged.
  otherwise {(j 1,n,n)} `merge` {(n,j 2,n)} `merge` {(j 3,n,n)}
  would become {(j 1,j 2,n),(j 3,n,n)} or
  {(j 1,n,n),(j 3,j 2,n)} depending on associativity.
  This is undesireable, because we don't know, which one of these
  merges is correct. Imagine all of these tuples corresponding to a
  key that appears twice in two different ways in the block.
  Merge proactively, we would fix, which key is used with the action.
  Also, merging elements may decrease their multiplicity, which
  is undesireable. (as couting equal objects is not possible now anymore.)
-}
instance Ord a => Merge (MultiSet a) where merge = Just `dot` MS.union

{- a variant of multiset-merge where each element is also merged
newtype FullMerge a = FullMerge {getFullMerge :: a}
instance Merge a => Merge (FullMerge (MultiSet a)) where
;
-}

-- instance Merge CH_Global -- redundant because it's a special case of Map k a

asPartialCH :: ConsHistory -> ConsHistoryP
asPartialCH = todo
{- turns a consistency history to a partial view of it. -}

flatten :: Timed (Space a) -> M.Map TimePos a
flatten mp = M.fromAscList (M.toAscList mp >>= \(t,mpi) -> M.toAscList mpi >>= \(pos,x) -> [((t,pos),x)])

interferesWith :: TimePos -> BlockSt -> STCons
interferesWith curr bc =
  ( playerInterferesWith curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjInterferesWith curr `foldMap` MS.toList (bcos bc))
  `also` ( envInterferesWith curr (bcenv bc))
;

interferesWithT :: TimePos -> BlockTr -> STCons
interferesWithT curr bct =
  ( playerInterferesWithT curr `foldMap` MS.toList (bctps bct) )
  `also` ( phyObjInterferesWithT curr `foldMap` MS.toList (bctos bct) )
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
      
      succs (_,pt,p1) = MS.fromList $ (\(pa ,pta)-> (j pa,j pta,   n)) <$> successors   (pt,p1)
      preds (p0,pt,_) = MS.fromList $ (\(pta,pa )-> (n   ,j pta,j pa)) <$> predecessors (p0,pt)
      
  in (if needsGroundCheck then isGrounded Tr curr p else alwaysOk)
     `also` isPermeable Tr curr p
     {- todo: higher-prototype: check, what all these items do. -}
     `also` (case snd3 p of {- teleport cases: handled automatically by global check in self-consistency -}
        Initiated (TP _) -> alwaysOk
        Completed (TP _) -> alwaysOk
          {- todo: handle TParrive and TPsend case, when player itself is sent. -}
        _ {- non-teleport cases -}  ->
          (if leavesPuzzle then alwaysOk else case movingNext of
              Nothing -> futureWith Tr St curr (thd3 p) leastc{bccps=MS.singleton (thd3 p)}
              Just d  -> mkSimpleSTCons Tr (applyDir d curr) leastc{bctcps=succs p} (nextReqStr d)
          ) `also` case movingFrom of
              Nothing -> pastWith   Tr St curr (fst3 p) leastc{bccps=MS.singleton (fst3 p)}
              Just d  -> mkSimpleSTCons Tr (applyDir d curr) leastc{bctcps=preds p} (fromReqStr d)
      )
;

futureWith :: (Show a, Block b, Block c) => This b -> This c -> TimePos -> a -> Cons c -> STCons
futureWith ths oth (t,pos) o cb =
  let dest = if ths `is_a` St then (t,pos) else (t+1,pos)
  in  mkSimpleSTCons oth dest cb
      $ show ((t,pos),ths) ++" requires future for "++show o ++ " at " ++ show (dest,oth)

pastWith :: (Show a, Block b, Block c) => This b -> This c -> TimePos -> a -> Cons c -> STCons
pastWith ths oth (t,pos) o cb =
  let (dest,atBoundary) = if ths `is_a` St then ((t-1,pos),t <= 0) else ((t+1,pos),False)
  in  if atBoundary then alwaysOk
      else mkSimpleSTCons oth dest cb
        $ show ((t,pos),ths) ++" requires past for "++show o ++ " at " ++ show (dest,oth)
;

phyObjInterferesWithT :: TimePos -> (PhyObj,PhyObjT,PhyObj) -> STCons
phyObjInterferesWithT _ _ = alwaysOk -- TODO

envInterferesWithT :: TimePos -> (Env,EnvT,Env) -> STCons
envInterferesWithT _ _ = alwaysOk -- TODO

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
  `also` futureWith St Tr curr o leastc{bctcos = MS.singleton (n  ,n,j o)}
  `also` pastWith   St Tr curr o leastc{bctcos = MS.singleton (j o,n,  n)}
-- physical objects are only checked, when they are on ground.
-- they are not checked, when they are in doors or in player's inventories.
;

envInterferesWith :: TimePos -> Env -> STCons
envInterferesWith curr env =
  {- is grounded -}
  (if (case env of Door _ _ -> True; Switch _ -> True; _ -> False)
    then isGrounded St curr env
    else alwaysOk)
  `also` futureWith St Tr curr env leastc{bctcenv = (j env,n,    n)}
  `also` pastWith   St Tr curr env leastc{bctcenv = (n    ,n,j env)}

adjustGlobalInfo :: BlockTr -> ConsHistory -> MayContra ConsHistory
adjustGlobalInfo bct ch0 = foldM f ch0 (bctps bct) where
  insertable t ch = maybe True (t==) $ M.lookup (tpch t) (chglobal ch)
  inserttp   t ch = ch {chglobal = M.insert (tpch t) t (chglobal ch)}
  failStr    t ch = "Teleport observation "++show t++" contradicts established global-information " ++ show (chglobal ch)
  f ch (_,Completed (TP tp),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch (_,Initiated (TP tp),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch _ = success ch
;

-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockSt that satisfies it.
{- perhaps needs tree-search? perhaps needs explicit encoding
of choice, such that in the last step, all combinations can be tried? -}
inferMinimal :: Cons BlockSt -> MayContra BlockSt
inferMinimal _ = failing "TODO: implement inferMinimal using STCons."

inferMinimalT :: Cons BlockSt -> MayContra BlockSt
inferMinimalT _ = failing "TODO: implement inferMinimalT using STCons."

