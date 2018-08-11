{-# LANGUAGE CPP,TupleSections,NamedFieldPuns,TypeFamilies,StandaloneDeriving,DeriveDataTypeable,FlexibleContexts,FlexibleInstances,ScopedTypeVariables #-}
-- GHC CPP macros: https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/phases.html#standard-cpp-macros
-- https://guide.aelve.com/haskell/cpp-vww0qd72
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

module Interference(
     module BaseBlock
    ,module Interference -- for ghci.
    {-
    ,stConsContradictions
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
import Control.Arrow (first,second,(***))
import Data.Dynamic
-- import Debug.Trace
-- import Control.Applicative

instance Block BlockSt where
  type OpenObs BlockSt = Specific (Space BlockSt)
  type ClosedObs BlockSt = Specific (Pos,BlockSt)
  data Cons BlockSt = BCC {
       bccenv :: Maybe Env
      ,bccos  :: MultiSet PhyObj
      ,bccps  :: MultiSet Player
    } deriving (Ord,Eq)
  type Antcpt BlockSt = Space (Maybe BlockSt)
  data This BlockSt = St
  this = St
  getter St = fst
  setter St cb = (cb,leastc)
  leastc = BCC{bccps=MS.empty,bccos=MS.empty,bccenv=Nothing}
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envOnStandables)
  in_standable ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envInStandables)
  permeable    ths tpos = mkSTConsFromChoice ths tpos ((\x->leastc{bccenv=x}) <$> envPermeables  )
  
  blockConstraints = blkConstraints
  
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
       ,bctcos :: MultiSet (Maybe PhyObj,Maybe PhyObjT,Maybe PhyObj)
       ,bctcps :: MultiSet (Maybe Player,Maybe PlayerT,Maybe Player)
    } deriving (Eq,Ord)
  type Antcpt BlockTr = Space (Maybe BlockTr)
  data This BlockTr = Tr
  this = Tr
  getter Tr = snd
  setter Tr cb = (leastc,cb)
  leastc = BCTC{bctcenv=(Nothing,Nothing,Nothing),bctcos=MS.empty,bctcps=MS.empty}
  
  on_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envOnStandables <*> envOnStandablesT <*> envOnStandables
  
  in_standable ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envInStandables <*> envInStandablesT <*> envInStandables
    
  permeable    ths tpos = mkSTConsFromChoice ths tpos $ 
    (\l t r -> leastc{bctcenv=(l,t,r)}) <$> envPermeables   <*> envPermeablesT   <*> envPermeables
  
  blockConstraints = blkConstraintsT
  
  -- get all block where the player is identified
  {- todo: user interferences of player to reduce observations during closed eyes -}
  reduceToClosed Proxy spo@(Specific _ plyr _ mp) =   spo { sobservations = M.filter (any (\(p1,_,p2) -> p1 == plyr || p2 == plyr) . MS.toList . bctps) mp}
  
  applypwObs p@Proxy sobs fo fc = if peyes (splayer sobs) then fo sobs else fc (reduceToClosed p sobs)
;

deriving instance Data PhyObjT
deriving instance Typeable PhyObjT
deriving instance Data Player
deriving instance Typeable Player
deriving instance Data PlayerAction
deriving instance Typeable PlayerAction
deriving instance Data Teleport
deriving instance Typeable Teleport
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

is_a :: (Typeable a,Typeable b) => a -> b -> Bool
ths `is_a` oth = typeOf ths == typeOf oth

{- used for teleport-constraints -}
mkSTConsOnGlobal :: CH_GlobalP -> ConsDesc -> STCons
mkSTConsOnGlobal chgl str = eLeaf (CHP M.empty chgl,str)

mkSTConsFromChoice :: Block b => This b -> TimePos -> [Cons b] -> ConsDesc -> STCons
mkSTConsFromChoice ths tpos bcs str = eAny $ 
    do bc <- bcs
       let mp = M.singleton tpos $ setter ths bc
       return $ eLeaf (CHP mp unknownGlobalP,str)

mkSimpleSTConsWithGlobal :: Block b => This b -> TimePos -> CH_GlobalP -> Cons b -> ConsDesc -> STCons
mkSimpleSTConsWithGlobal ths tpos cchg cb str =
  eLeaf (CHP (M.singleton tpos (setter ths cb)) cchg,str)

mkSimpleSTCons :: Block b => This b -> TimePos -> Cons b -> ConsDesc -> STCons
mkSimpleSTCons ths tpos cb str = mkSimpleSTConsWithGlobal ths tpos unknownGlobalP cb str

asPartialCH :: ConsHistory -> ConsHistoryP
asPartialCH = toPartial

concretize :: ConsHistoryP -> MayContra ConsHistory
concretize = todo {- use inferMinimal or simply concretize by simulation
  with player inputs -}

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
    leaf (chp,desc) chp' = [(chp `conjunct` chp',desc)]    :: [ConsRes]
    
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

{- list of contradictions-}
stConsContradictions :: ConsHistory -> STCons -> [ConsDesc]
stConsContradictions ch stcons = foldExpr leaf allExpr anyExpr stcons
  where
    {- apply constraint x  -}
    leaf (chp,desc) = if ch `satisfies` chp then [] else [desc]
    
    {- a conjunction of contradictions is simply the list of contradictions. -}
    allExpr = concat
    
    {- apply disjunction. if there is am empty sub-list,
    without contradictions, then take it. there is no contradiction there.
    if every sub-list has at-least 1 contradiction,
    then take the disjunction of them as the message. and this is done by sequence! -}
    anyExpr =
      map (\xs -> "Needs any one of: " ++ enclosing "{" "or" "}" xs)
      . sequence
;

--listFailReferenceOutOfBounds :: TimePos -> TimePos -> ConsRes
--listFailReferenceOutOfBounds curr other = maybeLeft $ show curr ++ " references out-of-bounds "++show other

{- elements which support conjunctive merging and partial description. -}
class Partial a where
  conjunct :: a -> a -> Maybe a {- todo: for error reporting, this should also return place of error+desc -}
  {- ASSOCIATIVE -}
  
  type Concrete a :: *
  type Concrete a = a
  toPartial :: Concrete a -> a
  {- INJECTIVE: a /= b -> toPartial a /= toPartial b  -}
  {- more laws in Test.hs -}
  
  {- how to make shrinking happen recursively? -}
  -- shrink
  -- narrower than
  
  {- counting not implemented -}
  satisfies :: Concrete a -> a -> Bool
;

{- returns True iff all known blocks of left cons-history
  satisfy everything in right partial cons-history-}
satisfiesCH :: ConsHistory -> ConsHistoryP -> Bool
satisfiesCH = satisfies
{- true iff, the multi-constraints can be seens as a special
case of the concrete-history. -}

defaultConjunct :: Eq a => a -> a -> Maybe a
defaultConjunct a b = if a == b then Just a else Nothing

instance Partial ConsHistoryP where
  conjunct (CHP st1 gl1) (CHP st2 gl2) = uncurry CHP <$> (st1,gl1) `conjunct` (st2,gl2)
  type Concrete ConsHistoryP = ConsHistory
  toPartial CH{chspace,chglobal} =
    CHP
      (M.map (mblk2partial *** mblk2partial) $ flatten chspace)
      (toPartial chglobal)
    where
      mblk2partial :: (Block b,Partial (Cons b)) => Maybe (Concrete (Cons b)) -> Cons b
      mblk2partial = maybe leastc toPartial
  satisfies CH{chspace,chglobal} CHP{chpspace,chpglobal} =
    let mp = flatten chspace
        blk bc = maybe (bc==leastc) (`satisfies` bc)
    in  satisfies chglobal chpglobal
        && (all (uncurry (&&))
           . M.mapWithKey
              (\k (bst,btr) -> maybe (False,False)
                (blk bst *** blk btr)
                $ M.lookup k mp)
           $ chpspace)
;

{- could use GHC.Generics here ... -}
instance Partial (Cons BlockSt) where
  conjunct b1 b2 = BCC
    <$> bccenv b1 `conjunct` bccenv b2
    <*> bccos  b1 `conjunct` bccos  b2
    <*> bccps  b1 `conjunct` bccps  b2
  type Concrete (Cons BlockSt) = BlockSt
  toPartial BC{bcps,bcos,bcenv} =
    BCC (toPartial bcenv) (toPartial bcos) (toPartial bcps)
  satisfies BC{bcps,bcos,bcenv} BCC{bccps,bccos,bccenv} =
    and [satisfies bcps  bccps ,
         satisfies bcos  bccos ,
         satisfies bcenv bccenv]
;

instance Partial (Cons BlockTr) where
  conjunct b1 b2 = BCTC
    <$> bctcenv b1 `conjunct` bctcenv b2
    <*> bctcos  b1 `conjunct` bctcos  b2
    <*> bctcps  b1 `conjunct` bctcps  b2
  type Concrete (Cons BlockTr) = BlockTr
  toPartial BCT{bctps,bctos,bctenv} =
    BCTC (toPartial bctenv) (toPartial bctos) (toPartial bctps)
  satisfies BCT{bctps,bctos,bctenv} BCTC{bctcps,bctcos,bctcenv} =
    and [satisfies bctps  bctcps ,
         satisfies bctos  bctcos ,
         satisfies bctenv bctcenv]
;

{- Partial instance for values that might not exist. -}
instance Partial a => Partial (Maybe a) where
  {- outer Maybe represents conjunct success/failure.
  inner maybe (of type a) represents knowning-ness of value -}
  conjunct Nothing mb = Just mb
  conjunct ma Nothing = Just ma
  conjunct (Just a) (Just b) = Just <$> conjunct a b
  type Concrete (Maybe a) = Concrete a
  toPartial = Just . toPartial {- result is just, because we know the value -}
  satisfies ca = maybe True (satisfies ca)
;
instance (Partial a, Partial b) => Partial (a,b) where
  conjunct (a1,b1) (a2,b2) = (,) <$> a1 `conjunct` a2 <*> b1 `conjunct` b2
  type Concrete (a,b) = (Concrete a,Concrete b)
  toPartial (a,b) = (toPartial a,toPartial b)
  satisfies (ca,cb) (a,b) = satisfies ca a && satisfies cb b
;

instance (Partial a, Partial b, Partial c) => Partial (a,b,c) where
  conjunct (a1,b1,c1) (a2,b2,c2) = (,,) <$> a1 `conjunct` a2 <*> b1 `conjunct` b2 <*> c1 `conjunct` c2
  type Concrete (a,b,c) = (Concrete a,Concrete b,Concrete c)
  toPartial (a,b,c) = (toPartial a,toPartial b, toPartial c)
  satisfies (ca,cb,cc) (a,b,c) = satisfies ca a && satisfies cb b && satisfies cc c
;

instance (Ord k, Partial a) => Partial (M.Map k a) where
  {- conjunct elements with same key. fail, if any single conjunct fails -}
  conjunct ma = M.traverseWithKey
    (\k a -> maybe (Just a) (conjunct a) $ M.lookup k ma)
  type Concrete (M.Map k a) = M.Map k (Concrete a)
  toPartial = M.map toPartial
  satisfies mca =
    M.foldr (&&) True
    . M.map (maybe False id)
    . M.mapWithKey (\k a -> (`satisfies` a) <$> (M.lookup k mca) )
;

instance Partial PhyObj     where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial Dir        where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial Env        where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial Player     where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial PlayerT    where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial PhyObjT    where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial EnvT       where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial EAOnce     where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial EARep      where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial
instance Partial Teleport   where conjunct = defaultConjunct; toPartial = id; satisfies = (==) . toPartial

{- elements of a multiset are not recursively merged.
  otherwise {(j 1,n,n)} `conjunct` {(n,j 2,n)} `conjunct` {(j 3,n,n)}
  would become {(j 1,j 2,n),(j 3,n,n)} or
  {(j 1,n,n),(j 3,j 2,n)} depending on associativity.
  This is undesireable, because we don't know, which one of these
  merges is correct. Imagine all of these tuples corresponding to a
  key that appears twice in two different ways in the block.
  Merge proactively, we would fix, which key is used with the action.
  Also, merging elements may decrease their multiplicity, which
  is undesireable. (as couting equal objects is not possible now anymore.)
-}
instance (Ord a,Partial a) => Partial (MultiSet a) where
  conjunct = Just `dot` MS.maxUnion
  type Concrete (MultiSet a) = MultiSet (Concrete a)
  toPartial = MS.map toPartial
  satisfies mca = all (\a -> any (`satisfies` a) mca)
;

{- a variant of multiset-conjunct where each element is also merged
newtype FullMerge a = FullMerge {getFullMerge :: a}
instance Partial a => Partial (FullMerge (MultiSet a)) where
;
-}
-- instance Partial CH_Global -- redundant because it's a special case of Map k a

flatten :: Timed (Space a) -> M.Map TimePos a
flatten mp = M.fromAscList (M.toAscList mp >>= \(t,mpi) -> M.toAscList mpi >>= \(pos,x) -> [((t,pos),x)])

blkConstraints :: TimePos -> BlockSt -> STCons
blkConstraints curr bc =
  ( playerConstraints curr `foldMap` MS.toList (bcps bc))
  `also` ( phyObjConstraints curr `foldMap` MS.toList (bcos bc))
  `also` ( envConstraints curr (bcenv bc))
;

blkConstraintsT :: TimePos -> BlockTr -> STCons
blkConstraintsT curr bct =
  ( playerConstraintsT curr `foldMap` MS.toList (bctps bct) )
  `also` ( phyObjConstraintsT curr `foldMap` MS.toList (bctos bct) )
  `also` ( envConstraintsT curr (bctenv bct))
;

{- checks for the valid status of links are tested here.
e.g. switch active ==> target active constraint can be formulated here.
also teleports are checked here. -}
globalConstraints :: ConsHistory -> STCons
globalConstraints CH{chglobal} = mconcat $ map (uncurry mkTPcons) $ M.toList chglobal 
  where {- assumption: tpch == tc -}
    mkTPcons _tc tp@Teleport{tpobjs=(ps,os)} =
      (objectAt tp) `foldMap` {- generic programming for checking objects are at source/dest-}
        (map toDyn (MS.toList os) ++ map toDyn (MS.toList ps))
      `also` alwaysOk {- TPsend at source -}
      `also` alwaysOk {- TPget  at source -}
; {- todo: perhaps use simply Either instead of dynamic? more type safety! -}

objectAt :: Teleport -> Dynamic -> STCons
objectAt tp@Teleport{tpsource,tpdest} o =
  let arrive = first pred $ tpdest in
  mkSTConsFromChoice Tr tpsource (tpExit tp o) (show tpsource ++ " needs a tp-exiting object " ++ showObj o)
  `also` mkSTConsFromChoice Tr arrive (tpArrive tp o) (show arrive ++ " needs a tp-arriving object " ++ showObj o)
;

applyIf :: (Typeable a, Typeable b) => (a -> b) -> Dynamic -> Dynamic
applyIf f d = case fromDynamic d of
  Just (a::a)  -> toDyn (f a)
  Nothing -> d
;

{- functions. only call on players or phyObjs -}
showObj :: Dynamic -> String
showObj = (\d -> fromDyn d "fatal[showObj]: was not player or phyobj")
  . applyIf (show :: Player -> String)
  . applyIf (show :: PhyObj -> String)

tpArrive,tpExit :: Teleport -> Dynamic -> [Cons BlockTr]
tpArrive tp = (\d -> fromDyn d $ error "fatal[tpArrive]: was not player or phyobj")
   . applyIf (tpArrivePlayer tp :: Player -> [Cons BlockTr])
   . applyIf (tpArrivePhyObj tp :: PhyObj -> [Cons BlockTr])
;
tpExit tp = (\d -> fromDyn d $ error "fatal[tpExit]: was not player or phyobj")
  . applyIf (tpExitPlayer tp :: Player -> [Cons BlockTr])
  . applyIf (tpExitPhyObj tp :: PhyObj -> [Cons BlockTr])
;

tpArrivePlayer tp@Teleport{tpch} p =
  {- dont know, whether teleport is from 0 -> 1 or 1 -> 0. and whether player is activator or not -}
  let p' b i = p{pinventory= adaptInv b i $ pinventory p}
      adaptInv b i = if b then (MS.delete (TOrb tpch i)) else id in
  ((\b i -> leastc{bctcps=MS.singleton (j (p' b i),j (Completed (TP b tp)),j p)} ) <$> [True,False] <*> [0,1])
    ++ return leastc{bctcps=MS.singleton (j p,j (Completed (TParriveP tpch)),j p)}
;
tpExitPlayer tp@Teleport{tpch} p =
  {- dont know, whether teleport is from 0 -> 1 or 1 -> 0. and whether player is activator or not -}
  let p' b i = p{pinventory= adaptInv b i $ pinventory p}
      adaptInv b i = if b then (MS.delete (TOrb tpch i)) else id in
  ((\b i -> leastc{bctcps=MS.singleton (j (p' b i),j (Initiated (TP b tp)),j p)} ) <$> [True,False] <*> [0,1])
    ++ return leastc{bctcps=MS.singleton (j p,j (Completed (TPexitP tpch)),j p)}

tpArrivePhyObj _ _ = [leastc] {- todo all -}
tpExitPhyObj _ _ = [leastc]


playerConstraintsT :: TimePos -> (Player,PlayerT,Player) -> STCons
playerConstraintsT curr p =
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
        Initiated (TP _ tp) -> mkSTConsOnGlobal (M.singleton (tpch tp) tp) $ "Player "++show (fst3 p)++" using tele-orb["++show (tpch tp)++"] fixing it to "++ show tp
        Completed (TP _ tp) -> mkSTConsOnGlobal (M.singleton (tpch tp) tp) $ "Player "++show (fst3 p)++" used tele-orb[" ++show (tpch tp)++"] fixing it to "++ show tp    
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
  let (dest,atBoundary) = if ths `is_a` St then ((t-1,pos),t <= 0) else ((t,pos),False)
  in  if atBoundary then alwaysOk
      else mkSimpleSTCons oth dest cb
        $ show ((t,pos),ths) ++" requires past for "++show o ++ " at " ++ show (dest,oth)
;

phyObjConstraintsT :: TimePos -> (PhyObj,PhyObjT,PhyObj) -> STCons
phyObjConstraintsT _ _ = alwaysOk -- TODO

envConstraintsT :: TimePos -> (Env,EnvT,Env) -> STCons
envConstraintsT _ _ = alwaysOk -- TODO

playerConstraints :: TimePos -> Player -> STCons
playerConstraints curr p =
  isGrounded St curr p
  `also` isPermeable St curr p
  `also` futureWith  St Tr curr p leastc{bctcps = MS.singleton (j p,n,  n)}
  `also` pastWith    St Tr curr p leastc{bctcps = MS.singleton (n  ,n,j p)}
;-- we don't require grounded-ness in the transition phase, because that will be handled by the check at the BlockTr.

phyObjConstraints :: TimePos -> PhyObj -> STCons
phyObjConstraints curr o =
  isGrounded St curr o
  `also` isPermeable St curr o
  `also` futureWith St Tr curr o leastc{bctcos = MS.singleton (n  ,n,j o)}
  `also` pastWith   St Tr curr o leastc{bctcos = MS.singleton (j o,n,  n)}
-- physical objects are only checked, when they are on ground.
-- they are not checked, when they are in doors or in player's inventories.
;

envConstraints :: TimePos -> Env -> STCons
envConstraints curr env =
  {- is grounded -}
  (if (case env of Door _ _ -> True; Switch _ -> True; _ -> False)
    then isGrounded St curr env
    else alwaysOk)
  `also` futureWith St Tr curr env leastc{bctcenv = (j env,n,    n)}
  `also` pastWith   St Tr curr env leastc{bctcenv = (n    ,n,j env)}

adjustGlobalInfo :: BlockTr -> ConsHistory -> MayContra ConsHistory
adjustGlobalInfo bct ch0 = foldM f ch0 (bctps bct) where
  f ch (_,Completed (TP _ tp),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch (_,Initiated (TP _ tp),_) = if insertable tp ch then success (inserttp tp ch) else failing $ failStr tp ch
  f ch _ = success ch
  insertable t ch = maybe True (t==) $ M.lookup (tpch t) (chglobal ch)
  inserttp   t ch = ch {chglobal = M.insert (tpch t) t (chglobal ch)}
  failStr    t ch = "Teleport observation "++show t++" contradicts established global-information " ++ show (chglobal ch)
;

-- a function crucial for concreteHistory.
-- given a set of conditions to be satisfied,
-- it searches for the simplest BlockSt that satisfies it.
-- though, this is not needed in the simple simulation approach currently.
inferMinimal :: Cons BlockSt -> MayContra BlockSt
inferMinimal _ = failing "TODO: implement inferMinimal using STCons."

inferMinimalT :: Cons BlockSt -> MayContra BlockSt
inferMinimalT _ = failing "TODO: implement inferMinimalT using STCons."

