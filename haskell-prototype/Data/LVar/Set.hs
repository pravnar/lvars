{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Data.LVar.Set
       (
         -- * Basic operations
         ISet, Snapshot(ISetSnap),
         newEmptySet, newSet, newFromList,
         putInSet, waitElem, waitSize, 

         -- * Iteration and callbacks
         forEach, addHandler,

         -- * Quasi-deterministic operations
         freezeSetAfter, withCallbacksThenFreeze, freezeSet,

         -- * Higher-level derived operations
         copy, traverseSet, traverseSet_, union, intersection,
         cartesianProd, cartesianProds, 

         -- * Alternate versions of derived ops that expose HandlerPools they create.
         forEachHP, traverseSetHP, traverseSetHP_,
         cartesianProdHP, cartesianProdsHP
       ) where

import           Control.Monad (void)
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import           Control.LVish as LV hiding (addHandler)
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                freezeLVAfter, liftIO)
import qualified Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- | We only have one mutable location here, so this is not a scalable implementation.
-- newtype ISet a = ISet (LVar (IORef (S.Set a))) a
newtype ISet s a = ISet (LVar s (IORef (S.Set a)) a)

unISet (ISet lv) = lv

-- | Physical identity, just as with IORefs.
instance Eq (ISet s v) where
  ISet lv1 == ISet lv2 = state lv1 == state lv2 

instance LVarData1 ISet where
  newtype Snapshot ISet a = ISetSnap (S.Set a)
      deriving (Show,Ord,Read,Eq)
  freeze    = fmap ISetSnap . freezeSet
  newBottom = newEmptySet

  -- TODO: traverseSnap

-- | Create a new, empty, monotonically growing 'ISet'.
newEmptySet :: Par d s (ISet s a)
newEmptySet = newSet S.empty

-- | Create a new set populated with initial elements.
newSet :: S.Set a -> Par d s (ISet s a)
newSet s = WrapPar$ fmap (ISet . WrapLVar) $ newLV$ newIORef s

-- | Create a new 'ISet' drawing initial elements from an existing list.
newFromList :: Ord a => [a] -> Par d s (ISet s a)
newFromList ls = newSet (S.fromList ls)

-- (Todo: in production you might want even more ... like going from a Vector)

--------------------------------------------------------------------------------
-- Quasi-deterministic ops:
--------------------------------------------------------------------------------

type QPar = Par QuasiDet 

-- | Freeze an 'ISet' after a specified callback/handler is done running.  This
-- differs from withCallbacksThenFreeze by not taking an additional action to run in
-- the context of the handlers.
--
--    (@'freezeSetAfter' 's' 'f' == 'withCallbacksThenFreeze' 's' 'f' 'return ()' @)
freezeSetAfter :: ISet s a -> (a -> QPar s ()) -> QPar s ()
freezeSetAfter s f = withCallbacksThenFreeze s f (return ())
  
-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: Eq b => ISet s a -> (a -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (ISet (WrapLVar lv)) callback action =
    do
       hp  <- newPool 
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       WrapPar$ 
         freezeLVAfter lv (initCB hp res) deltCB
       -- We additionally have to quiesce here because we fork the inital set of
       -- callbacks on their own threads:
       quiesce hp
       IV.get res
  where
    deltCB x = return$ Just$ unWrapPar$ callback x
    initCB hp resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      set <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ do
        F.foldlM (\() v -> forkInPool hp$ callback v) () set -- Non-allocating traversal.
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-- | Get the exact contents of the set.  Using this may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
freezeSet :: ISet s a -> QPar s (S.Set a)
freezeSet (ISet (WrapLVar lv)) = WrapPar $ 
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

--------------------------------------------------------------------------------

-- | Add an (asynchronous) callback that listens for all new elements added to the set.
addHandler :: HandlerPool                 -- ^ pool to enroll in 
           -> ISet s a                    -- ^ Set to listen to
           -> (a -> Par d s ())           -- ^ callback
           -> Par d s ()
addHandler hp (ISet (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler hp lv globalCB (\x -> return$ Just$ unWrapPar$ callb x)
    return ()
  where
    globalCB ref = do
      set <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ 
        F.foldlM (\() v -> forkInPool hp $ callb v) () set -- Non-allocating traversal.

-- | Shorthandfor creating a new handler pool and adding a single handler to it.
forEach :: ISet s a -> (a -> Par d s ()) -> Par d s HandlerPool
forEach = forEachHP Nothing

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
putInSet :: Ord a => a -> ISet s a -> Par d s ()
putInSet !elm (ISet (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update set =
          let set' = S.insert elm set in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if S.size set' > S.size set
          then (set',Just elm)
          else (set,Nothing)


-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet s a -> Par d s ()
waitElem !elm (ISet (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.member elm set of
        True  -> return (Just ())
        False -> return (Nothing)
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise  = return Nothing 


-- | Wait on the SIZE of the set, not its contents.
waitSize :: Int -> ISet s a -> Par d s ()
waitSize !sz (ISet lv) = WrapPar$
    getLV (unWrapLVar lv) globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.size set >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

--------------------------------------------------------------------------------
-- Higher level routines that could be defined using the above interface.
--------------------------------------------------------------------------------

-- | Return a fresh set which will contain strictly more elements than the input set.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: Ord a => ISet s a -> Par d s (ISet s a)
copy = traverseSet return 

-- | Establish monotonic map between the input and output sets.
traverseSet :: Ord b => (a -> Par d s b) -> ISet s a -> Par d s (ISet s b)
traverseSet f s = fmap snd $ traverseSetHP Nothing f s

-- | An imperative-style, inplace version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par d s b) -> ISet s a -> ISet s b -> Par d s ()
traverseSet_ f s o = void $ traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
union s1 s2 = do
  hp <- newPool
  os <- newEmptySet
  addHandler hp s1 (`putInSet` os)
  addHandler hp s2 (`putInSet` os)
  return os

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
-- Well, for now we cheat and use liftIO:
intersection s1 s2 = do
  hp <- newPool
  os <- newEmptySet
  addHandler hp s1 (fn os s2)
  addHandler hp s2 (fn os s1)
  return os
 where  
  fn outSet other@(ISet lv) elm = do
    -- At this point 'elm' has ALREADY been added to "us", we check "them":    
    peek <- LI.liftIO$ readIORef (state lv)
    if S.member elm peek 
      then putInSet elm outSet
      else return ()

-- | Cartesian product of two sets.
cartesianProd :: (Ord a, Ord b) => ISet s a -> ISet s b -> Par d s (ISet s (a,b))
cartesianProd s1 s2 =
  fmap snd $ 
  cartesianProdHP Nothing s1 s2 
  
-- | Takes the cartesian product of several sets.
cartesianProds :: Ord a => [ISet s a] -> Par d s (ISet s [a])
cartesianProds ls =
  fmap snd $ 
  cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- TODO: unionHP, intersectionHP...

-- | Variant of 'forEach' that optionally uses an existing handler pool.
forEachHP :: Maybe HandlerPool -> ISet s a -> (a -> Par d s ()) -> Par d s HandlerPool
forEachHP mh is cb = do 
   hp <- fromMaybe newPool (fmap return mh)
   addHandler hp is cb
   return hp

traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a ->
                 Par d s (HandlerPool, ISet s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  hp <- traverseSetHP_ mh fn set os  
  return (hp,os)

traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a -> ISet s b ->
                  Par d s HandlerPool
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    putInSet x' os

-- | Variant of 'cartesianProd' that exposes the handler pool for quiescing and
-- optionally uses an existing, input handler pool.
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet s a -> ISet s b ->
                   Par d s (HandlerPool, ISet s (a,b))
cartesianProdHP mh s1 s2 = do
  -- This is implemented much like intersection:
  hp <- fromMaybe newPool (fmap return mh)
  os <- newEmptySet
  addHandler hp s1 (fn os s2 (\ x y -> (x,y)))
  addHandler hp s2 (fn os s1 (\ x y -> (y,x)))
  return (hp,os)
 where
  -- This is expensive, but we've got to do it from both sides to counteract races:
  fn outSet other@(ISet lv) cmbn elm1 = do
    peek <- LI.liftIO$ readIORef (state lv)
--    liftIO $ putStrLn " ! Cartesian prod handler running..."
    F.foldlM (\() elm2 -> putInSet (cmbn elm1 elm2) outSet) () peek

-- cartesian :: S.Set t -> S.Set (t, t)
-- cartesian x = S.fromDistinctAscList [(i,j) | i <- xs, j <- xs]
--     where xs = S.toAscList x

-- | Variant of 'cartesianProds' that exposes the handler pool for quiescing and
-- optionally uses an existing, input handler pool.
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet s a] ->
                    Par d s (HandlerPool, ISet s [a])
cartesianProdsHP mh [] = do s <- newEmptySet
                            h <- fromMaybe newPool (fmap return mh) -- Pointless!
                            return (h,s)
cartesianProdsHP mh ls = do
  -- We use one handler pool for the whole network of related sets we create:
  hp <- fromMaybe newPool (fmap return mh)  
#if 1
  -- Case 1: recursive definition in terms of pairwise products:
  -- It would be best to create a balanced tree of these, I believe:
  let loop [lst]     = traverseSetHP (Just hp) (\x -> return [x]) lst -- Inefficient!
      loop (nxt:rst) = do
        (_, partial) <- loop rst
        (_,p1) <- cartesianProdHP (Just hp) nxt partial
        traverseSetHP (Just hp) (\ (x,tl) -> return (x:tl)) p1 -- Inefficient!!
  (_, x) <- loop ls
  return (hp, x)
#else
  os <- newEmptySet
  let loop done [] acc = acc
      loop done (nxt:rest) acc =
        addHandler hp nxt (fn os done rest)
        
--  forM_ ls $ \ inSet -> do 
--    addHandler hp s1 (fn os s2 (\ x y -> (x,y)))

  return os
 where
  fn outSet left right newElm = do
    peeksL <- liftIO$ mapM (readIORef . state . unISet) left
    peeksR <- liftIO$ mapM (readIORef . state . unISet) right

--    F.foldlM (\() elm2 -> putInSet (cmbn elm1 elm2) outSet) () peek
    return undefined
#endif


--------------------------------------------------------------------------------
-- Set specific DeepFreeze instances:
--------------------------------------------------------------------------------

-- Teach it how to freeze WITHOUT the annoying snapshot constructor:

instance DeepFreeze (ISet s a) (S.Set a) where
  type Session (ISet s a) = s
  deepFreeze iv = do ISetSnap m <- freeze iv
                     return m

#if 0
------------------------------------------------------------
-- Most general, but causes overlapping instances
------------------------------------------------------------    
instance (DeepFreeze a b, Ord b, Session a ~ s0) =>
         DeepFreeze (ISet s0 a) (S.Set b)
  where
    type Session (ISet s0 a) = s0
    deepFreeze = freezer     
      
freezer :: forall a b s0 . (DeepFreeze a b, Ord b, Session a ~ s0) =>
           (ISet s0 a) -> Par QuasiDet s0 (S.Set b)
freezer from = do
      x <- freezeSet from
      let fn :: a -> S.Set b -> QPar s0 (S.Set b)
          fn elm acc = do elm' <- deepFreeze elm
                          return (S.insert elm' acc)
      y <- F.foldrM fn S.empty x
      return y

#else
------------------------------------------------------------
-- Compromise to avoid overlap
------------------------------------------------------------
instance (LVarData1 f, DeepFreeze (f s0 a) b, Ord b) =>
         DeepFreeze (ISet s0 (f s0 a)) (S.Set b)  where
    type Session (ISet s0 (f s0 a)) = s0
    deepFreeze from = do
      x <- freezeSet from
      let fn :: f s0 a -> S.Set b -> QPar s0 (S.Set b)
          fn elm acc = do elm' <- deepFreeze elm
                          return (S.insert elm' acc)
      y <- F.foldrM fn S.empty x 
      return y      
#endif

