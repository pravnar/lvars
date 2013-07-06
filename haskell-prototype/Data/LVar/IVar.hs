{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-} 
module Data.LVar.IVar
--       (IVar, new, get, put, put_, spawn, spawn_, spawnP, freezeIVar)
       where

import           Data.IORef
import           Control.DeepSeq
import qualified Control.Monad.Par.Class as PC
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO)

import           Data.Traversable (traverse)

import           Control.LVish as LV
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV,
                                                DeepFreeze(..), unsafeUnQPar)
import qualified Control.LVish.SchedIdempotent as LI 
import           Data.Traversable (traverse)

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):
newtype IVar s a = IVar (LVar s (IORef (Maybe a)) a)

instance Eq (IVar s a) where
  (==) (IVar lv1) (IVar lv2) = state lv1 == state lv2

{-
instance LVarData1 (IVar s) where
  -- type Snapshot IVar a = Maybe a
  newtype Snapshot (IVar s) a = IVarSnap (Maybe a)
    deriving (Show,Ord,Read,Eq)
  freeze    = fmap IVarSnap . freezeIVar
  newBottom = new
  traverseSnap f (IVarSnap m) = fmap IVarSnap $ traverse f m

unSnap :: Snapshot (IVar s) a -> Maybe a
unSnap (IVarSnap m) = m
-}
--------------------------------------

new :: Par d s (IVar s a)
new = WrapPar$ fmap (IVar . WrapLVar) $
      newLV $ newIORef Nothing
      
-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- @IVar@.
get :: IVar s a -> Par d s a
get (IVar (WrapLVar iv)) = WrapPar$ getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Jusbt _
        deltaThresh  x     = return $ Just x  -- always past threshold
        
-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--         
-- Strict up to WHNF in the element put.
put_ :: Eq a => IVar s a -> a -> Par d s ()
put_ (IVar (WrapLVar iv)) !x = WrapPar $ putLV iv putter
  where putter ref      = atomicModifyIORef ref update

        update (Just y) | x == y = (Just y, Just y)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               error$ "Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")"
        update Nothing  = (Just x, Just x)


freezeIVar :: IVar s a -> LV.Par QuasiDet s (Maybe a)
freezeIVar (IVar (WrapLVar lv)) = WrapPar$ 
  do freezeLV lv
     getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

--------------------------------------------------------------------------------

spawn :: (Eq a, NFData a) => Par d s a -> Par d s (IVar s a)
spawn p  = do r <- new;  fork (p >>= put r);   return r
              
spawn_ :: Eq a => Par d s a -> Par d s (IVar s a)
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

spawnP :: (Eq a, NFData a) => a -> Par d s (IVar s a)
spawnP a = spawn (return a)

put :: (Eq a, NFData a) => IVar s a -> a -> Par d s ()
put v a = deepseq a (put_ v a)

{-
instance PC.ParFuture IVar Par where
  spawn_ = spawn_
  get = get

instance PC.ParIVar IVar Par where
  fork = fork  
  put_ = put_
  new = new
-}


--------------------------------------------------------------------------------
-- IVar specific DeepFreeze instances:
--------------------------------------------------------------------------------

{-
-- Teach it how to freeze WITHOUT the annoying snapshot constructor:
instance DeepFreeze (IVar s a) (Maybe a) where
  deepFreeze iv = do IVarSnap m <- freeze iv
                     return m

instance DeepFreeze (IVar s a) b =>
         DeepFreeze (IVar s (IVar s a)) (Maybe b)
  where
    deepFreeze (from :: (IVar (IVar a))) = do
      x <- freezeIVar from       :: Par QuasiDet s (Maybe (IVar a))
      y <- traverse deepFreeze x :: Par QuasiDet s (Maybe b)
      return y

-}
