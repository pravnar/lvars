{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
             FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module NewClass where

import GHC.Prim (Constraint)
import Control.DeepSeq (NFData, deepseq)

import qualified Control.Monad.Par as MP 
import qualified Data.LVar.IVar as LI
import qualified Control.LVish as LVish

-- class Monad m => ParFuture m where
  
  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.
--   type Future a 



-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
--
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
-- 
class Monad m => ParFuture future m | m -> future where

  -- | Different implementations may place different constraints on
  -- what is allowable inside a Future.  For example, some
  -- implementations may require an `Eq` Constraint.
  type FutContents future a :: Constraint

  -- | Create a potentially-parallel computation, and return a /future/
  -- (or /promise/) that can be used to query the result of the forked
  -- computataion.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --
  spawn  :: (NFData a, FutContents future a) => m a -> m (future a)

  -- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: FutContents future a => m a -> m (future a)

  -- | Wait for the result of a future, and then return it.
  get    :: future a -> m a

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  --
  -- >  spawnP = spawn . return
  spawnP :: (NFData a, FutContents future a) =>   a -> m (future a)

  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)

test1 :: String
test1 = MP.runPar $ do
  x <- MP.spawn $ return "hello"
  MP.get x

-- We achieve backwards compatibility simply by putting in a null constraint:
instance ParFuture MP.IVar MP.Par  where
  type FutContents MP.IVar a = ()
  get    = MP.get
  spawn  = MP.spawn
  spawn_ = MP.spawn_
  spawnP = MP.spawnP

test2 :: String
test2 = MP.runPar $ do
  x <- spawn $ return "hello"
  get x

instance ParFuture (LI.IVar s) (LVish.Par d s) where
  type FutContents (LI.IVar s) a = (Eq a)
  spawn_ m = LI.spawn_ m
  get iv = LI.get iv

test3 :: String
test3 = LVish.runPar $ do
  x <- spawn $ return "hello"
  get x

