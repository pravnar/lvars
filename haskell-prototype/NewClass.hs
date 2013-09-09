{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, CPP,
             FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module NewClass where

import GHC.Prim (Constraint)
import Control.DeepSeq (NFData, deepseq)

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
--
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
-- 
class Monad m => ParFuture future m | m -> future where
-- class Monad m => ParFuture m where
  
  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.
--   type Future a 

  -- | Different implementations may place different constraints on
  -- what is allowable inside a Future.  For example, some
  -- implementations require an Eq Constraint.
  type FutContents a :: Constraint

  -- | Create a potentially-parallel computation, and return a /future/
  -- (or /promise/) that can be used to query the result of the forked
  -- computataion.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --
  spawn  :: (NFData a, FutContents a) => m a -> m (future a)

  -- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: FutContents a => m a -> m (future a)

  -- | Wait for the result of a future, and then return it.
  get    :: future a -> m a

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  --
  -- >  spawnP = spawn . return
  spawnP :: (NFData a, FutContents a) =>   a -> m (future a)

  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)


