{-# LANGUAGE EmptyDataDecls #-}

-- | This module is NOT Safe-Haskell, but it must be used to create
-- new LVar types.
module Control.LVish.DeepFrz.Types
       (
         Frzn, Trvrsbl
       )
       where

-- | An uninhabited type that signals an LVar has been frozen.
--   LVars should use this inplace of their `s` parameter.
data Frzn

-- | An uninhabited type that signals an LVar is not only frozen, but
-- it may be traversed in whatever order its internal representation
-- dictates.
data Trvrsbl 
