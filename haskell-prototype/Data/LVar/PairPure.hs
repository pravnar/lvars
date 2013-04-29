{-# LANGUAGE BangPatterns #-}

module Data.LVar.PairPure
       ( IPair,
         newPair,
         putFst,
         putSnd,
         getFst,
         getSnd, 
         ) where
import LVarTracePure

------------------------------------------------------------------------------
-- IPairs implemented on top of LVars:
------------------------------------------------------------------------------

type IPair a b = LVar (IVarContents a, IVarContents b)

newPair :: Par (IPair a b)
newPair = newLV (IVC Nothing,
                 IVC Nothing)

putFst :: (Show a, Show b) => IPair a b -> a -> Par ()
putFst lv !elt = putLV lv (IVC (Just elt), IVC Nothing)

putSnd :: (Show a, Show b) => IPair a b -> b -> Par ()
putSnd lv !elt = putLV lv (IVC Nothing, IVC (Just elt))

getFst :: (Show a, Show b) => IPair a b -> Par a
getFst lv = getLV lv test
 where
   test (IVC (Just x),_) = Just x
   test (IVC Nothing,_)  = Nothing

getSnd :: IPair a b -> Par b
getSnd lv = getLV lv test
 where
   test (_,IVC (Just x)) = Just x
   test (_,IVC Nothing)  = Nothing

