{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, ScopedTypeVariables,
  UndecidableInstances, MultiParamTypeClasses, TypeOperators, DataKinds #-}

module Main where

import Data.Sized.Sized
import Data.Sized.Matrix

import Test.QuickCheck as QC
import QC
import qualified Data.Sized.Sparse.Matrix as SM
import Control.Applicative

import Data.Array

-- Small first cut at tests.
main :: IO ()
main = do
	quickCheck prop_mm1
	quickCheck prop_fmap1
--	quickCheck prop_joins
	putStrLn "[Done]"

prop_mm1 :: Matrix (Sized 3, Sized 4) Int
         -> Matrix (Sized 4, Sized 5) Int
         -> Matrix (Sized 5, Sized 2) Int
         -> Bool
prop_mm1 m1 m2 m3 =  ((m1 `mm` m2) `mm` m3) == (m1 `mm` (m2 `mm` m3))
  where
	_types = (m1 :: Matrix (Sized 3, Sized 4) Int,
		 m2 ::  Matrix (Sized 4, Sized 5) Int,
		 m3 ::  Matrix (Sized 5, Sized 2) Int)

prop_fmap1 :: Matrix (Sized 9, Sized 29) Int -> Bool
prop_fmap1 m1 = fmap (+1) m1 == forEach m1 (\ _i a -> a + 1)
  where
	_types = (m1 :: Matrix (Sized 9, Sized 29) Int)

-- prop_joins m1 m2 m3 m4 = (m1 `above` m3) `beside` (m2 `above` m4)
-- 		      == (m1 `beside` m2) `above` (m3 `beside` m4)
--   where _types = (m1 :: Matrix (Sized 3, Sized 4) Int,
-- 		  m4 :: Matrix (Sized 7, Sized 5) Int)

{-
  Very simple test that fails to compile:

    Couldn't match type `3 GHC.TypeLits.+ 7' with `7 GHC.TypeLits.+ 3'
    NB: `GHC.TypeLits.+' is a type function, and may not be injective
    Expected type: 7 GHC.TypeLits.+ 3
      Actual type: 3 GHC.TypeLits.+ 7
    In the first argument of `(==)', namely `(m1 `above` m2)'

prop_joins :: Matrix (Sized 3, Sized 4) Int
           -> Matrix (Sized 7, Sized 4) Int
           -> Bool
prop_joins m1 m2 = (m1 `above` m2) == (m2 `above` m1)
  where _types = (m1 :: Matrix (Sized 3, Sized 4) Int,
                  m2 :: Matrix (Sized 7, Sized 4) Int)

-}
