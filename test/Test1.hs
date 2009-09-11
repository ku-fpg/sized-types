{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables  #-}
module Main where
	
import Data.Sized.Ix
import Data.Sized.Matrix

import Test.QuickCheck as QC
import Data.Sized.QC.Ix
import Data.Sized.QC.Matrix
import Control.Applicative
import Data.Sized.Arith

import Data.Array

main = do
	quickCheck prop_mm1
	quickCheck prop_fmap1
	quickCheck prop_joins
	putStrLn "[Done]"

prop_mm1 m1 m2 m3 =  ((m1 `mm` m2) `mm` m3) == (m1 `mm` (m2 `mm` m3))
  where
	types = (m1 :: Matrix (X3,X4) Int,
		 m2 :: Matrix (X4,X5) Int,
		 m3 :: Matrix (X5,X2) Int)
		
prop_fmap1 m1 = fmap (+1) m1 == forEach m1 (\ i a -> a + 1)
  where
	types = (m1 :: Matrix (X9,X29) Int)

prop_joins m1 m2 m3 m4 = (m1 `above` m3) `beside` (m2 `above` m4)
		      == (m1 `beside` m2) `above` (m3 `beside` m4)
  where types = (m1 :: Matrix (X3,X4) Int,
		 m4 :: Matrix (X7,X5) Int)

	      