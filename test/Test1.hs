{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Main where

import Data.Sized.Matrix

import QC.QC()
import Test.QuickCheck as QC
-- import qualified Data.Sized.Sparse.Matrix as SM


-- NatType equivalences required for the join tests.
--type instance (4 + 5) = 9
--type instance (3 + 7) = 10

-- Small first cut at tests.
main :: IO ()
main = do
    quickCheck prop_mm1
    quickCheck prop_fmap1
    quickCheck prop_joins
    putStrLn "[Done]"

prop_mm1 :: Vector2 3 4 Int
         -> Vector2 4 5 Int
         -> Vector2 5 2 Int
         -> Bool
prop_mm1 m1 m2 m3 =  ((m1 `mm` m2) `mm` m3) == (m1 `mm` (m2 `mm` m3))
  where
    _types = (m1 :: Vector2 3 4 Int,
              m2 ::  Vector2 4 5 Int,
              m3 ::  Vector2 5 2 Int)

prop_fmap1 :: Vector2 9 29 Int -> Bool
prop_fmap1 m1 = fmap (+1) m1 == forEach m1 (\ _i a -> a + 1)
  where
    _types = (m1 :: Vector2 9 29 Int)

prop_joins :: Vector2 3 4 Int
           -> Vector2 3 5 Int
           -> Vector2 7 4 Int
           -> Vector2 7 5 Int
           -> Bool
prop_joins m1 m2 m3 m4 = (m1 `above` m3) `beside` (m2 `above` m4)
                      == (m1 `beside` m2) `above` (m3 `beside` m4)
  where _types = (m1 :: Vector2 3 4 Int,
                  m4 :: Vector2 7 5 Int)
