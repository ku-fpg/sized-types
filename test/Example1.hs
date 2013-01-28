{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module Main where

import Data.Sized.Fin
import Data.Sized.Matrix
import Data.Sized.Signed as S
import Data.Sized.Unsigned as U
import Control.Applicative

import GHC.TypeLits

-- NatType equivalences required for the above and beside tests.
type instance (3 + 3) = 6
type instance (4 + 4) = 8


main :: IO ()
main = do
	print example1
	print example2
	print $ transpose example2
	print $ example2 `mm` transpose example2
	print $ fmap odd example2
	print $ example2 `above` example2
	print $ example2 `beside` example2
	print $ example3
	print $ example4
	print $ example5
	print $ example6
	print $ example7
--      cropAt function no longer supported
--	print $ example8
	print $ fmap (\ v -> if v == (0 :: Double)
		 	     then S ""
			     else showAsE 3 v)
	      $ fmap (fromIntegral) example6

	let s :: [Signed 4]
	    s = [ x * y | x <- [1..5], y <- [0..5]]
	print s

	let u :: [Unsigned 4]
	    u = [ x * y | x <- [1..5], y <- [0..5]]
	print u

	print $ fmap S.toVector s
	print $ fmap U.toVector u


example1 :: Matrix (Fin 5,Fin 5) Int
example1 = identity

example2 :: Matrix (Fin 3,Fin 4) Int
example2 = matrix [1..12]

example3 :: Matrix (Fin 4,Fin 5) Double
example3 = pure 1.2

example4 :: Matrix (Fin 4,Fin 5) (Fin 4,Fin 5)
example4 = coord

-- also works in 2D
example5 :: Matrix (Fin 6) Bool
example5 = forAll $ \ i -> i > 3

example6 :: Matrix (Fin 3,Fin 4) Int
example6 = forEach example2 $ \ (i,j) a ->
		if i == 0 || j == 0 then a else 0

example7 :: Matrix (Fin 10,Fin 10) Int
example7 = matrix [1..100]


--      cropAt function no longer supported
-- example8 :: Matrix (Fin 4,Fin 5) Int
-- example8 = example7 `cropAt` (2,3)
