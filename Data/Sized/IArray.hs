{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, TypeOperators, GADTs, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Data.Sized.IArray where

import Data.Array as A hiding (indices,(!), ixmap, assocs)
import qualified Data.Array as A
import Prelude as P hiding (all)
import Control.Applicative
import qualified Data.Traversable as T hiding (all)
import qualified Data.Foldable as F hiding (all)
import qualified Data.List as L hiding (all)
import Numeric
import Data.Array.Base as B
import Data.Array.IArray as I
import GHC.TypeLits

import Data.Sized.Sized

-- | 'fromList' turns a finite list into a (n-dimentional) matrix. You often need to give the type of the result.
matrix :: forall m i a . (IArray m a, SizedIx i) => [a] -> m i a
matrix xs | size' == fromIntegral (L.length xs) = I.listArray (low,high) xs
	  | otherwise = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show size' ++ " elements"
			      ++ ", found " ++ show (L.length xs) ++ " elements."

    where
        size' = rangeSize (low,high)
  	low :: i
	low = minBound
	high :: i
	high = maxBound


-- | 'coord' returns a matrix filled with indexes.
coord :: (IArray m i, SizedIx i) => m i i
coord = matrix universe

-- | 'forAll' creates a matrix out of a mapping from the coordinates.
forAll :: (IArray m a, SizedIx i) => (i -> a) -> m i a
forAll f = I.array corners [ (i,f i) | i <- universe ]

-- | 'forEach' takes a matrix, and calls a function for each element, to give a new matrix of the same size.
forEach :: (IArray m a, IArray m b, SizedIx i) => m i a -> (i -> a -> b) -> m i b
forEach a f = I.array corners [ (i,f i (a ! i)) | i <- universe ]

-- | 'transpose' a 2D matrix.
transpose :: (IArray m a, SizedIx x, SizedIx y) => m (x,y) a -> m (y,x) a
transpose = I.ixmap corners $ \ (x,y) -> (y,x)

-- | 'mm' is the 2D matrix multiply.
mm :: (IArray mx a, SizedIx m, SizedIx n, SizedIx o, Num a) => mx (m,n) a -> mx (n,o) a -> mx (m,o) a
mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- universe ]

-- | slice a 2D matrix into rows.
rows :: (SizedIx n, SizedIx m, IArray mx a, IArray mx (mx n a)) => mx (m,n) a -> mx m (mx n a)
rows a = forAll $ \ m -> forAll $ \ n -> a ! (m,n)

-- | slice a 2D matrix into columns.
columns :: (SizedIx n, SizedIx m, IArray mx a, IArray mx (mx m a)) => mx (m,n) a -> mx n (mx m a)
columns a = forAll $ \ n -> forAll $ \ m -> a ! (m,n)

-- A 'Vector' is a 1D Matrix.
type Vector ix a = Array (Sized ix) a

-- | append two 1-d matrixes
append :: (SingI left, SingI right, SingI (left + right)) => Vector left a -> Vector right a -> Vector (left + right) a
append m1 m2 = matrix (I.elems m1 ++ I.elems m2)

type instance (5 + 5) = 10

-------------------------------------------------------------------------------------


show2D :: forall m n a . (Show a, SizedIx m, SizedIx n) => ((m, n) -> a) -> String
show2D ff = (joinLines $ map showRow m_rows)
	where
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . addTail . L.zipWith (++) ("[":repeat " ")
		addTail xs  = init xs ++ [last xs ++ " ]"]
		showRow	r   = concat (zipWith showEle (I.elems r) m_cols_size)
		showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then "" else ",")
                m           :: A.Array (m,n) String
                m           = forAll (show . ff)
		m_cols      = columns m
		m_rows      = I.elems $ rows m'
		m_cols_size = map (maximum . map L.length . I.elems) (I.elems m_cols)

data S = S String

instance Show S where
	show (S s) = s

showAsE :: (RealFloat a) => Int -> a -> S
showAsE i a = S $ showEFloat (Just i) a ""

showAsF :: (RealFloat a) => Int -> a -> S
showAsF i a = S $ showFFloat (Just i) a ""

--------------------------------------------------------------------------------

-- We take the liberty of overloading Array here. It is only possible because
-- we have sized types.

instance (SizedIx ix) => Applicative (Array ix) where
        pure a  = forAll $ \ _ -> a
        m <*> a = forAll $ \ i -> (m ! i) (a ! i)
