-- | Sized matrixes.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, TypeOperators, DataKinds, DeriveDataTypeable #-}
module Data.Sized.Matrix where

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
import Data.Typeable
import Data.Dynamic

import Data.Sized.Sized


-- | A 'Matrix' is an array with the sized determined uniquely by the
-- /type/ of the index type, 'ix', with every type in 'ix' used.
data Matrix ix a = Matrix (Array ix a)
        deriving (Typeable)

instance (Ix ix) => Functor (Matrix ix) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

-- | A 'Vector' is a 1D Matrix.
type Vector  (ix :: Nat) a = Matrix (Sized ix) a

-- | A 'Vector2' is a 2D Matrix.
type Vector2 (ix :: Nat) (iy :: Nat) a = Matrix (Sized ix,Sized iy) a

instance IArray Matrix a where
   bounds (Matrix arr) = B.bounds arr
   numElements (Matrix arr) = B.numElements arr
   unsafeArray (a,b) ass = Matrix $ B.unsafeArray (a,b) ass
   unsafeAt (Matrix arr) i = B.unsafeAt arr i


-- | 'matrix' turns a finite list into a matrix. You often need to give the type of the result.
matrix :: forall i a . (SizedIx i) => [a] -> Matrix i a
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


-- | what is the population of a matrix?
population :: forall i a . (SizedIx i) => Matrix i a -> Int
population _ = rangeSize (minBound :: i,maxBound)

-- | 'coord' returns a matrix filled with indexes.
coord :: (SizedIx i) => Matrix i i
coord = matrix universe

-- | Same as for lists.
zipWith :: (SizedIx i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f a b = forAll $ \ i -> f (a ! i) (b ! i)

-- | 'forEach' takes a matrix, and calls a function for each element, to give a new matrix of the same size.
forEach :: (SizedIx i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forEach a f = Data.Sized.Matrix.zipWith f coord a

-- | 'forAll' creates a matrix out of a mapping from the coordinates.
forAll :: (SizedIx i) => (i -> a) -> Matrix i a
forAll f = fmap f coord

instance (SizedIx i) => Applicative (Matrix i) where
	pure a = fmap (const a) coord	-- possible because we are a fixed size
	                                -- Also why use use newtype here.
	a <*> b = forAll $ \ i -> (a ! i) (b ! i)

-- | 'mm' is the 2D matrix multiply.
mm :: (SizedIx m, SizedIx n, SizedIx o, Num a) => Matrix (m,n) a -> Matrix (n,o) a -> Matrix (m,o) a
mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- universe ]

-- | 'transpose' a 2D matrix.
transpose :: (SizedIx x, SizedIx y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap corners $ \ (x,y) -> (y,x)

identity :: (SizedIx x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord


-- | append to 1D vectors
append :: (SingI left, SingI right, SingI (left + right))
      => Vector left a -> Vector right a -> Vector (left + right) a
append m1 m2 = matrix (I.elems m1 ++ I.elems m2)

-- | stack two matrixes 'above' each other.

above :: (SingI top, SingI bottom, SingI y, SingI (top + bottom))
      => Vector2 top y a -> Vector2 bottom y a -> Vector2 (top + bottom) y a
above m1 m2 = matrix (I.elems m1 ++ I.elems m2)

-- | stack two matrixes 'beside' each other.
beside :: (SingI left, SingI right, SingI x, SingI (left + right))
      => Vector2 x left a -> Vector2 x right a -> Vector2 x (left + right) a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

-- | slice a 2D matrix into rows.
rows :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- universe ]) <$> coord

-- | slice a 2D matrix into columns.
columns :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

-- | join a matrix of matrixes into a single matrix.
joinRows :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinRows a = (\ (m,n) -> (a ! m) ! n) <$> coord

-- | join a matrix of matrixes into a single matrix.
joinColumns :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix n (Matrix m a) -> Matrix (m,n) a
joinColumns a = (\ (m,n) -> (a ! n) ! m) <$> coord

instance (SizedIx ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ I.elems a)

instance (SizedIx ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (I.elems m)

-- | 'show2D' displays a 2D matrix, and is the worker for 'show'.
--
-- > GHCi> matrix [1..42] :: Matrix (X7,X6) Int
-- > [  1,  2,  3,  4,  5,  6,
-- >    7,  8,  9, 10, 11, 12,
-- >   13, 14, 15, 16, 17, 18,
-- >   19, 20, 21, 22, 23, 24,
-- >   25, 26, 27, 28, 29, 30,
-- >   31, 32, 33, 34, 35, 36,
-- >   37, 38, 39, 40, 41, 42 ]
-- >


show2D :: (SizedIx n, SizedIx m, Show a) => Matrix (m, n) a -> String
show2D m0 = (joinLines $ map showRow m_rows)
	where
                m           = fmap show m0
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . addTail . L.zipWith (++) ("[":repeat " ")
		addTail xs  = init xs ++ [last xs ++ " ]"]
		showRow	r   = concat (I.elems $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
		showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then "" else ",")
		m_cols      = columns m
		m_rows      = I.elems $ rows m'
		m_cols_size = fmap (maximum . map L.length . I.elems) m_cols

instance (Show a, Show ix, SizedIx ix) => Show (Matrix ix a) where
        show m = "matrix " ++ show (I.bounds m) ++ " " ++ show (I.elems m)

-- TODO: read instance

-- | 'S' is shown as the contents, without the quotes.
-- One use is a matrix of S, so that you can do show-style functions
-- using fmap.
newtype S = S String

instance Show S where
	show (S s) = s

showAsE :: (RealFloat a) => Int -> a -> S
showAsE i a = S $ showEFloat (Just i) a ""

showAsF :: (RealFloat a) => Int -> a -> S
showAsF i a = S $ showFFloat (Just i) a ""

