-- | Sized matrixes.
--
-- Copyright: (c) 2013 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, ScopedTypeVariables,
  UndecidableInstances, MultiParamTypeClasses, TypeOperators, DataKinds,
  FlexibleContexts, DeriveDataTypeable, CPP #-}
module Data.Sized.Matrix where

import Prelude as P hiding (all)
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.List as L hiding (all)
import Data.Array.Base as B
import Data.Array.IArray as I
import GHC.TypeLits (type (+))
#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif
import Numeric

import Data.Sized.Fin

-- | A 'Matrix' is an array with the size determined uniquely by the
-- /type/ of the index type, 'ix', with every type in 'ix' used.
newtype Matrix ix a = Matrix (Array ix a)
        deriving ( Eq
                 , Ord
#if __GLASGOW_HASKELL__ >= 708
                 , Typeable
#endif
                 )

-- | A 'Vector' is a 1D Matrix, using a TypeNat to define its length.
type Vector  (ix :: Nat) a = Matrix (Fin ix) a

-- | A 'Vector2' is a 2D Matrix, using a TypeNat's to define its size.
type Vector2 (ix :: Nat) (iy :: Nat) a = Matrix (Fin ix,Fin iy) a

instance (Ix ix) => Functor (Matrix ix) where
    fmap f (Matrix xs) = Matrix (fmap f xs)

instance IArray Matrix a where
   bounds (Matrix arr) = B.bounds arr
   numElements (Matrix arr) = B.numElements arr
   unsafeArray (a,b) ass = Matrix $ B.unsafeArray (a,b) ass
   unsafeAt (Matrix arr) i = B.unsafeAt arr i

instance (Bounded i, Ix i) => Applicative (Matrix i) where
    pure a = fmap (const a) coord   -- possible because we are a fixed size
                                    -- Also why use use newtype here.
    a <*> b = forAll $ \ i -> (a ! i) (b ! i)

-- | 'matrix' turns a finite list into a matrix. You often need to give the type of the result.
matrix :: forall i a . (Bounded i, Ix i) => [a] -> Matrix i a
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
population :: forall i a . (Bounded i, Ix i) => Matrix i a -> Int
population _ = rangeSize (minBound :: i,maxBound)

allIndices :: (Bounded i, Ix i) => Matrix i a -> [i]
allIndices _ = universe

-- | 'zeroOf' is for use to force typing issues, and is 0.
zeroOf :: (Bounded i, Ix i) => Matrix i a -> i
zeroOf _ = minBound

-- | 'coord' returns a matrix filled with indexes.
coord :: (Bounded i, Ix i) => Matrix i i
coord = matrix universe

-- | Same as for lists.
zipWith :: (Bounded i, Ix i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f a b = forAll $ \ i -> f (a ! i) (b ! i)

-- | 'forEach' takes a matrix, and calls a function for each element, to give a new matrix of the same size.
forEach :: (Bounded i, Ix i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forEach a f = Data.Sized.Matrix.zipWith f coord a

-- | 'forAll' creates a matrix out of a mapping from the coordinates.
forAll :: (Bounded i, Ix i) => (i -> a) -> Matrix i a
forAll f = fmap f coord

-- | 'mm' is the 2D matrix multiply.
mm :: (Bounded m, Ix m, Bounded n, Ix n, Bounded o, Ix o, Num a) => Matrix (m,n) a -> Matrix (n,o) a -> Matrix (m,o) a
mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- universe ]

-- | 'transpose' a 2D matrix.
transpose :: (Bounded x, Ix x, Bounded y, Ix y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap corners $ \ (x,y) -> (y,x)

-- | return the identity for a specific matrix size.
identity :: (Bounded x, Ix x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord

-- | append to 1D vectors
append :: (SingI left, SingI right, SingI (left + right))
      => Vector left a -> Vector right a -> Vector (left + right) a
append m1 m2 = matrix (I.elems m1 ++ I.elems m2)

-- TODO.  Is the type constraint for 'both' sufficient ?
-- In an earlier version we had:
--         , ADD top bottom ~ both
--         , SUB both top ~ bottom
--         , SUB both bottom ~ top

-- | stack two matrixes 'above' each other.

above :: (SingI top, SingI bottom, SingI y, SingI (top + bottom))
      => Vector2 top y a -> Vector2 bottom y a -> Vector2 (top + bottom) y a
above m1 m2 = matrix (I.elems m1 ++ I.elems m2)

-- | stack two matrixes 'beside' each other.
beside :: (SingI left, SingI right, SingI x, SingI (left + right))
      => Vector2 x left a -> Vector2 x right a -> Vector2 x (left + right) a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

-- | look at a matrix through a functor lens, to another matrix.
ixfmap :: (Bounded i, Ix i, Bounded j, Ix j, Functor f) => (i -> f j) -> Matrix j a -> Matrix i (f a)
ixfmap f m = (fmap (\ j -> m ! j) . f) <$> coord

-- FIXME.  This is difficult to do with the simplifications appearing Sized.
-- The Index class no longer exists (which required addIndex)
-- Is this required ???

-- | grab /part/ of a matrix.
--cropAt :: (Index i ~ Index ix, Bounded i, Ix i, Bounded ix, Ix ix) => Matrix ix a -> ix -> Matrix i a
--cropAt m corner = ixmap (\ i -> (addIndex corner (toIndex i))) m

-- | slice a 2D matrix into rows.
rows :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- universe ]) <$> coord

-- | slice a 2D matrix into columns.
columns :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

-- | join a matrix of matrixes into a single matrix.
joinRows :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinRows a = (\ (m,n) -> (a ! m) ! n) <$> coord

-- | join a matrix of matrixes into a single matrix.
joinColumns :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix n (Matrix m a) -> Matrix (m,n) a
joinColumns a = (\ (m,n) -> (a ! n) ! m) <$> coord

instance (Bounded ix, Ix ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ I.elems a)

instance (Bounded ix, Ix ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (I.elems m)

-- | 'show2D' displays a 2D matrix, and is the worker for 'show'.
--
-- > GHCi> matrix [1..42] :: Matrix (Fin 7, Fin 6) Int
-- > [  1,  2,  3,  4,  5,  6,
-- >    7,  8,  9, 10, 11, 12,
-- >   13, 14, 15, 16, 17, 18,
-- >   19, 20, 21, 22, 23, 24,
-- >   25, 26, 27, 28, 29, 30,
-- >   31, 32, 33, 34, 35, 36,
-- >   37, 38, 39, 40, 41, 42 ]
-- >

show2D :: (Bounded n, Ix n, Bounded m, Ix m, Show a) => Matrix (m, n) a -> String
show2D m0 = (joinLines $ map showRow m_rows)
  where
    m           = fmap show m0
    m'          = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
    joinLines   = unlines . addTail . L.zipWith (++) ("[":repeat " ")
    addTail xs  = init xs ++ [last xs ++ " ]"]
    showRow r   = concat (I.elems $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
    showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then "" else ",")
    m_cols      = columns m
    m_rows      = I.elems $ rows m'
    m_cols_size = fmap (maximum . map L.length . I.elems) m_cols

instance (Show a, Show ix, Bounded ix, Ix ix) => Show (Matrix ix a) where
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
