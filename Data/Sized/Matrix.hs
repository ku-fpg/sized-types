-- | Sized matrixes.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses #-}
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

import Data.Sized.Sized


-- | A 'Matrix' is an array with the sized determined uniquely by the
-- /type/ of the index type, 'ix', with every type in 'ix' used.
data Matrix ix a = Matrix (Array ix a)
        deriving Show

{-
class M (m :: * -> * -> *) where
  (.!) :: (SizedIx ix) => m ix a -> ix -> a
  toList' :: m i a -> [a]
  fromList' :: [a] -> m i a
-}
-- TODO: add instances

instance (Ix ix) => Functor (Matrix ix) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

-- A 'Vector' is a 1D Matrix.
type Vector ix a = Matrix (Sized ix) a

instance IArray Matrix a where
   bounds (Matrix arr) = B.bounds arr
   numElements (Matrix arr) = B.numElements arr
   unsafeArray (a,b) ass = Matrix $ B.unsafeArray (a,b) ass
   unsafeAt (Matrix arr) i = B.unsafeAt arr i

-- | '!' looks up an element in the matrix.
---(!) ::  (Ix n) => Matrix n a -> n -> a
--(!) (Matrix xs) n = xs A.! n

-- TODO: remove toList (use elems) and fromList (use matrix).

-- | 'toList' turns a matrix into an always finite list.
toList :: (Ix i) => Matrix i a -> [a]
toList (Matrix a) = A.elems a

-- | 'fromList' turns a finite list into a (n-dimentional) matrix. You often need to give the type of the result.
fromList :: forall m i a . (IArray m a, SizedIx i) => [a] -> m i a
fromList xs | size' == fromIntegral (L.length xs) = I.listArray (low,high) xs
	    | otherwise = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show size' ++ " elements"
			      ++ ", found " ++ show (L.length xs) ++ " elements."

    where
        size' = rangeSize (low,high)
  	low :: i
	low = minBound
	high :: i
	high = maxBound

-- | 'matrix' turns a finite list into a matrix. You often need to give the type of the result.
matrix :: forall m i a . (IArray m a, SizedIx i) => [a] -> m i a
matrix = fromList

-- | 'indices' is a version of 'Data.Sized.Ix.all' that takes a type, for forcing the result type using the Matrix type.
indices :: (SizedIx i) => Matrix i a -> [i]
indices _ = universe

-- | what is the population of a matrix?
population :: forall i a . (SizedIx i) => Matrix i a -> Int
population _ = rangeSize (minBound :: i,maxBound)

-- | 'assocs' extracts the index/value pairs.
--assocs :: (SizedIx i) => Matrix i a -> [(i,a)]
--assocs (Matrix a) = A.assocs a

--(//) :: (SizedIx i) => Matrix i e -> [(i, e)] -> Matrix i e
--(//) (Matrix arr) ixs = Matrix (arr A.// ixs)

--accum :: (SizedIx i) => (e -> a -> e) -> Matrix i e -> [(i, a)] -> Matrix i e
--accum f (Matrix arr) ixs = Matrix (A.accum f arr ixs)

-- | 'zeroOf' is for use to force typing issues, and is 0.
--zeroOf :: (SizedIx i) => Matrix i a -> i
--zeroOf _ = minBound

-- | 'coord' returns a matrix filled with indexes.
coord :: (SizedIx i) => Matrix i i
coord = fromList universe

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
	a <*> b = forAll $ \ i -> (a ! i) (b ! i)

-- | 'mm' is the 2D matrix multiply.
--mm :: (IArray mx a, SizedIx m, SizedIx n, SizedIx o, Num a) => mx (m,n) a -> mx (n,o) a -> mx (m,o) a
--mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- universe ]

{-

-- | 'transpose' a 2D matrix.
transpose :: (SizedIx x, SizedIx y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap $ \ (x,y) -> (y,x)

-- | return the identity for a specific matrix size.
identity :: (SizedIx x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord

-- | stack two matrixes 'above' each other.
above :: (SizedIx m, SizedIx top, SizedIx bottom, SizedIx both
	 , ADD top bottom ~ both
	 , SUB both top ~ bottom
	 , SUB both bottom ~ top
	 )
      => Matrix (top,m) a -> Matrix (bottom,m) a -> Matrix (both,m) a
above m1 m2 = fromList (toList m1 ++ toList m2)

-- | stack two matrixes 'beside' each other.
beside
  :: (SizedIx m,
      SizedIx left,
      SizedIx right,
      SizedIx both
     , ADD left right ~ both
     , SUB both left ~ right
     , SUB both right ~ left
     ) =>
     Matrix (m, left) a -> Matrix (m, right) a -> Matrix (m, both) a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

-- | append two 1-d matrixes
append ::
     (SizedIx left,
      SizedIx right,
      SizedIx both
     , ADD left right ~ both
     , SUB both left ~ right
     , SUB both right ~ left
     ) => Matrix left a -> Matrix right a -> Matrix both a
append m1 m2 = fromList (toList m1 ++ toList m2)

-- | look at a matrix through a lens to another matrix.
ixmap :: (SizedIx i, SizedIx j) => (i -> j) -> Matrix j a -> Matrix i a
ixmap f m = (\ i -> m ! f i) <$> coord

-- | look at a matrix through a functor lens, to another matrix.
ixfmap :: (SizedIx i, SizedIx j, Functor f) => (i -> f j) -> Matrix j a -> Matrix i (f a)
ixfmap f m = (fmap (\ j -> m ! j) . f) <$> coord

-- | grab /part/ of a matrix.
cropAt :: (Index i ~ Index ix, SizedIx i, SizedIx ix) => Matrix ix a -> ix -> Matrix i a
cropAt m corner = ixmap (\ i -> (addIndex corner (toIndex i))) m

-- | slice a 2D matrix into rows.
rows :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- all ]) <$> coord

-- | slice a 2D matrix into columns.
columns :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

-- | join a matrix of matrixes into a single matrix.
joinRows :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinRows a = (\ (m,n) -> (a ! m) ! n) <$> coord

-- | join a matrix of matrixes into a single matrix.
joinColumns :: (Bounded n, SizedIx n, Bounded m, SizedIx m) => Matrix n (Matrix m a) -> Matrix (m,n) a
joinColumns a = (\ (m,n) -> (a ! n) ! m) <$> coord

-- | generate a 2D single row from a 1D matrix.
unitRow :: (SizedIx m, Bounded m) => Matrix m a -> Matrix (X1, m) a
unitRow = ixmap snd

-- | generate a 1D matrix from a 2D matrix.
unRow :: (SizedIx m, Bounded m) => Matrix (X1, m) a -> Matrix m a
unRow = ixmap (\ n -> (0,n))

-- | generate a 2D single column from a 1D matrix.
unitColumn :: (SizedIx m, Bounded m) => Matrix m a -> Matrix (m, X1) a
unitColumn = ixmap fst

-- | generate a 1D matrix from a 2D matrix.
unColumn :: (SizedIx m, Bounded m) => Matrix (m, X1) a -> Matrix m a
unColumn = ixmap (\ n -> (n,0))

-- | very general; required that m and n have the same number of elements, rebundle please.
squash :: (SizedIx n, SizedIx m) => Matrix m a -> Matrix n a
squash = fromList . toList

instance (Bounded i, Ix ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ toList a)

instance (SizedIx ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (toList m)

-- | 'showMatrix' displays a 2D matrix, and is the worker for 'show'.
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

showMatrix :: (SizedIx n, SizedIx m) => Matrix (m, n) String -> String
showMatrix m = (joinLines $ map showRow m_rows)
	where
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . addTail . L.zipWith (++) ("[":repeat " ")
		addTail xs  = init xs ++ [last xs ++ " ]"]
		showRow	r   = concat (toList $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
		showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then "" else ",")
		m_cols      = columns m
		m_rows      = toList $ rows m'
		m_cols_size = fmap (maximum . map L.length . toList) m_cols


instance (Show a, SizedIx ix) => Show (Matrix ix a) where
	show = showMatrix . fmap show . unitRow

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

scanM :: (SizedIx ix, Bounded ix, Enum ix)
      => ((left,a,right) -> (right,b,left))
      -> (left, Matrix ix a,right)
      -> (right,Matrix ix b,left)
scanM f (l,m,r) =  ( fst3 (tmp ! minBound), snd3 `fmap` tmp, trd3 (tmp ! maxBound) )
  where tmp = forEach m $ \ i a -> f (prev i, a, next i)
	prev i = if i == minBound then l else (trd3 (tmp ! (pred i)))
	next i = if i == maxBound then r else (fst3 (tmp ! (succ i)))
	fst3 (a,_,_) = a
	snd3 (_,b,_) = b
	trd3 (_,_,c) = c

scanL :: (SizedIx ix, Bounded ix, Enum ix)
      => ((a,right) -> (right,b))
      -> (Matrix ix a,right)
      -> (right,Matrix ix b)
scanL = error "to be written"

scanR :: (SizedIx ix, Bounded ix, Enum ix)
      => ((left,a) -> (b,left))
      -> (left, Matrix ix a)
      -> (Matrix ix b,left)
scanR f (l,m) = ( fst `fmap` tmp, snd (tmp ! maxBound) )
  where tmp = forEach m $ \ i a -> f (prev i,a)
	prev i = if i == minBound then l else (snd (tmp ! (pred i)))
-}

