-- | Sized matrixes.
-- 
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Data.Sized.Matrix 
	( module Data.Sized.Matrix
	, module Data.Sized.Ix
	) where

import Data.Array as A hiding (indices,(!), ixmap, assocs)
import qualified Data.Array as A
import Prelude as P hiding (all)
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.List as L 
import Numeric 

import Data.Sized.Ix

-- | A 'Matrix' is an array with the sized determined uniquely by the 
-- /type/ of the index type, 'ix'. 
data Matrix ix a = Matrix (Array ix a)
	deriving Eq

-- | '!' looks up an element in the matrix.
(!) :: (Size n) => Matrix n a -> n -> a
(!) (Matrix xs) n = xs A.! n

instance (Size i) => Functor (Matrix i) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

-- | 'toList' turns a matrix into an always finite list.
toList :: (Size i) => Matrix i a -> [a]
toList (Matrix a) = elems a

-- | 'fromList' turns a finite list into a matrix. You often need to give the type of the result.
fromList :: (Size i) => [a] -> Matrix i a
fromList xs = check minBound maxBound
    where 
	check low high | size low == L.length xs
		       = Matrix $ listArray (low,high) xs
		       | otherwise
		       = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show (L.length (range (low,high))) ++ " elements"
			      ++ ", found " ++ show (L.length xs) ++ " elements."

-- | 'matrix' turns a finite list into a matrix. You often need to give the type of the result.
matrix :: (Size i) => [a] -> Matrix i a
matrix = fromList

-- | 'indices' is a version of 'Data.Sized.Ix.all' that takes a type, for forcing the result type using the Matrix type.
indices :: (Size i) => Matrix i a -> [i]
indices _ = all

-- | what is the length of a matrix?
length :: (Size i) => Matrix i a -> Int
length = size . zeroOf

-- | 'assocs' extracts the index/value pairs.
assocs :: (Size i) => Matrix i a -> [(i,a)]
assocs (Matrix a) = A.assocs a

(//) :: (Size i) => Matrix i e -> [(i, e)] -> Matrix i e
(//) (Matrix arr) ixs = Matrix (arr A.// ixs)

accum :: (Size i) => (e -> a -> e) -> Matrix i e -> [(i, a)] -> Matrix i e
accum f (Matrix arr) ixs = Matrix (A.accum f arr ixs)

-- | 'zeroOf' is for use to force typing issues, and is 0.
zeroOf :: (Size i) => Matrix i a -> i
zeroOf _ = minBound

-- | 'coord' returns a matrix filled with indexes.
coord :: (Size i) => Matrix i i
coord = fromList all

-- | Same as for lists.
zipWith :: (Size i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f a b = forAll $ \ i -> f (a ! i) (b ! i)

-- | 'forEach' takes a matrix, and calls a function for each element, to give a new matrix of the same size.
forEach :: (Size i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forEach a f = Data.Sized.Matrix.zipWith f coord a

-- | 'forAll' creates a matrix out of a mapping from the coordinates.
forAll :: (Size i) => (i -> a) -> Matrix i a
forAll f = fmap f coord

instance (Size i) => Applicative (Matrix i) where
	pure a = fmap (const a) coord	-- possible because we are a fixed size
	a <*> b = forAll $ \ i -> (a ! i) (b ! i)
	
-- | 'mm' is the 2D matrix multiply.
mm :: (Size m, Size n, Size m', Size n', n ~ m', Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- all ]
 
-- | 'transpose' a 2D matrix.
transpose :: (Size x, Size y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap $ \ (x,y) -> (y,x)

-- | return the identity for a specific matrix size.
identity :: (Size x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord

-- | stack two matrixes 'above' each other.
above :: (Size m, Size top, Size bottom, Size both
	 , ADD top bottom ~ both
	 , SUB both top ~ bottom
	 , SUB both bottom ~ top 
	 ) 
      => Matrix (top,m) a -> Matrix (bottom,m) a -> Matrix (both,m) a
above m1 m2 = fromList (toList m1 ++ toList m2)

-- | stack two matrixes 'beside' each other.
beside
  :: (Size m,
      Size left,
      Size right,
      Size both
     , ADD left right ~ both
     , SUB both left ~ right
     , SUB both right ~ left
     ) =>
     Matrix (m, left) a -> Matrix (m, right) a -> Matrix (m, both) a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

-- | append two 1-d matrixes
append ::
     (Size left,
      Size right,
      Size both
     , ADD left right ~ both
     , SUB both left ~ right
     , SUB both right ~ left
     ) => Matrix left a -> Matrix right a -> Matrix both a
append m1 m2 = fromList (toList m1 ++ toList m2)

-- | look at a matrix through a lens to another matrix.
ixmap :: (Size i, Size j) => (i -> j) -> Matrix j a -> Matrix i a
ixmap f m = (\ i -> m ! f i) <$> coord

-- | look at a matrix through a functor lens, to another matrix.
ixfmap :: (Size i, Size j, Functor f) => (i -> f j) -> Matrix j a -> Matrix i (f a)
ixfmap f m = (fmap (\ j -> m ! j) . f) <$> coord

-- | grab /part/ of a matrix.
cropAt :: (Index i ~ Index ix, Size i, Size ix) => Matrix ix a -> ix -> Matrix i a
cropAt m corner = ixmap (\ i -> (addIndex corner (toIndex i))) m

-- | slice a 2D matrix into rows.
rows :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- all ]) <$> coord

-- | slice a 2D matrix into columns.
columns :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

-- | join a matrix of matrixes into a single matrix.
joinRows :: (Bounded n, Size n, Bounded m, Size m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinRows a = (\ (m,n) -> (a ! m) ! n) <$> coord

-- | join a matrix of matrixes into a single matrix.
joinColumns :: (Bounded n, Size n, Bounded m, Size m) => Matrix n (Matrix m a) -> Matrix (m,n) a
joinColumns a = (\ (m,n) -> (a ! n) ! m) <$> coord

-- | generate a 2D single row from a 1D matrix.
unitRow :: (Size m, Bounded m) => Matrix m a -> Matrix (X1, m) a
unitRow = ixmap snd

-- | generate a 1D matrix from a 2D matrix.
unRow :: (Size m, Bounded m) => Matrix (X1, m) a -> Matrix m a
unRow = ixmap (\ n -> (0,n))

-- | generate a 2D single column from a 1D matrix.
unitColumn :: (Size m, Bounded m) => Matrix m a -> Matrix (m, X1) a
unitColumn = ixmap fst

-- | generate a 1D matrix from a 2D matrix.
unColumn :: (Size m, Bounded m) => Matrix (m, X1) a -> Matrix m a
unColumn = ixmap (\ n -> (n,0))

-- | very general; required that m and n have the same number of elements, rebundle please.
squash :: (Size n, Size m) => Matrix m a -> Matrix n a
squash = fromList . toList

instance (Size ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ toList a)
 
instance (Size ix) => F.Foldable (Matrix ix) where
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

showMatrix :: (Size n, Size m) => Matrix (m, n) String -> String
showMatrix m = joinLines $ map showRow m_rows
	where
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . L.zipWith (++) ("[":repeat " ") 
		showRow	r   = concat (toList $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
		showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then " ]" else ",")
		m_cols      = columns m
		m_rows      = toList $ rows m'
		m_cols_size = fmap (maximum . map L.length . toList) m_cols


instance (Show a, Size ix,Size (Row ix), Size (Column ix)) => Show (Matrix ix a) where
	show = showMatrix . fmap show . ixmap seeIn2D 

-- | 'S' is shown as the contents, without the quotes.
-- One use is a matrix of S, so that you can do show-style functions
-- using fmap.
newtype S = S String

instance Show S where
	show (S s) = s

showAs :: (RealFloat a) => Int -> a -> S 
showAs i a = S $ showEFloat (Just i) a ""

scanM :: (Size ix, Bounded ix, Enum ix)
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

scanL :: (Size ix, Bounded ix, Enum ix)
      => ((a,right) -> (right,b))
      -> (Matrix ix a,right)
      -> (right,Matrix ix b)
scanL = error "to be written"

scanR :: (Size ix, Bounded ix, Enum ix)
      => ((left,a) -> (b,left))
      -> (left, Matrix ix a)
      -> (Matrix ix b,left)
scanR f (l,m) = ( fst `fmap` tmp, snd (tmp ! maxBound) )
  where tmp = forEach m $ \ i a -> f (prev i,a)
	prev i = if i == minBound then l else (snd (tmp ! (pred i)))

 
