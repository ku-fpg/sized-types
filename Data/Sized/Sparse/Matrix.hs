-- | Sparse Matrix.
-- 
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Data.Sized.Sparse.Matrix where
	
import Data.Sized.Ix as X
import qualified Data.Sized.Matrix as M
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative
		
data Matrix ix a = Matrix a (Map ix a)

instance Functor (Matrix ix) where
    fmap f (Matrix d mp) = Matrix (f d) (fmap f mp)

-- 'fromAssocList' generates a sparse matrix. 
fromAssocList :: (Ord i, Eq a) => a -> [(i,a)] -> Matrix i a
fromAssocList d xs = Matrix d (Map.fromList [ (i,a) | (i,a) <- xs, a /= d ])

toAssocList :: (Matrix i a) -> (a,[(i,a)])
toAssocList (Matrix d mp) = (d,Map.toList mp)

-- | '!' looks up an element in the sparse matrix. If the element is not found
-- in the sparse matrix, '!' returns the default value.
(!) :: (Ord ix) => Matrix ix a -> ix -> a
(!) (Matrix d sm) ix = Map.findWithDefault d ix sm 

fill :: (Size ix) => Matrix ix a -> M.Matrix ix a
fill sm = M.forAll $ \ i -> sm ! i

-- Might be just internal, because nothing else leaks defaults.
prune :: (Size ix, Eq a) => a -> Matrix ix a -> Matrix ix a
prune d sm@(Matrix d' m) | d == d'   = Matrix d (Map.filter (/= d) m)
	  	         | otherwise = sparse d (fill sm)	-- it might be possible to do better; think about it

-- | Make a Matrix sparse, with a default 'zero' value.
sparse :: (Size ix, Eq a) => a -> M.Matrix ix a -> Matrix ix a
sparse d other = Matrix d (Map.fromList [ (i,v) | (i,v) <- M.assocs other, v /= d ])

mm :: (Size m, Size n, Size m', Size n', n ~ m', Eq a, Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm s1 s2 = Matrix 0 mp
  where
	mp = Map.fromList [ ((x,y),v)
			| (x,y) <- X.all
			, let s = (rs M.! x) `Set.intersection` (cs M.! y)	 
			, not (Set.null s)
			, let v = foldb1 (+) [ s1 ! (x,k) * s2 ! (k,y) | k <- Set.toList s ]
			, v /= 0
			] 
	(Matrix _ mp1) = prune 0 s1
	(Matrix _ mp2) = prune 0 s2
	rs = rowSets    (Map.keysSet mp1)
	cs = columnSets (Map.keysSet mp2)

	foldb1 _ [x] = x
	foldb1 f xs = foldb1 f (take len_before xs) `f` foldb1 f (drop len_before xs)
	  where len = length xs
	  	len_before = len `div` 2



rowSets :: (Size a, Ord b) => Set (a,b) -> M.Matrix a (Set b)
rowSets set = M.accum f (pure Set.empty) (Set.toList set)
   where
	f set' e = Set.insert e set'
	
columnSets :: (Size b, Ord a) => Set (a,b) -> M.Matrix b (Set a)
columnSets = rowSets . Set.map (\ (a,b) -> (b,a))

instance (Size i) => Applicative (Matrix i) where
	pure a =  Matrix a (Map.empty)
	sm1@(Matrix d1 m1) <*> sm2@(Matrix d2 m2)
		= Matrix (d1 d2) (Map.fromList [ (k,(sm1 ! k) (sm2 ! k)) | k <- Set.toList keys ])
	    where keys = Map.keysSet m1 `Set.union` Map.keysSet m2

instance (Show a, Size ix) => Show (Matrix ix a) where
	show m = show (fill m)

transpose :: (Size x, Size y, Eq a) => Matrix (x,y) a -> Matrix (y,x) a
transpose (Matrix d m) = Matrix d (Map.fromList [ ((y,x),a) | ((x,y),a) <- Map.assocs m ])

zipWith :: (Size x) => (a -> b -> c) -> Matrix x a -> Matrix x b -> Matrix x c
zipWith f m1 m2 = pure f <*> m1 <*> m2 
	
	