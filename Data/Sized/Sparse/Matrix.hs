-- | Sparse Matrix.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, ScopedTypeVariables,
  UndecidableInstances, MultiParamTypeClasses, TypeOperators, DataKinds #-}
module Data.Sized.Sparse.Matrix where

import Data.Array.Base as B
import Data.Ix
import Data.Sized.Fin as X
import qualified Data.Sized.Matrix as M
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative

data SpMatrix ix a = SpMatrix a (Map ix a)

instance Functor (SpMatrix ix) where
    fmap f (SpMatrix d mp) = SpMatrix (f d) (fmap f mp)

-- 'fromAssocList' generates a sparse matrix.
fromAssocList :: (Ord i, Eq a) => a -> [(i,a)] -> SpMatrix i a
fromAssocList d xs = SpMatrix d (Map.fromList [ (i,a) | (i,a) <- xs, a /= d ])

toAssocList :: (SpMatrix i a) -> (a,[(i,a)])
toAssocList (SpMatrix d mp) = (d,Map.toList mp)

-- | 'getElem' looks up an element in the sparse matrix. If the element is not found
-- in the sparse matrix, 'getElem' returns the default value.
getElem :: (Ord ix) => SpMatrix ix a -> ix -> a
getElem (SpMatrix d sm) ix = Map.findWithDefault d ix sm

fill :: (Bounded ix, Ix ix) => SpMatrix ix a -> M.Matrix ix a
fill sm = M.forAll $ \ i -> getElem sm i

-- Might be just internal, because nothing else leaks defaults.
prune :: (Bounded ix, Ix ix, Eq a) => a -> SpMatrix ix a -> SpMatrix ix a
prune d sm@(SpMatrix d' m) | d == d'   = SpMatrix d (Map.filter (/= d) m)
	  	         | otherwise = sparse d (fill sm)	-- it might be possible to do better; think about it

-- | Make a Matrix sparse, with a default 'zero' value.
sparse :: (Bounded ix, Ix ix, Eq a) => a -> M.Matrix ix a -> SpMatrix ix a
sparse d other = SpMatrix d (Map.fromList [ (i,v) | (i,v) <- assocs other, v /= d ])

mm :: (Bounded m, Ix m, Bounded n, Ix n, Bounded m', Ix m', Bounded n', Ix n', n ~ m', Num a, Eq a) =>
      SpMatrix (m,n) a -> SpMatrix (m',n') a -> SpMatrix (m,n') a
mm s1 s2 = SpMatrix 0 mp
  where
	mp = Map.fromList [ ((x,y),v)
			| (x,y) <- X.universe
			, let s = (rs B.! x) `Set.intersection` (cs B.! y)
			, not (Set.null s)
			, let v = foldb1 (+) [(getElem s1  (x,k)) * (getElem s2 (k,y)) | k <- Set.toList s ]
			, v /= 0
			]
	(SpMatrix _ mp1) = prune 0 s1
	(SpMatrix _ mp2) = prune 0 s2
	rs = rowSets    (Map.keysSet mp1)
	cs = columnSets (Map.keysSet mp2)

	foldb1 _ [x] = x
	foldb1 f xs = foldb1 f (take len_before xs) `f` foldb1 f (drop len_before xs)
	  where len = length xs
	  	len_before = len `div` 2



rowSets :: (Bounded a, Ix a, Ord b) => Set (a,b) -> M.Matrix a (Set b)
rowSets set = B.accum f (pure Set.empty) (Set.toList set)
   where
	f set' e = Set.insert e set'

columnSets :: (Bounded b, Ix b, Ord a) => Set (a,b) -> M.Matrix b (Set a)
columnSets = rowSets . Set.map (\ (a,b) -> (b,a))

instance (Bounded i, Ix i) => Applicative (SpMatrix i) where
	pure a =  SpMatrix a (Map.empty)
	sm1@(SpMatrix d1 m1) <*> sm2@(SpMatrix d2 m2)
		= SpMatrix (d1 d2) (Map.fromList [ (k, (getElem sm1  k) (getElem sm2 k)) | k <- Set.toList keys ])
	    where keys = Map.keysSet m1 `Set.union` Map.keysSet m2

instance (Show a, Show ix, Bounded ix, Ix ix) => Show (SpMatrix ix a) where
    show m = show (fill m)

transpose :: (Bounded x, Ix x, Bounded y, Ix y, Eq a) => SpMatrix (x,y) a -> SpMatrix (y,x) a
transpose (SpMatrix d m) = SpMatrix d (Map.fromList [ ((y,x),a) | ((x,y),a) <- Map.assocs m ])

zipWith :: (Bounded x, Ix x) => (a -> b -> c) -> SpMatrix x a -> SpMatrix x b -> SpMatrix x c
zipWith f m1 m2 = pure f <*> m1 <*> m2
