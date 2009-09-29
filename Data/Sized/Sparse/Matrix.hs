{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Data.Sized.Sparse.Matrix where
	
import Data.Sized.Ix
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
fromAssocList :: (Size i, Eq a) => a -> [(i,a)] -> Matrix i a
fromAssocList d xs = Matrix d (Map.fromList [ (i,a) | (i,a) <- xs, a /= d ])

toAssocList (Matrix d mp) = (d,Map.toList mp)

-- | '!' looks up an element in the sparse matrix. If the element is not found
-- in the sparse matrix, '!' returns the default value.
(!) :: (Size ix) => Matrix ix a -> ix -> a
(!) (Matrix d sm) id = Map.findWithDefault d id sm 

fill :: (Size ix) => Matrix ix a -> M.Matrix ix a
fill sm = M.forAll $ \ i -> sm ! i

-- Might be just internal, because nothing else leaks defaults.
prune :: (Size ix, Eq a) => a -> Matrix ix a -> Matrix ix a
prune d sm@(Matrix d' m) | d == d'   = Matrix d (Map.filter (/= d) m)
	  	         | otherwise = sparse d (fill sm)	-- it might be possible to do better; think about it

-- | Make a Matrix sparse, with a default 'zero' value.
sparse :: (Size ix, Eq a) => a -> M.Matrix ix a -> Matrix ix a
sparse d other = Matrix d (Map.fromList [ (i,v) | (i,v) <- M.assocs other, v /= d ])

foldb1 f [x] = x
foldb1 f xs = foldb1 f (take len_before xs) `f` foldb1 f (drop len_before xs)
  where len = length xs
	len_before = len `div` 2

mm :: (Size m, Size n, Size m', Size n', n ~ m', Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm s1 s2 = Matrix 0 mp
  where
	mp = Map.fromList [ ((x,y),v)
			| (x,y) <- M.indices_
			, let s = (rs M.! x) `Set.intersection` (cs M.! y)	 
			, not (Set.null s)
			, let v = foldb1 (+) [ s1 ! (x,k) * s2 ! (k,y) | k <- Set.toList s ]
			, v /= 0
			] 
	sm1@(Matrix _ mp1) = prune 0 s1
	sm2@(Matrix _ mp2) = prune 0 s2
	rs = rowSets    (Map.keysSet mp1)
	cs = columnSets (Map.keysSet mp2)

rowSets :: (Size a, Ord b) => Set (a,b) -> M.Matrix a (Set b)
rowSets set = M.accum f (pure Set.empty) (Set.toList set)
   where
	f set e = Set.insert e set
	
columnSets :: (Size b, Ord a) => Set (a,b) -> M.Matrix b (Set a)
columnSets = rowSets . Set.map (\ (a,b) -> (b,a))

instance (Size i) => Applicative (Matrix i) where
	pure a =  Matrix a (Map.empty)
	sm1@(Matrix d1 m1) <*> sm2@(Matrix d2 m2)
		= Matrix (d1 d2) (Map.fromList [ (k,(sm1 ! k) (sm2 ! k)) | k <- Set.toList keys ])
	    where keys = Map.keysSet m1 `Set.union` Map.keysSet m2

instance (Show a, Size ix,Size (Row ix), Size (Column ix)) => Show (Matrix ix a) where
	show m = show (fill m)

transpose :: (Size x, Size y, Eq a) => Matrix (x,y) a -> Matrix (y,x) a
transpose (Matrix d m) = Matrix d (Map.fromList [ ((y,x),a) | ((x,y),a) <- Map.assocs m ])

m1 = M.matrix [1..6] :: M.Matrix (X2,X3) Int
m2 = M.matrix [1..12] :: M.Matrix (X3,X4) Int
m3 = m1 `M.mm` m2
m4 = M.identity :: M.Matrix (X200,X200) Int


zipWith :: (Size x) => (a -> b -> c) -> Matrix x a -> Matrix x b -> Matrix x c
zipWith f m1 m2 = pure f <*> m1 <*> m2 
	
	