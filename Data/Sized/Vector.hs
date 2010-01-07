
{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances  #-}

module Data.Sized.Vector where

import qualified Data.Array as A
import Data.List as L

data Vector ix a = Vector (A.Array ix a)
	deriving Show

vector :: (Bounds ix) => ix -> [a] -> Vector ix a
vector ix vals = Vector (A.listArray (toBounds ix) vals)

class (A.Ix ix) => Bounds ix where
  toBounds :: ix -> (ix,ix)
  fromBounds :: (ix,ix) -> ix

instance Bounds Int where
  toBounds ix = (0,ix - 1)
  fromBounds (low,high) = (high - low) + 1

instance (Bounds a, Bounds b) => Bounds (a,b) where
  toBounds (ix1,ix2) = ((l1,l2),(h1,h2))
	where (l1,h1) = toBounds ix1
	      (l2,h2) = toBounds ix2
  fromBounds ((l1,l2),(h1,h2)) = (ix1,ix2)
	where ix1 = fromBounds (l1,h1)
	      ix2 = fromBounds (l2,h2)

toList :: (Bounds ix) => Vector ix a -> [a]
toList (Vector a) = A.elems a

assocs :: (Bounds ix) => Vector ix a -> [(ix,a)]
assocs (Vector a) = A.assocs a

size :: Bounds ix => Vector ix a -> ix
size (Vector a) = fromBounds $ A.bounds a

indices (Vector a) = A.indices a

ixmap :: (Bounds i, Bounds j) => (i,i) -> (i -> j) -> Vector j a -> Vector i a
ixmap b f v = undefined -- (\ i -> v ! f i) <$> coord
              where is = undefined 

above :: (Bounds x, Bounds y, Num x, Num y) => Vector (x,y) a -> Vector (x,y) a -> Vector (x,y) a
above v1 v2 | cols v1 == cols v2 = vector (rows v1 + rows v2, cols v1) xs
            | otherwise            = error "Column count mismatch"
            where cols v = snd $ size v
                  rows v = fst $ size v
                  xs = toList v1 ++ toList v2



--instance (Show a, Size ix,Size (Row ix), Size (Column ix)) => Show (Vector ix a) where
--	show arr = showMatrix' (fmap show (ixmap seeIn2D arr))

	
	

showMatrix' :: (Int,Int) -> [[String]] -> String
showMatrix' (x,y) m = joinLines $ L.zipWith showRow m (map (const False) (init m) ++ [True])
	where
		joinLines   = unlines . L.zipWith (++) ("[":repeat " ") 
		showRow	r f  = concat (L.zipWith3 showEle r m_cols_size (map (const False) (init r) ++ [f]))
		showEle str s f = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then " ]" else ",")
		m_cols      = L.transpose m
		m_cols_size = fmap (maximum . map L.length) m_cols
