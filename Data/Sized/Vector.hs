
{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances, FlexibleInstances #-}

module Data.Sized.Vector where

import qualified Data.Array as A
import qualified Data.List as L

data Vector ix a = Vector (A.Array ix a)
--	deriving Show

vector :: (Bounds ix) => ix -> [a] -> Vector ix a
vector ix vals = Vector (A.listArray (toBounds ix) vals)

instance (Bounds ix) => Functor (Vector ix) where
	fmap f (Vector xs) = Vector (fmap f xs)

class (A.Ix ix) => Bounds ix where
  toBounds :: ix -> (ix,ix)
  fromBounds :: (ix,ix) -> ix
  range    :: (ix,ix) -> [ix]

instance Bounds Int where
  toBounds ix = (0,ix - 1)
  fromBounds (low,high) = (high - low) + 1
  range (low,high) = [low..high]

instance (Bounds a, Bounds b) => Bounds (a,b) where
  toBounds (ix1,ix2) = ((l1,l2),(h1,h2))
	where (l1,h1) = toBounds ix1
	      (l2,h2) = toBounds ix2
  fromBounds ((l1,l2),(h1,h2)) = (ix1,ix2)
	where ix1 = fromBounds (l1,h1)
	      ix2 = fromBounds (l2,h2)
  range ((l1,l2),(h1,h2)) = [(x,y) | x <- range (l1,h1), y <- range (l2,h2)]

(!) :: (Bounds ix) => Vector ix a -> ix -> a
(!) (Vector a) x = a A.! x

toList :: (Bounds ix) => Vector ix a -> [a]
toList (Vector a) = A.elems a

assocs :: (Bounds ix) => Vector ix a -> [(ix,a)]
assocs (Vector a) = A.assocs a

size :: Bounds ix => Vector ix a -> ix
size (Vector a) = fromBounds $ A.bounds a

bounds v = toBounds $ size v

indices :: (Bounds ix) => Vector ix a -> [ix]
indices (Vector a) = A.indices a

ixmap :: (Bounds i, Bounds j) => i -> (i -> j) -> Vector j a -> Vector i a
ixmap b f v = vector b [v ! f idx | idx <- range (toBounds b)]

transpose :: (Bounds x, Bounds y) => Vector (x,y) a -> Vector (y,x) a
transpose v = ixmap (y',x') (\ (x,y) -> (y,x)) v
    where (x',y') = size v

rows :: (Bounds x, Bounds y) => Vector (x,y) a -> Vector x (Vector y a)
rows v = vector xmax $ map (vector ymax) [[v ! (x,y) | y <- range (yl,yh)] | x <- range (xl,xh)]
         where (xmax,ymax) = size v
               ((xl,yl),(xh,yh)) = bounds v

cols :: (Bounds x, Bounds y) => Vector (x,y) a -> Vector y (Vector x a)
cols v = vector ymax $ map (vector xmax) [[v ! (x,y) | x <- range (xl,xh)] | y <- range (yl,yh)]
         where (xmax,ymax) = size v
               ((xl,yl),(xh,yh)) = bounds v

above :: (Bounds x, Bounds y, Num x, Num y) => Vector (x,y) a -> Vector (x,y) a -> Vector (x,y) a
above v1 v2 | numcols v1 == numcols v2 = vector (numrows v1 + numrows v2, numcols v1) xs
            | otherwise            = error "Column count mismatch"
            where numcols v = snd $ size v
                  numrows v = fst $ size v
                  xs = toList v1 ++ toList v2

beside :: (Bounds x, Bounds y, Num x, Num y) => Vector (x,y) a -> Vector (x,y) a -> Vector (x,y) a
beside v1 v2 = transpose $ transpose v1 `above` transpose v2

show' v = showMatrix' (size v) (foo v)

foo v = toList $ fmap toList $ rows $ fmap show v

instance (Show a, Bounds ix) => Show (Vector (ix,ix) a) where
	show arr = show' arr



--instance (Show a, Size ix,Size (Row ix), Size (Column ix)) => Show (Vector ix a) where
--	show arr = showMatrix' (fmap show (ixmap seeIn2D arr))

showMatrix' :: (Bounds ix) => (ix,ix) -> [[String]] -> String
showMatrix' (x,y) m = joinLines $ L.zipWith showRow m (map (const False) (init m) ++ [True])
	where
		joinLines   = unlines . L.zipWith (++) ("[":repeat " ") 
		showRow	r f  = concat (L.zipWith3 showEle r m_cols_size (map (const False) (init r) ++ [f]))
		showEle str s f = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then " ]" else ",")
		m_cols      = L.transpose m
		m_cols_size = fmap (maximum . map L.length) m_cols
