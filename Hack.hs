{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}


import Data.Sized.Matrix
import Data.Sized.Vector as V
import Data.Array as A hiding (indices,(!), ixmap, assocs)
import Data.List as L

x :: Matrix (X2,X3) Int
x = matrix [1..6]

-- x :: Matrix (x,y) a -> Matrix (Int,Int) a

y :: Matrix (Int,Int) Int
y = matrix2 (2,3) [1..6]

matrix2 :: (Int,Int) -> [a] -> Matrix (Int,Int) a
matrix2 (x,y) vs = Matrix (listArray ((0,0),(x-1,y-1)) vs)


