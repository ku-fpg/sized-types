{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}


import Data.Sized.Matrix (Matrix(Matrix),matrix)
import Data.Sized.Ix hiding (size)
-- import qualified Data.Sized.Matrix as M
import Data.Sized.Vector as V
import Data.Array as A hiding (indices,(!), ixmap, assocs)
import qualified Data.List as L

x :: Matrix (X2,X3) Int
x = matrix [1..6]

-- x :: Matrix (x,y) a -> Matrix (Int,Int) a

y :: Matrix (Int,Int) Int
y = matrix2 (2,3) [1..6]

matrix2 :: (Int,Int) -> [a] -> Matrix (Int,Int) a
matrix2 (x,y) vs = Matrix (listArray ((0,0),(x-1,y-1)) vs)



data SubMatrix a = SplitH (SubMatrix a) (SubMatrix a)
                    | SplitV (SubMatrix a) (SubMatrix a)
                    | NonZero Int (Vector Int a)
                    | Zero Int

-- matrixtoSubMatrix should take a matrix and split it 
vectorToSubMatrix v s | pleaseSplitV = SplitV (vectorToSubMatrix vLeft s) (vectorToSubMatrix vRight s)
                      | pleaseSplitH = SplitH (vectorToSubMatrix vTop s) (vectorToSubMatrix vBottom s)
                      | otherwise   = NonZero numRows (fromIdentity v)
    where pleaseSplitV = numCols > s
          pleaseSplitH = numRows > s
          (vLeft,vRight) = splitVectorV v (numCols `div` 2)
          (vTop,vBottom) = splitVectorH v (numRows `div` 2)
          (numRows,numCols) = size v

splitVectorV v col | col < numCols = (vLeft,vRight)
                   | otherwise     = error "Column number out of bounds"
    where vLeft  = vector (numRows,col) [v ! (x,y) | (x,y) <- indices v, y<col]
          vRight = vector (numRows,numCols-col) [v ! (x,y) | (x,y) <- indices v, y>=col]
          (numRows,numCols) = size v

splitVectorH v row | row < numRows = (vTop,vBottom)
                   | otherwise     = error "Row number out of bounds"
    where vTop  = vector (row,numCols) [v ! (x,y) | (x,y) <- indices v, x<row]
          vBottom = vector (numRows-row,numCols) [v ! (x,y) | (x,y) <- indices v, x>=row]
          (numRows,numCols) = size v


subMatrixToVector :: (Num a) => SubMatrix a -> Vector (Int,Int) a
subMatrixToVector (Zero s)            = zeros (s,s)
subMatrixToVector (NonZero x v)       = shiftH x $ toIdentity v
subMatrixToVector (SplitH top bot)    = (subMatrixToVector top) `above` (subMatrixToVector bot)
subMatrixToVector (SplitV left right) = (subMatrixToVector left) `beside` (subMatrixToVector right)

toIdentity :: (Bounds ix, Num a) => Vector ix a -> Vector (ix,ix) a
toIdentity v = vector (ix,ix) [ if x == y then v ! x else 0 | x <- ids, y <- ids]
    where ix = size v
          ids = indices v

fromIdentity :: (Bounds ix, Num a) => Vector (ix,ix) a -> Vector ix a
fromIdentity v = vector ix [ v ! (x,y) | (x,y) <- indices v, x == y]
    where (ix,_) = size v


zeros :: (Bounds ix, Num a) => ix -> Vector ix a
zeros ix  = vector ix (repeat 0)

shiftH :: (Bounds x, Bounds y, Integral y) => 
          y -> Vector (x,y) a -> Vector (x,y) a
shiftH n v = ixmap (xmax,ymax) (\(a,b) -> (a,(b - n) `mod` ymax)) v
    where (xmax,ymax) = size v

-- shift' x y | x + y > maxBound = (x + y) - maxBound - 1
--            | otherwise        = x + y
