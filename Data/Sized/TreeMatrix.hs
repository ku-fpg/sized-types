module Data.Sized.TreeMatrix where 

import Data.Sized.Vector
import qualified Data.List as L
import Data.Maybe (fromJust)
import Debug.Trace


data TreeMatrix a = SplitH (TreeMatrix a) (TreeMatrix a)
                    | SplitV (TreeMatrix a) (TreeMatrix a)
                    | NonZero Int (Vector Int a)
                    | Zero Int
                 deriving Show

-- matrixtoSubMatrix should take a matrix and split it 
toTreeMatrix v s | pleaseSplitV = SplitV (toTreeMatrix vLeft s) (toTreeMatrix vRight s)
                      | pleaseSplitH = SplitH (toTreeMatrix vTop s) (toTreeMatrix vBottom s)
                      | isZeros v    = Zero s
--                      | trace (show ("argh")) False = undefined
                      | otherwise    = NonZero shiftBy (fromIdentity v shiftBy)
    where pleaseSplitV = numCols > s
          pleaseSplitH = numRows > s
          shiftBy = fromJust $ L.findIndex (/= 0) $ toList v
          (vLeft,vRight) = splitVectorV v (numCols `div` 2 `div` s * s)
          (vTop,vBottom) = splitVectorH v (numRows `div` 2 `div` s * s)
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


fromTreeMatrix :: (Num a) => TreeMatrix a -> Vector (Int,Int) a
fromTreeMatrix (Zero s)            = zeros (s,s)
fromTreeMatrix (NonZero x v)       = shiftH x $ toIdentity v
fromTreeMatrix (SplitH top bot)    = (fromTreeMatrix top) `above` (fromTreeMatrix bot)
fromTreeMatrix (SplitV left right) = (fromTreeMatrix left) `beside` (fromTreeMatrix right)

toIdentity :: (Bounds ix, Num a) => Vector ix a -> Vector (ix,ix) a
toIdentity v = vector (ix,ix) [ if x == y then v ! x else 0 | x <- ids, y <- ids]
    where ix = size v
          ids = indices v

fromIdentity :: (Bounds ix, Integral ix, Num a) => Vector (ix,ix) a -> ix -> Vector ix a
fromIdentity v s = vector ix [ v ! (x,(y+s) `mod` ix) | (x,y) <- indices v, x == y]
    where (ix,_) = size v

isZeros :: (Bounds ix, Num a) => Vector ix a -> Bool
isZeros v = sum (toList v) == 0

zeros :: (Bounds ix, Num a) => ix -> Vector ix a
zeros ix  = vector ix (repeat 0)

shiftH :: (Bounds x, Bounds y, Integral y) => 
          y -> Vector (x,y) a -> Vector (x,y) a
shiftH n v = ixmap (xmax,ymax) (\(a,b) -> (a,(b - n) `mod` ymax)) v
    where (xmax,ymax) = size v
