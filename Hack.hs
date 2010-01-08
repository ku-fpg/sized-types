{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}


import Data.Sized.Matrix (Matrix(Matrix),matrix)
import Data.Sized.Ix hiding (size)
-- import qualified Data.Sized.Matrix as M
import Data.Sized.Vector as V
import Data.Array as A hiding (indices,(!), ixmap, assocs)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Debug.Trace

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
                 deriving Show

-- matrixtoSubMatrix should take a matrix and split it 
vectorToSubMatrix v s | pleaseSplitV = SplitV (vectorToSubMatrix vLeft s) (vectorToSubMatrix vRight s)
                      | pleaseSplitH = SplitH (vectorToSubMatrix vTop s) (vectorToSubMatrix vBottom s)
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


subMatrixToVector :: (Num a) => SubMatrix a -> Vector (Int,Int) a
subMatrixToVector (Zero s)            = zeros (s,s)
subMatrixToVector (NonZero x v)       = shiftH x $ toIdentity v
subMatrixToVector (SplitH top bot)    = (subMatrixToVector top) `above` (subMatrixToVector bot)
subMatrixToVector (SplitV left right) = (subMatrixToVector left) `beside` (subMatrixToVector right)

toIdentity :: (Bounds ix, Num a) => Vector ix a -> Vector (ix,ix) a
toIdentity v = vector (ix,ix) [ if x == y then v ! x else 0 | x <- ids, y <- ids]
    where ix = size v
          ids = indices v

fromIdentity :: (Bounds ix, Integral ix, Num a) => Vector (ix,ix) a -> ix -> Vector ix a
fromIdentity v s = vector ix [ v ! (x,(y+s) `mod` ix) | (x,y) <- indices v, x == y]
    where (ix,_) = size v

isIdentity :: (Bounds ix, Num a) => Vector (ix,ix) a -> Bool
isIdentity v = sum [v ! (x,y) | (x,y) <- indices v, x /= y] == 0 && (xmax == ymax)
    where (xmax,ymax) = size v

isZeros :: (Bounds ix, Num a) => Vector ix a -> Bool
isZeros v = sum (toList v) == 0

zeros :: (Bounds ix, Num a) => ix -> Vector ix a
zeros ix  = vector ix (repeat 0)

shiftH :: (Bounds x, Bounds y, Integral y) => 
          y -> Vector (x,y) a -> Vector (x,y) a
shiftH n v = ixmap (xmax,ymax) (\(a,b) -> (a,(b - n) `mod` ymax)) v
    where (xmax,ymax) = size v

vecEqualTo :: (Bounds ix, Eq a) => Vector (ix,ix) a -> Vector (ix,ix) a -> (Bool,[(ix,ix)])
vecEqualTo v1 v2 = (numElemsEq,contentsEq)
    where numElemsEq = x1 == x2 && y1 == y2
          (x1,y1) = size v1
          (x2,y2) = size v2
          contentsEq = [(x,y) | (x,y) <- indices v1, (v1 ! (x,y)) /= (v2 ! (x,y))]


main = do
  g <- readH
--  let var = vectorToSubMatrix g 32
  let g' = subMatrixToVector $ vectorToSubMatrix g 32
--  let gstr = unlines $ map unwords $ toList $ fmap toList $ rows $ fmap show g
-- let gstr'= unlines $ map unwords $ toList $ fmap toList $ rows $ fmap show g'
--   writeFile "gfst" gstr
--   writeFile "gsnd" gstr'
--   print var
  let (num,ids) = g `vecEqualTo` g'
  print (num,ids)

-- foo v = toList $ fmap toList $ rows $ fmap show v

readH :: IO (Vector (Int,Int) Int)
readH = do
	str <- readFile "Hmat.csv" 
	let wds = (map read (words str)) :: [Int]
	return $ vector (384,1408) wds