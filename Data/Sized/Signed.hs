-- | Signed, fixed sized numbers.
-- 
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

module Data.Sized.Signed 
	( Signed
	, toMatrix
	, fromMatrix
	) where
	
import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.List as L
import Data.Bits

newtype Signed ix = Signed Integer 

-- 'toMatrix' turns a sized 'Signed' value into a 'Matrix' of 'Bool's. 
toMatrix :: (Size ix, Enum ix) => Signed ix -> Matrix ix Bool
toMatrix s@(Signed v) = matrix $ take (bitSize s) $ map odd $ iterate (`div` 2) v

-- 'toMatrix' turns a a 'Matrix' of 'Bool's into sized 'Signed' value. 
fromMatrix :: (Size ix, Enum ix) => Matrix ix Bool -> Signed ix
fromMatrix m = mkSigned $
	  sum [ n	
	      | (n,b) <- zip (iterate (* 2) 1)
			      (M.toList m)
	      , b
	      ]
-- 
mkSigned :: (Size ix, Enum ix) => Integer -> Signed ix
mkSigned v = res
   where sz' = 2 ^ (fromIntegral bitCount :: Integer)
	 bitCount = bitSize res - 1
	 res = case divMod v sz' of
	  	(s,v') | even s    -> Signed v' 
		       | otherwise -> Signed (v' - sz') 

instance (Size ix, Enum ix) => Eq (Signed ix) where
	(Signed a) == (Signed b) = a == b
instance (Size ix, Enum ix) => Ord (Signed ix) where
	(Signed a) `compare` (Signed b) = a `compare` b
instance (Size ix, Enum ix) => Show (Signed ix) where
	show (Signed a) = show a
instance (Size ix, Enum ix) => Integral (Signed ix) where
  	toInteger (Signed m) = m
	quotRem (Signed a) (Signed b) = 
		case quotRem a b of
		   (q,r) -> (mkSigned q,mkSigned r)
instance (Size ix, Enum ix) => Num (Signed ix) where
	(Signed a) + (Signed b) = mkSigned $ a + b
	(Signed a) - (Signed b) = mkSigned $ a - b
	(Signed a) * (Signed b) = mkSigned $ a * b
	abs (Signed n) = mkSigned $ abs n
	signum (Signed n) = mkSigned $ signum n
	fromInteger n = mkSigned n
instance (Size ix, Enum ix) => Real (Signed ix) where
	toRational (Signed n) = toRational n
instance (Size ix, Enum ix) => Enum (Signed ix) where
	fromEnum (Signed n) = fromEnum n
	toEnum n = mkSigned (toInteger n)	
instance (Size ix, Enum ix) => Bits (Signed ix) where
	bitSize s = f s undefined
	  where
		f :: (Size a) => Signed a -> a -> Int
		f _ ix = size ix
	complement = fromMatrix . fmap not . toMatrix
	isSigned _ = True
	a `xor` b = fromMatrix (M.zipWith (/=) (toMatrix a) (toMatrix b))
	a .|. b = fromMatrix (M.zipWith (||) (toMatrix a) (toMatrix b))
	a .&. b = fromMatrix (M.zipWith (&&) (toMatrix a) (toMatrix b))
	shiftL (Signed v) i = mkSigned (v * (2 ^ i))
	shiftR (Signed v) i = mkSigned (v `div` (2 ^ i))
 	rotate v i = fromMatrix (forAll $ \ ix -> m ! (toEnum ((fromEnum ix - i) `mod` M.length m)))
		where m = toMatrix v
		

	
