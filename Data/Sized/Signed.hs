{-# LANGUAGE ScopedTypeVariables #-}

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
	,           S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
	, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19
	, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29
	, S30, S31, S32
	) where

import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.List as L
import Data.Bits
import Data.Array.IArray as I

newtype Signed ix = Signed Integer

-- 'toMatrix' turns a sized 'Signed' value into a 'Matrix' of 'Bool's.
toMatrix :: forall ix . (Size ix) => Signed ix -> Matrix ix Bool
toMatrix s@(Signed v) = matrix $ take (size (error "toMatrix" :: ix)) $ map odd $ iterate (`div` 2) v

-- 'toMatrix' turns a a 'Matrix' of 'Bool's into sized 'Signed' value.
fromMatrix :: (Size ix) => Matrix ix Bool -> Signed ix
fromMatrix m = mkSigned $
	  sum [ n
	      | (n,b) <- zip (iterate (* 2) 1)
			      (M.toList m)
	      , b
	      ]
--
mkSigned :: forall ix . (Size ix) => Integer -> Signed ix
mkSigned v = res
   where sz' = 2 ^ (fromIntegral bitCount :: Integer)
	 bitCount = size (error "mkUnsigned" :: ix) - 1
	 res = case divMod v sz' of
	  	(s,v') | even s    -> Signed v'
		       | otherwise -> Signed (v' - sz')

instance (Size ix) => Eq (Signed ix) where
	(Signed a) == (Signed b) = a == b
instance (Size ix) => Ord (Signed ix) where
	(Signed a) `compare` (Signed b) = a `compare` b
instance (Size ix) => Show (Signed ix) where
	show (Signed a) = show a
instance (Enum ix, Size ix) => Read (Signed ix) where
	readsPrec i str = [ (mkSigned a,r) | (a,r) <- readsPrec i str ]
instance (Size ix) => Integral (Signed ix) where
  	toInteger (Signed m) = m
	quotRem (Signed a) (Signed b) =
		case quotRem a b of
		   (q,r) -> (mkSigned q,mkSigned r)
instance (Size ix) => Num (Signed ix) where
	(Signed a) + (Signed b) = mkSigned $ a + b
	(Signed a) - (Signed b) = mkSigned $ a - b
	(Signed a) * (Signed b) = mkSigned $ a * b
	abs (Signed n) = mkSigned $ abs n
	signum (Signed n) = mkSigned $ signum n
	fromInteger n = mkSigned n
instance (Size ix) => Real (Signed ix) where
	toRational (Signed n) = toRational n
instance (Size ix) => Enum (Signed ix) where
	fromEnum (Signed n) = fromEnum n
	toEnum n = mkSigned (toInteger n)
instance (Size ix, Integral ix) => Bits (Signed ix) where
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
 	rotate v i = fromMatrix (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` M.length m)))
		where m = toMatrix v
        testBit u idx = toMatrix u ! (fromIntegral idx)
        -- new is 7.6?
        bit   i  = fromMatrix (forAll $ \ ix -> if ix == fromIntegral i then True else False)
        popCount n = sum $ fmap (\ b -> if b then 1 else 0) $ M.toList $ toMatrix n


instance forall ix . (Size ix) => Bounded (Signed ix) where
	minBound = Signed (- maxMagnitude)
            where maxMagnitude = 2 ^ (size (error "Bounded/Signed" :: ix) -1)
        maxBound = Signed (maxMagnitude - 1)
            where maxMagnitude = 2 ^ (size (error "Bounded/Signed" :: ix) -1)


type S2 = Signed X2
type S3 = Signed X3
type S4 = Signed X4
type S5 = Signed X5
type S6 = Signed X6
type S7 = Signed X7
type S8 = Signed X8
type S9 = Signed X9
type S10 = Signed X10
type S11 = Signed X11
type S12 = Signed X12
type S13 = Signed X13
type S14 = Signed X14
type S15 = Signed X15
type S16 = Signed X16
type S17 = Signed X17
type S18 = Signed X18
type S19 = Signed X19
type S20 = Signed X20
type S21 = Signed X21
type S22 = Signed X22
type S23 = Signed X23
type S24 = Signed X24
type S25 = Signed X25
type S26 = Signed X26
type S27 = Signed X27
type S28 = Signed X28
type S29 = Signed X29
type S30 = Signed X30
type S31 = Signed X31
type S32 = Signed X32
