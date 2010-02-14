{-# LANGUAGE ScopedTypeVariables #-}

-- | Unsigned, fixed sized numbers.
-- 
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

module Data.Sized.Unsigned 
	( Unsigned
	, toMatrix
	, fromMatrix
	,      U1,  U2,  U3,  U4,  U5,  U6,  U7,  U8,  U9
	, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19
	, U20, U21, U22, U23, U24, U25, U26, U27, U28, U29
	, U30, U31, U32
	) where
	
import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.List as L
import Data.Bits

newtype Unsigned ix = Unsigned Integer 

toMatrix :: forall ix . (Size ix) => Unsigned ix -> Matrix ix Bool
toMatrix s@(Unsigned v) = matrix $ take (size (error "toMatrix" :: ix)) $ map odd $ iterate (`div` 2) v

fromMatrix :: (Size ix) => Matrix ix Bool -> Unsigned ix
fromMatrix m = mkUnsigned $
	  sum [ n	
	      | (n,b) <- zip (iterate (* 2) 1)
			      (M.toList m)
	      , b
	      ]

mkUnsigned :: forall ix . (Size ix) => Integer -> Unsigned ix
mkUnsigned v = res
   where sz' = 2 ^ (fromIntegral bitCount :: Integer)
	 bitCount = size (error "mkUnsigned" :: ix)
	 res = Unsigned (v `mod` sz')

instance (Size ix) => Eq (Unsigned ix) where
	(Unsigned a) == (Unsigned b) = a == b
instance (Size ix) => Ord (Unsigned ix) where
	(Unsigned a) `compare` (Unsigned b) = a `compare` b
instance (Size ix) => Show (Unsigned ix) where
	show (Unsigned a) = show a
instance (Size ix) => Read (Unsigned ix) where
	readsPrec i str = [ (mkUnsigned a,r) | (a,r) <- readsPrec i str ]
instance (Size ix) => Integral (Unsigned ix) where
  	toInteger (Unsigned m) = m
	quotRem (Unsigned a) (Unsigned b) = 
		case quotRem a b of
		   (q,r) -> (mkUnsigned q,mkUnsigned r)
instance (Size ix) => Num (Unsigned ix) where
	(Unsigned a) + (Unsigned b) = mkUnsigned $ a + b
	(Unsigned a) - (Unsigned b) = mkUnsigned $ a - b
	(Unsigned a) * (Unsigned b) = mkUnsigned $ a * b
	abs (Unsigned n) = mkUnsigned $ abs n
	signum (Unsigned n) = mkUnsigned $ signum n
	fromInteger n = mkUnsigned n
instance (Size ix) => Real (Unsigned ix) where
	toRational (Unsigned n) = toRational n
instance (Size ix) => Enum (Unsigned ix) where
	fromEnum (Unsigned n) = fromEnum n
	toEnum n = mkUnsigned (toInteger n)	
instance (Size ix, Integral ix) => Bits (Unsigned ix) where
	bitSize s = f s undefined
	  where
		f :: (Size a) => Unsigned a -> a -> Int
		f _ ix = size ix
	complement = fromMatrix . fmap not . toMatrix
	isSigned _ = False
	a `xor` b = fromMatrix (M.zipWith (/=) (toMatrix a) (toMatrix b))
	a .|. b = fromMatrix (M.zipWith (||) (toMatrix a) (toMatrix b))
	a .&. b = fromMatrix (M.zipWith (&&) (toMatrix a) (toMatrix b))
	shiftL (Unsigned v) i = mkUnsigned (v * (2 ^ i))
	shiftR (Unsigned v) i = mkUnsigned (v `div` (2 ^ i))
	-- it might be possible to loosen the Integral requirement
 	rotate v i = fromMatrix (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` M.length m)))
		where m = toMatrix v
        testBit u idx = toMatrix u ! (fromIntegral idx)

instance forall ix . (Size ix) => Bounded (Unsigned ix) where
	minBound = Unsigned 0
        maxBound = Unsigned (2 ^ (size (error "Bounded/Unsigned" :: ix)) - 1)

-- | common; numerically boolean.
type U1 = Unsigned X1

type U2 = Unsigned X2
type U3 = Unsigned X3
type U4 = Unsigned X4
type U5 = Unsigned X5
type U6 = Unsigned X6
type U7 = Unsigned X7
type U8 = Unsigned X8
type U9 = Unsigned X9
type U10 = Unsigned X10
type U11 = Unsigned X11
type U12 = Unsigned X12
type U13 = Unsigned X13
type U14 = Unsigned X14
type U15 = Unsigned X15
type U16 = Unsigned X16
type U17 = Unsigned X17
type U18 = Unsigned X18
type U19 = Unsigned X19
type U20 = Unsigned X20
type U21 = Unsigned X21
type U22 = Unsigned X22
type U23 = Unsigned X23
type U24 = Unsigned X24
type U25 = Unsigned X25
type U26 = Unsigned X26
type U27 = Unsigned X27
type U28 = Unsigned X28
type U29 = Unsigned X29
type U30 = Unsigned X30
type U31 = Unsigned X31
type U32 = Unsigned X32
