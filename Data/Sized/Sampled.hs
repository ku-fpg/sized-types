{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, FlexibleContexts, DataKinds, ExistentialQuantification #-}
module Data.Sized.Sampled where

import Data.Sized.Signed as S
import Data.Sized.Matrix as M
import Data.Sized.Sized

import GHC.TypeLits

-- A signed fixed precision number, with max value m, via n sampled bits.

-- We add an extra bit, to represent the *sign*.
data Sampled (m :: Nat) (n :: Nat) = Sampled (Signed n) Rational
--	deriving Show

toMatrix :: (SingI m, SingI n) => Sampled m n -> Matrix (Sized n) Bool
toMatrix (Sampled sig _) = S.toMatrix sig

fromMatrix :: forall n m1 . (SingI n, SingI m1) => Matrix (Sized n) Bool -> Sampled m1 n
fromMatrix m = mkSampled (fromIntegral scale * fromIntegral val / fromIntegral precision)
   where val :: Signed n
	 val = S.fromMatrix m
	 scale     :: Integer
 	 scale     = fromIntegral (fromNat (sing :: Sing m1))
 	 precision :: Integer
 	 precision = 2 ^ (fromIntegral (fromNat (sing :: Sing n) - 1) :: Integer)

mkSampled :: forall n m . (SingI n, SingI m) => Rational -> Sampled m n
mkSampled v = Sampled val (fromIntegral scale * fromIntegral val / fromIntegral precision)
   where scale     :: Integer
	 scale     = fromIntegral (fromNat (sing :: Sing m))
	 precision :: Integer
	 precision = 2 ^ (fromIntegral (fromNat (sing :: Sing n) - 1) :: Integer)
	 val0      :: Rational
	 val0      = v / fromIntegral scale
	 val1 	   :: Integer
		     -- Key rounding step
	 val1      = round (val0 * fromIntegral precision)
	 val       = if val1 >= precision then maxBound
		     else if val1 <= -precision then minBound
		     else fromInteger val1

instance (SingI ix) => Eq (Sampled m ix) where
	(Sampled a _) == (Sampled b _) = a == b
instance (SingI ix) => Ord (Sampled m ix) where
	(Sampled a _) `compare` (Sampled b _) = a `compare` b
instance (SingI ix) => Show (Sampled m ix) where
	show (Sampled _ s) = show (fromRational s :: Double)
instance (SingI ix, SingI m) => Read (Sampled m ix) where
	readsPrec i str = [ (mkSampled a,r) | (a,r) <- readsPrec i str ]

instance (SingI ix, SingI m) => Num (Sampled m ix) where
	(Sampled _ a) + (Sampled _ b) = mkSampled $ a + b
	(Sampled _ a) - (Sampled _ b) = mkSampled $ a - b
	(Sampled _ a) * (Sampled _ b) = mkSampled $ a * b
	abs (Sampled _ n) = mkSampled $ abs n
	signum (Sampled _ n) = mkSampled $ signum n
	fromInteger n = mkSampled (fromInteger n)

instance (SingI ix, SingI m) => Real (Sampled m ix) where
	toRational (Sampled _ n) = toRational n

instance (SingI ix, SingI m) => Fractional (Sampled m ix) where
	fromRational n      = mkSampled n
	recip (Sampled _ n) = mkSampled $ recip n

-- This is a bit of a hack, and may generate -ve values from fromEnum.
instance (SingI ix, SingI m) => Enum (Sampled m ix) where
	fromEnum (Sampled n _) = fromEnum n

	toEnum n = mkSampled (fromIntegral scale * fromIntegral val / fromIntegral precision)
	   where val :: Signed ix
		 val = fromIntegral n
   		 scale     :: Integer
	 	 scale     = fromIntegral (fromNat (sing :: Sing m))
	 	 precision :: Integer
	 	 precision = 2 ^ (fromIntegral (fromNat (sing :: Sing ix) - 1) :: Integer)
