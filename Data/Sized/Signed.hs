{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, FlexibleContexts, DataKinds #-}

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
	, toVector
	, fromVector
	,           S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
	, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19
	, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29
	, S30, S31, S32
	) where

import Data.Array.IArray(elems, (!))
import Data.Sized.Matrix as M
import Data.Sized.Fin
import Data.Bits
import GHC.TypeLits

type Signed (ix::Nat) = Signed' ix Integer

newtype Signed' (ix :: Nat) a = Signed a deriving (Eq, Ord)

-- specialise to Integer
{-# SPECIALISE toVector   :: (SingI ix) => Signed ix -> Vector ix Bool #-}
{-# SPECIALISE fromVector :: (SingI ix) => Vector ix Bool -> Signed ix #-}
{-# SPECIALISE mkSigned   :: (SingI ix) => Integer -> Signed ix #-}

-- specialise to Int
{-# SPECIALISE toVector   :: (SingI ix) => Signed' ix Int -> Vector ix Bool #-}
{-# SPECIALISE fromVector :: (SingI ix) => Vector ix Bool -> Signed' ix Int #-}
{-# SPECIALISE mkSigned   :: (SingI ix) => Int -> Signed' ix Int #-}

-- 'toVector' turns a sized 'Signed' value into a 'Vector' of 'Bool's.
toVector :: forall ix a. (SingI ix,Integral a) => Signed' ix a -> Vector ix Bool
toVector (Signed v) = matrix $ take (fromIntegral $ fromSing (sing :: Sing ix)) $ map odd $ iterate (`div` 2) v

-- 'fromVector' turns a 'Vector' of 'Bool's into a sized 'Signed' value.
fromVector :: (SingI ix,Integral a) => Vector ix Bool -> Signed' ix a
fromVector m = mkSigned $
	  sum [ n
	      | (n,b) <- zip (iterate (* 2) 1)
			      (elems m)
	      , b
	      ]
--
mkSigned :: forall ix a. (SingI ix,Integral a) => a -> Signed' ix a
mkSigned v = res
    where sz' = 2 ^ bitCount
          bitCount :: a
	  bitCount =  fromIntegral (fromNat (sing :: Sing ix) - 1)
	  res = case divMod v sz' of
	  	  (s,v') | even s    -> Signed v'
		         | otherwise -> Signed (v' - sz')

instance (SingI ix,Show a) => Show (Signed' ix a) where
        {-# SPECIALISE instance SingI ix => Show (Signed ix) #-}
        {-# SPECIALISE instance SingI ix => Show (Signed' ix Int) #-}
	show (Signed a) = show a

instance (SingI ix,Read a,Integral a) => Read (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Read (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Read (Signed' ix Int) #-}
	readsPrec i str = [ (mkSigned a,r) | (a,r) <- readsPrec i str ]

instance (SingI ix,Integral a) => Integral (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Integral (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Integral (Signed' ix Int) #-}
  	toInteger (Signed m) = toInteger m
	quotRem (Signed a) (Signed b) =
		case quotRem a b of
		   (q,r) -> (mkSigned q,mkSigned r)

instance (SingI ix,Num a,Integral a) => Num (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Num (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Num (Signed' ix Int) #-}
	(Signed a) + (Signed b) = mkSigned $ a + b
	(Signed a) - (Signed b) = mkSigned $ a - b
	(Signed a) * (Signed b) = mkSigned $ a * b
	abs (Signed n) = mkSigned $ abs n
	signum (Signed n) = mkSigned $ signum n
	fromInteger n = mkSigned (fromInteger n)

instance (SingI ix,Integral a) => Real (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Real (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Real (Signed' ix Int) #-}
	toRational (Signed n) = toRational n

instance (SingI ix,Integral a) => Enum (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Enum (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Enum (Signed' ix Int) #-}
	fromEnum (Signed n) = fromEnum n
	toEnum n = mkSigned (toEnum n)

instance (SingI ix,Eq a,Bits a,Integral a) => Bits (Signed' ix a) where
        {-# SPECIALISE instance (SingI ix) => Bits (Signed ix) #-}
        {-# SPECIALISE instance (SingI ix) => Bits (Signed' ix Int) #-}
	bitSize _ = fromIntegral (fromNat (sing :: Sing ix))

	complement (Signed v) = Signed (complement v)
	isSigned _ = True
	a `xor` b = fromVector (M.zipWith (/=) (toVector a) (toVector b))
	a .|. b = fromVector (M.zipWith (||) (toVector a) (toVector b))
	a .&. b = fromVector (M.zipWith (&&) (toVector a) (toVector b))
	shiftL (Signed v) i = mkSigned (v * (2 ^ i))
	shiftR (Signed v) i = mkSigned (v `div` (2 ^ i))
 	rotate v i = fromVector (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` mLeng)))
		where m = toVector v
                      mLeng = size $ M.zeroOf m
        testBit u idx = toVector u ! (fromIntegral idx)
        -- new is 7.6?
        bit   i  = fromVector (forAll $ \ ix -> if ix == fromIntegral i then True else False)
        popCount n = sum $ fmap (\ b -> if b then 1 else 0) $ elems $ toVector n


instance forall ix a. (SingI ix,Num a) => Bounded (Signed' ix a) where
        {-# SPECIALISE instance forall ix. (SingI ix) => Bounded (Signed ix) #-}
        {-# SPECIALISE instance forall ix. (SingI ix) => Bounded (Signed' ix Int) #-}
	minBound = Signed (- maxMagnitude)
            where maxMagnitude = 2 ^ (fromNat (sing :: Sing ix) - 1)
        maxBound = Signed (maxMagnitude - 1)
            where maxMagnitude = 2 ^ (fromNat (sing :: Sing ix) - 1)


type S2 = Signed' 2 Int
type S3 = Signed' 3 Int
type S4 = Signed' 4 Int
type S5 = Signed' 5 Int
type S6 = Signed' 6 Int
type S7 = Signed' 7 Int
type S8 = Signed' 8 Int
type S9 = Signed' 9 Int
type S10 = Signed' 10 Int
type S11 = Signed' 11 Int
type S12 = Signed' 12 Int
type S13 = Signed' 13 Int
type S14 = Signed' 14 Int
type S15 = Signed' 15 Int
type S16 = Signed' 16 Int
type S17 = Signed' 17 Int
type S18 = Signed' 18 Int
type S19 = Signed' 19 Int
type S20 = Signed' 20 Int
type S21 = Signed' 21 Int
type S22 = Signed' 22 Int
type S23 = Signed' 23 Int
type S24 = Signed' 24 Int
type S25 = Signed' 25 Int
type S26 = Signed' 26 Int
type S27 = Signed' 27 Int
type S28 = Signed' 28 Int
type S29 = Signed' 29 Int
type S30 = Signed' 30 Int
type S31 = Signed' 31 Int
type S32 = Signed' 32 Int
