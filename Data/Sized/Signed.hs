{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, FlexibleContexts, DataKinds, DeriveDataTypeable, CPP #-}

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
#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

newtype Signed (ix :: Nat) = Signed Integer
    deriving ( Eq
             , Ord
#if __GLASGOW_HASKELL__ >= 708
             , Typeable
#endif
             )

-- 'toVector' turns a sized 'Signed' value into a 'Vector' of 'Bool's.
toVector :: forall ix . (SingI ix) => Signed ix -> Vector ix Bool
toVector (Signed v) = matrix $ take (fromIntegral $ fromSing (sing :: Sing ix)) $ map odd $ iterate (`div` 2) v

-- 'fromVector' turns a 'Vector' of 'Bool's into a sized 'Signed' value.
fromVector :: (SingI ix) => Vector ix Bool -> Signed ix
fromVector m = mkSigned $
    sum [ n
        | (n,b) <- zip (iterate (* 2) 1)
                       (elems m)
        , b
        ]
--
mkSigned :: forall ix . (SingI ix) => Integer -> Signed ix
mkSigned v = res
    where sz' = 2 ^ bitCount
          bitCount :: Integer
          bitCount =  fromIntegral (fromNat (sing :: Sing ix) - 1)
          res = case divMod v sz' of
                  (s,v') | even s    -> Signed v'
                         | otherwise -> Signed (v' - sz')

instance (SingI ix) => Show (Signed ix) where
    show (Signed a) = show a

instance (SingI ix) => Read (Signed ix) where
    readsPrec i str = [ (mkSigned a,r) | (a,r) <- readsPrec i str ]

instance (SingI ix) => Integral (Signed ix) where
    toInteger (Signed m) = m
    quotRem (Signed a) (Signed b) =
        case quotRem a b of
             (q,r) -> (mkSigned q,mkSigned r)

instance (SingI ix) => Num (Signed ix) where
    (Signed a) + (Signed b) = mkSigned $ a + b
    (Signed a) - (Signed b) = mkSigned $ a - b
    (Signed a) * (Signed b) = mkSigned $ a * b
    abs (Signed n) = mkSigned $ abs n
    signum (Signed n) = mkSigned $ signum n
    fromInteger n = mkSigned n

instance (SingI ix) => Real (Signed ix) where
    toRational (Signed n) = toRational n

instance (SingI ix) => Enum (Signed ix) where
    fromEnum (Signed n) = fromEnum n
    toEnum n = mkSigned (toInteger n)

instance (SingI ix) => Bits (Signed ix) where
#if MIN_VERSION_base(4,7,0)
    bitSizeMaybe = return . finiteBitSize
#endif
    bitSize = finiteBitSize
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

#if MIN_VERSION_base(4,7,0)
instance (SingI ix) => FiniteBits (Signed ix) where
    finiteBitSize _ = fromIntegral (fromNat (sing :: Sing ix))
#else
finiteBitSize :: SingI ix => Signed ix -> Int
finiteBitSize _ = fromIntegral (fromNat (sing :: Sing ix))
#endif

instance forall ix . (SingI ix) => Bounded (Signed ix) where
    minBound = Signed (- maxMagnitude)
      where maxMagnitude = 2 ^ (fromNat (sing :: Sing ix) - 1)
    maxBound = Signed (maxMagnitude - 1)
      where maxMagnitude = 2 ^ (fromNat (sing :: Sing ix) - 1)


type S2 = Signed 2
type S3 = Signed 3
type S4 = Signed 4
type S5 = Signed 5
type S6 = Signed 6
type S7 = Signed 7
type S8 = Signed 8
type S9 = Signed 9
type S10 = Signed 10
type S11 = Signed 11
type S12 = Signed 12
type S13 = Signed 13
type S14 = Signed 14
type S15 = Signed 15
type S16 = Signed 16
type S17 = Signed 17
type S18 = Signed 18
type S19 = Signed 19
type S20 = Signed 20
type S21 = Signed 21
type S22 = Signed 22
type S23 = Signed 23
type S24 = Signed 24
type S25 = Signed 25
type S26 = Signed 26
type S27 = Signed 27
type S28 = Signed 28
type S29 = Signed 29
type S30 = Signed 30
type S31 = Signed 31
type S32 = Signed 32
