{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, FlexibleContexts, DataKinds, DeriveDataTypeable, CPP #-}

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
    , toVector
    , fromVector
    , showBits
    ,      U1,  U2,  U3,  U4,  U5,  U6,  U7,  U8,  U9
    , U10, U11, U12, U13, U14, U15, U16, U17, U18, U19
    , U20, U21, U22, U23, U24, U25, U26, U27, U28, U29
    , U30, U31, U32
    ) where

import Data.Array.IArray(elems, (!))
import Data.Sized.Matrix as M
import Data.Sized.Fin
import Data.Bits
import Data.Ix
import Data.Typeable

newtype Unsigned (ix :: Nat) = Unsigned Integer
    deriving ( Eq
             , Ord
#if __GLASGOW_HASKELL__ >= 708
             , Typeable
#endif
             )

-- 'toVector' turns a sized 'Unsigned' value into a 'Vector' of 'Bool's.
toVector :: forall ix . (SingI ix) => Unsigned ix -> Vector ix Bool
toVector (Unsigned v) = matrix $ take (fromIntegral $ fromSing (sing :: Sing ix)) $ map odd $ iterate (`div` 2) v

-- 'fromVector' turns a 'Vector' of 'Bool's into sized 'Unsigned' value.
fromVector :: (SingI ix) => Vector ix Bool -> Unsigned ix
fromVector m = mkUnsigned $
    sum [ n
        | (n,b) <- zip (iterate (* 2) 1)
                       (elems m)
        , b
        ]

mkUnsigned :: forall ix . (SingI ix) => Integer -> Unsigned ix
mkUnsigned x = Unsigned (x `mod` (2 ^ bitCount))
    where bitCount = fromNat (sing :: Sing ix)

instance Show (Unsigned ix) where
    show (Unsigned a) = show a

instance (SingI ix) => Read (Unsigned ix) where
    readsPrec i str = [ (mkUnsigned a,r) | (a,r) <- readsPrec i str ]

instance (SingI ix) => Integral (Unsigned ix) where
    toInteger (Unsigned m) = m
    quotRem (Unsigned a) (Unsigned b) =
        case quotRem a b of
             (q,r) -> (mkUnsigned q,mkUnsigned r) -- TODO: check for size

instance (SingI ix) => Num (Unsigned ix) where
    (Unsigned a) + (Unsigned b) = mkUnsigned $ a + b
    (Unsigned a) - (Unsigned b) = mkUnsigned $ a - b
    (Unsigned a) * (Unsigned b) = mkUnsigned $ a * b
    abs (Unsigned n) = mkUnsigned $ abs n
    signum (Unsigned n) = mkUnsigned $ signum n
    fromInteger n = mkUnsigned n

instance (SingI ix) => Real (Unsigned ix) where
    toRational (Unsigned n) = toRational n

instance (SingI ix) => Enum (Unsigned ix) where
    fromEnum (Unsigned n) = fromEnum n
    toEnum n = mkUnsigned (toInteger n)

instance (SingI ix) => Bits (Unsigned ix) where
    bitSizeMaybe = return . finiteBitSize
    bitSize = finiteBitSize
    complement (Unsigned v) = Unsigned (complement v)
    isSigned _ = False
    (Unsigned a) `xor` (Unsigned b) = Unsigned (a `xor` b)
    (Unsigned a) .|. (Unsigned b) = Unsigned (a .|. b)
    (Unsigned a) .&. (Unsigned b) = Unsigned (a .&. b)
    shiftL (Unsigned v) i = mkUnsigned (shiftL v i)
    shiftR (Unsigned v) i = mkUnsigned (shiftR v i)

-- TODO: fix
    -- it might be possible to loosen the Integral requirement
-- rotate (Ui i = fromVector (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` M.population m)))
--   where m = toVector v

    rotate v i = fromVector (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` mLeng)))
      where m = toVector v
            mLeng = size $ M.zeroOf m

    testBit (Unsigned u) idx = testBit u idx
    bit   i  = fromVector (forAll $ \ ix -> if ix == fromIntegral i then True else False)
    popCount n = sum $ fmap (\ b -> if b then 1 else 0) $ elems $ toVector n

instance (SingI ix) => FiniteBits (Unsigned ix) where
    finiteBitSize _ = fromIntegral (fromNat (sing :: Sing ix))

showBits :: (SingI ix) => Unsigned ix -> String
showBits u = "0b" ++ reverse
                 [ if testBit u i then '1' else '0'
                 | i <- [0..(finiteBitSize u - 1)]
                 ]

instance (SingI ix) => Bounded (Unsigned ix) where
    minBound = Unsigned 0
    maxBound = Unsigned (2 ^ (fromNat (sing :: Sing ix)) - 1)

-- We do not address efficiency in this implementation.
instance (SingI ix) => Ix (Unsigned ix) where
    range     (l, u)    = [l .. u]
    inRange   (l, u) v  =  (l <= v) && (v <= u)
    index     (l, u) v | inRange (l,u) v = fromIntegral (v - l)
                       | otherwise       = error "Error in Ix array index"
    rangeSize (l, u)   | l <= u           = fromIntegral $ (toInteger u) - (toInteger l) + 1
                       | otherwise       = 0


-- | common; numerically boolean.
type U1 = Unsigned 1

type U2 = Unsigned 2
type U3 = Unsigned 3
type U4 = Unsigned 4
type U5 = Unsigned 5
type U6 = Unsigned 6
type U7 = Unsigned 7
type U8 = Unsigned 8
type U9 = Unsigned 9
type U10 = Unsigned 10
type U11 = Unsigned 11
type U12 = Unsigned 12
type U13 = Unsigned 13
type U14 = Unsigned 14
type U15 = Unsigned 15
type U16 = Unsigned 16
type U17 = Unsigned 17
type U18 = Unsigned 18
type U19 = Unsigned 19
type U20 = Unsigned 20
type U21 = Unsigned 21
type U22 = Unsigned 22
type U23 = Unsigned 23
type U24 = Unsigned 24
type U25 = Unsigned 25
type U26 = Unsigned 26
type U27 = Unsigned 27
type U28 = Unsigned 28
type U29 = Unsigned 29
type U30 = Unsigned 30
type U31 = Unsigned 31
type U32 = Unsigned 32
