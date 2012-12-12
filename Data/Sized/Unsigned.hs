{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, FlexibleContexts, DataKinds #-}

-- | Unsigned, fixed sized numbers.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

module Data.Sized.Unsigned where
{-
	( Unsigned
	, toMatrix
	, fromMatrix
	,      U1,  U2,  U3,  U4,  U5,  U6,  U7,  U8,  U9
	, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19
	, U20, U21, U22, U23, U24, U25, U26, U27, U28, U29
	, U30, U31, U32
	) where
-}

import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.Bits
import Data.Ix
import Data.Array.IArray as I
import GHC.TypeLits
import Data.Typeable

newtype Unsigned (ix :: Nat) = Unsigned Integer

{-
toMatrix :: forall ix . (Size ix) => Unsigned ix -> Matrix ix Bool
toMatrix (Unsigned v) = matrix $ take (size (error "toMatrix" :: ix)) $ map odd $ iterate (`div` 2) v

fromMatrix :: (Size ix) => Matrix ix Bool -> Unsigned ix
fromMatrix m = mkUnsigned $
	  sum [ n
	      | (n,b) <- zip (iterate (* 2) 1)
			      (M.toList m)
	      , b
	      ]
-}

mkUnsigned :: forall ix . (SingI ix) => Integer -> Unsigned ix
mkUnsigned x = Unsigned (x `mod` (2 ^ fromNat (sing :: Sing ix)))

fromNat :: Sing (n :: Nat) -> Integer
fromNat = fromSing

instance Eq (Unsigned ix) where
	(Unsigned a) == (Unsigned b) = a == b

instance Ord (Unsigned ix) where
	(Unsigned a) `compare` (Unsigned b) = a `compare` b

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
        bitSize _ = fromIntegral (fromNat (sing :: Sing ix))

	complement (Unsigned v) = Unsigned (complement v)
	isSigned _ = False
	(Unsigned a) `xor` (Unsigned b) = Unsigned (a `xor` b)
	(Unsigned a) .|. (Unsigned b) = Unsigned (a .|. b)
	(Unsigned a) .&. (Unsigned b) = Unsigned (a .&. b)
	shiftL (Unsigned v) i = mkUnsigned (shiftL v i)
	shiftR (Unsigned v) i = mkUnsigned (shiftR v i)

-- TODO: fix
	-- it might be possible to loosen the Integral requirement
-- 	rotate (Ui i = fromMatrix (forAll $ \ ix -> m ! (fromIntegral ((fromIntegral ix - i) `mod` M.population m)))
--		where m = toMatrix v
        testBit (Unsigned u) idx = testBit u idx
        bit   i  = fromMatrix (forAll $ \ ix -> if ix == fromIntegral i then True else False)
        popCount n = sum $ fmap (\ b -> if b then 1 else 0) $ M.toList $ toMatrix n

--showBits :: Unsigned ix -> Vector ix Bool
showBits :: (SingI ix) => Unsigned ix -> String
showBits u = "0b" ++ reverse
                 [ if testBit u i then '1' else '0'
                 | i <- [0..(bitSize u - 1)]
                 ]

instance SingI nat => Typeable (Unsigned (nat :: Nat)) where
  typeOf _ = mkTyConApp (mkTyCon3 "sized-types" "Data.Sized.Unsigned" ("Unsigned#" ++ show (fromSing (sing :: Sing nat)))) []

{-
instance forall ix . (Size ix) => Bounded (Unsigned ix) where
	minBound = Unsigned 0
        maxBound = Unsigned (2 ^ (size (error "Bounded/Unsigned" :: ix)) - 1)

-- Unsigned ix as member of Size class.
-- We do not address efficiency in this implementation.

type instance Index (Unsigned ix)  = Int

instance forall ix . (Size ix) => Ix (Unsigned ix) where
    range     (l, u)    = [l .. u]
    inRange   (l, u) v  =  (l <= v) && (v <= u)
    index     (l, u) v | inRange (l,u) v = fromIntegral (v - l)
                       | otherwise       = error "Error in Ix array index"
    rangeSize (l, u)   | l <= u           = fromIntegral $ (toInteger u) - (toInteger l) + 1
                       | otherwise       = 0

instance forall ix . (Size ix) => Size (Unsigned ix) where
    size         = const s
	where s  = fromIntegral $ toInteger (maxBound :: Unsigned ix) + 1
    addIndex v n =  v + (fromIntegral n)  -- fix bounds issues
    toIndex v    = fromIntegral v

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
-}
