-- | Fin types
--
-- Copyright: (c) 2013 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, FlexibleInstances, GADTs, DeriveDataTypeable  #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
module Data.Sized.Fin
    ( -- TNat
      Fin
    , fromNat
    , corners
    , universe
    , size
    , module Data.Singletons
    , Nat
    )
    where

import Data.Ix
import Data.Typeable
import Data.Singletons
import Data.Singletons.TypeLits

--type TNat (a::Nat) = Sing a

newtype Fin (n :: Nat) = Fin Integer
    deriving (Eq, Ord, Typeable)

fromNat :: Sing (n :: Nat) -> Integer
fromNat = fromSing

-- A finite (bounding) corners of an finite indexed entity
corners :: forall i . (Bounded i) => (i,i)
corners = (minBound :: i,maxBound)

-- | A list of all possible values of a type.
universe :: (Bounded ix, Ix ix) => [ix]
universe = range corners

-- property:length (universe :: [a]) == size a
size :: forall ix . (Bounded ix, Ix ix) => ix -> Int
size _ = rangeSize (corners :: (ix,ix))

mkFin :: forall x . SingI x => Integer -> Fin x
mkFin n  | m == 0 = error "<<Fin 0>>"
         | n < 0  = error $ show n ++ " (:: Fin " ++ show m ++ ") is below upper bound"
         | n >= m = error $ show n ++ " (:: Fin " ++ show m ++ ") is above upper bound"
         | otherwise = Fin n
                where m = fromNat (sing :: Sing x)

instance Show (Fin a) where
   show (Fin a) = show a

instance SingI a => Read (Fin a) where
   readsPrec i str0 = [ (mkFin v,str1) | (v,str1) <- readsPrec i str0 ]

instance SingI a => Num (Fin a) where
   (Fin a) + (Fin b) = mkFin (a + b)
   (Fin a) * (Fin b) = mkFin (a * b)
   (Fin a) - (Fin b) = mkFin (a - b)
   abs (Fin a) = mkFin (abs a)
   signum (Fin a) = mkFin (signum a)
   fromInteger n = mkFin (fromInteger n)

instance (SingI a) => Ix (Fin a) where
  range   (Fin n, Fin m) = [ mkFin x | x <- range (n,m) ]
  index   (Fin n, Fin m) (Fin i) = index (n,m) i
  inRange (Fin n, Fin m) (Fin i) = inRange (n,m) i
  rangeSize (Fin n, Fin m) = fromIntegral $ max ((m - n) + 1) 0

instance SingI a => Bounded (Fin a) where
   minBound = mkFin 0
   maxBound = n where n = mkFin (fromSing (sing :: Sing a) - 1)

instance Enum (Fin a) where
   fromEnum (Fin n) = fromIntegral n
   toEnum n = Fin (fromIntegral n)

instance (SingI a) => Real (Fin a) where
   toRational (Fin n) = toRational n

instance (SingI a) => Integral (Fin a) where
   quot (Fin n) (Fin m) = mkFin (n `quot` m)
   rem (Fin n) (Fin m) = mkFin (n `rem` m)
   div (Fin n) (Fin m) = mkFin (n `div` m)
   mod (Fin n) (Fin m) = mkFin (n `mod` m)
   quotRem a b = (a `quot` b,a `rem` b)
   divMod a b = (a `div` b,a `mod` b)
   toInteger (Fin n) = n
