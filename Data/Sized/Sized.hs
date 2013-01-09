-- | Sized types X0 to X256.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, FlexibleInstances, GADTs  #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
module Data.Sized.Sized
    ( TNat
    , Sized
    , fromNat
    , corners
    , universe
    , size
     )
    where

import Data.Ix
import GHC.TypeLits

type TNat (a::Nat) = Sing a

newtype Sized (n :: Nat) = MkSized Integer
    deriving (Eq, Ord)

fromNat :: Sing (n :: Nat) -> Integer
fromNat = fromSing

-- A finite (bounding) corners of an finite indexed entity
corners :: forall i . (Bounded i, Ix i) => (i,i)
corners = (minBound :: i,maxBound)

-- | A list of all possible values of a type.
universe :: (Bounded ix, Ix ix) => [ix]
universe = range corners

-- property:length (universe :: [a]) == size a
size :: forall ix . (Bounded ix, Ix ix) => ix -> Int
size _ = rangeSize (corners :: (ix,ix))

mkSized :: forall x . SingI x => Integer -> Sized x
mkSized n | m == 0 = error "<<Sized 0>>"
          | n < 0  = error $ show n ++ " (:: Sized " ++ show m ++ ") is below upper bound"
          | n >= m = error $ show n ++ " (:: Sized " ++ show m ++ ") is above upper bound"
          | otherwise = MkSized n
                where m = fromSing (sing :: Sing x)

instance Show (Sized a) where
   show (MkSized a) = show a

instance SingI a => Read (Sized a) where
   readsPrec i str0 = [ (mkSized v,str1) | (v,str1) <- readsPrec i str0 ]

instance SingI a => Num (Sized a) where
   (MkSized a) + (MkSized b) = mkSized (a + b)
   (MkSized a) * (MkSized b) = mkSized (a * b)
   (MkSized a) - (MkSized b) = mkSized (a - b)
   abs (MkSized a) = mkSized (abs a)
   signum (MkSized a) = mkSized (signum a)
   fromInteger n = mkSized (fromInteger n)

instance (SingI a) => Ix (Sized a) where
  range   (MkSized n, MkSized m) = [ mkSized x | x <- range (n,m) ]
  index   (MkSized n, MkSized m) (MkSized i) = index (n,m) i
  inRange (MkSized n, MkSized m) (MkSized i) = inRange (n,m) i
  rangeSize (MkSized n, MkSized m) = fromIntegral $ max ((m - n) + 1) 0

instance SingI a => Bounded (Sized a) where
   minBound = mkSized 0
   maxBound = n where n = mkSized (fromSing (sing :: Sing a) - 1)

instance Enum (Sized a) where
   fromEnum (MkSized n) = fromIntegral n
   toEnum n = MkSized (fromIntegral n)

instance (SingI a) => Real (Sized a) where
   toRational (MkSized n) = toRational n

instance (SingI a) => Integral (Sized a) where
   quot (MkSized n) (MkSized m) = mkSized (n `quot` m)
   rem (MkSized n) (MkSized m) = mkSized (n `rem` m)
   div (MkSized n) (MkSized m) = mkSized (n `div` m)
   mod (MkSized n) (MkSized m) = mkSized (n `mod` m)
   quotRem a b = (a `quot` b,a `rem` b)
   divMod a b = (a `div` b,a `mod` b)
   toInteger (MkSized n) = n
