-- | Sized types X0 to X256.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-# LANGUAGE DataKinds, TypeFamilies, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables, GADTs  #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, FlexibleInstances, OverlappingInstances #-}
module Data.Sized.Sized where

import Data.Ix
import GHC.TypeLits

data Sized :: Nat -> * where
   Sized :: Integer -> Sized (a :: Nat)

class (Bounded i, Ix i) => SizedIx i
instance (Bounded i, Ix i) => SizedIx i

-- | A list of all possible values of a type.
universe :: (SizedIx ix) => [ix]
universe = range (minBound,maxBound)

size :: forall ix . (SizedIx ix) => ix -> Int
size _ = rangeSize (minBound,maxBound :: ix)

--size :: forall n  . SingI n => Sized n -> Integer
--size _ =

mkSized :: forall n . SingI n => Integer -> Sized n
mkSized n | m == 0 = error "<<Sized 0>>"
          | n < 0  = error $ show n ++ " (:: Sized " ++ show m ++ ") is below upper bound"
          | n >= m = error $ show n ++ " (:: Sized " ++ show m ++ ") is above upper bound"
          | otherwise = Sized n
                where m = fromSing (sing :: Sing n)

instance Show (Sized a) where
   show (Sized a) = show a

instance SingI a => Read (Sized a) where
   readsPrec i str0 = [ (mkSized v,str1) | (v,str1) <- readsPrec i str0 ]

instance (SingI a) => Num (Sized a) where
   (Sized a) + (Sized b) = mkSized (a + b)
   (Sized a) * (Sized b) = mkSized (a * b)
   (Sized a) - (Sized b) = mkSized (a - b)
   abs (Sized a) = mkSized (abs a)
   signum (Sized a) = mkSized (signum a)
   fromInteger n = mkSized (fromInteger n)

instance Eq (Sized a) where
  (Sized a) == (Sized b) = a == b

instance Ord (Sized a) where
  (Sized a) `compare` (Sized b) = a `compare` b

{-
-- This allows zero-length arrays, later.
instance Ix (Sized 0) where
  range   (_,_) = [ ]
  index   (_,_) _ = error "index using Sized 0"
  inRange (_,_) _ = False
-}

instance (SingI a) => Ix (Sized a) where
  range   (Sized n,Sized m) = [ mkSized x | x <- range (n,m) ]
  index   (Sized n,Sized m) (Sized i) = index (n,m) i
  inRange (Sized n,Sized m) (Sized i) = inRange (n,m) i
  rangeSize (Sized n,Sized m) = fromIntegral $ max ((m - n) + 1) 0

instance SingI a => Bounded (Sized a) where
   minBound = mkSized 0
   maxBound = n where n = mkSized (fromSing (sing :: Sing a) - 1)

instance Enum (Sized a) where
   fromEnum (Sized n) = fromIntegral n
   toEnum n = Sized (fromIntegral n)

