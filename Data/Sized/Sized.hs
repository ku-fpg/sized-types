-- | Sized types X0 to X256.
--
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc

{-
Status of Branch:  NatsNotReady

This branch attempted to replace the data Types X0, (X0_ a) and (X1_ a)
with the recent addtion to GHC, TypeNats   (Type Level Literals)
http://hackage.haskell.org/trac/ghc/wiki/TypeNats

Although this branch does successfully build, and "appears" to
be operational (except for matrix above, beside functions).

At the current time (GHC 7.6 - Dec. 2012),  TypeNats do not fulfill the
needs of this SizedTypes module, or its usage in KansasLava.

1.  Type level computation with TypeNats is limited:
(<=) :: Nat -> Nat -> Prop    -- Comparison
(+)  :: Nat -> Nat -> Nat     -- Addition
(*)  :: Nat -> Nat -> Nat     -- Multiplication
(^)  :: Nat -> Nat -> Nat     -- Exponentiation
        We really need at least Subtraction and Log2 as well.
Also, these computations are strictly limited in application because,
for example,  (3 + 7) =/= (7 + 3)

    Couldn't match type `3 GHC.TypeLits.+ 7' with `7 GHC.TypeLits.+ 3'
    NB: `GHC.TypeLits.+' is a type function, and may not be injective
    Expected type: 7 GHC.TypeLits.+ 3
      Actual type: 3 GHC.TypeLits.+ 7
    In the first argument of `(==)', namely `(m1 `above` m2)'

2.  Template Haskell has not yet been updated to support TypeNats
and KansasLava uses TH in the defintion of number Rep instances.

3.  Distinguishing between kinds and types with the same name,
and stablishing constraints in KansasLava was difficult.
Not able to push this, however, since #2 stopped development
when defining instances of Rep.

These issues are likely to be resolved in future releases of GHC,
so it may be worthwhile to re-visit this branch, then.


-}

{-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, FlexibleInstances, GADTs  #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Data.Sized.Sized where

import Data.Ix
import GHC.TypeLits

data Sized :: Nat -> * where
        Sized :: Integer -> Sized (a :: Nat)
     deriving (Eq, Ord)

{-
class    (Bounded i, Ix i) => SizedIx i where {}
instance (Bounded i, Ix i) => SizedIx i where {}
-}

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

instance SingI a => Num (Sized a) where
   (Sized a) + (Sized b) = mkSized (a + b)
   (Sized a) * (Sized b) = mkSized (a * b)
   (Sized a) - (Sized b) = mkSized (a - b)
   abs (Sized a) = mkSized (abs a)
   signum (Sized a) = mkSized (signum a)
   fromInteger n = mkSized (fromInteger n)

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

instance (SingI a) => Real (Sized a) where
   toRational (Sized n) = toRational n

instance (SingI a) => Integral (Sized a) where
   quot (Sized n) (Sized m) = mkSized (n `quot` m)
   rem (Sized n) (Sized m) = mkSized (n `rem` m)
   div (Sized n) (Sized m) = mkSized (n `div` m)
   mod (Sized n) (Sized m) = mkSized (n `mod` m)
   quotRem a b = (a `quot` b,a `rem` b)
   divMod a b = (a `div` b,a `mod` b)
   toInteger (Sized n) = n

