
-- Copy this module if you need Quick Check.
module QC.QC where

import qualified Test.QuickCheck as QC
import Data.Ix

import Data.Sized.Fin
import Data.Sized.Matrix

import GHC.TypeLits

instance (SingI n) => QC.Arbitrary (Fin n) where
	arbitrary = QC.elements [minBound .. maxBound]

instance (QC.Arbitrary ix, Bounded ix, Ix ix, QC.Arbitrary a) => QC.Arbitrary (Matrix ix a) where
	arbitrary = f $ \ ixs -> do
          elems <- sequence [ QC.arbitrary | _ <- ixs ]
          return $ matrix elems
         where f :: (Bounded ix, Ix ix) => ([ix] -> m (Matrix ix a)) -> m (Matrix ix a)
               f fn = fn (allIndices (undefined :: Matrix ix a))
