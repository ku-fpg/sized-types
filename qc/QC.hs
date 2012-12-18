
-- Copy this module if you need Quick Check.
module QC where

import qualified Test.QuickCheck as QC
import Data.Ix

import Data.Sized.Sized
import Data.Sized.Matrix as M

import GHC.TypeLits

instance (SingI n) => QC.Arbitrary (Sized n) where
	arbitrary = QC.elements [minBound .. maxBound]

instance (QC.Arbitrary ix, Bounded ix, Ix ix, QC.Arbitrary a) => QC.Arbitrary (Matrix ix a) where
	arbitrary = f $ \ ixs -> do
          elems <- sequence [ QC.arbitrary | _ <- ixs ]
          return $ matrix elems
         where f :: (Bounded ix, Ix ix) => ([ix] -> m (Matrix ix a)) -> m (Matrix ix a)
               f fn = fn (M.allIndices (undefined :: Matrix ix a))
