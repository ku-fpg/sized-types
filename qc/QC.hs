
-- Copy this module if you need Quick Check.
module QC where

import qualified Test.QuickCheck as QC
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Data.Sized.Arith
import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

instance Size n => QC.Arbitrary (X0_ n) where
	arbitrary = QC.elements [minBound .. maxBound]
	
instance Size n => QC.Arbitrary (X1_ n) where
	arbitrary = QC.elements [minBound .. maxBound]	

instance (QC.Arbitrary ix, Size ix, QC.Arbitrary a) => QC.Arbitrary (Matrix ix a) where
	arbitrary = f $ \ ixs -> do
          elems <- sequence [ QC.arbitrary | _ <- ixs ]
          return $ matrix elems
         where f :: (Size ix) => ([ix] -> m (Matrix ix a)) -> m (Matrix ix a)
               f fn = fn M.all
