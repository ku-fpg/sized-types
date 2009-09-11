module Data.Sized.QC.Matrix where
	
import qualified Test.QuickCheck as QC
import Data.Sized.Ix
import Data.Sized.Matrix 

instance (QC.Arbitrary ix, Size ix, QC.Arbitrary a) => QC.Arbitrary (Matrix ix a) where
	arbitrary = f $ \ ixs -> do
          elems <- sequence [ QC.arbitrary | _ <- ixs ]
          return $ matrix elems
         where f :: (Size ix) => ([ix] -> m (Matrix ix a)) -> m (Matrix ix a)
               f fn = fn indices_
