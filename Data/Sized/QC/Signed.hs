module Data.Sized.QC.Signed where
	
import Data.Sized.Signed
import Data.Sized.Ix
import Test.QuickCheck

newtype MyInt32 = MyInt32 (Signed X32)
	deriving (Eq,Ord, Num, Enum, Bit)

	

-- instance Arbitrary MyInt32 where
	