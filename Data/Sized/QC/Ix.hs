module Data.Sized.QC.Ix where

import qualified Test.QuickCheck as QC
import Data.Sized.Ix
import Data.Sized.Matrix 
import Data.Sized.Arith

instance Size n => QC.Arbitrary (X0_ n) where
	arbitrary = QC.elements [minBound .. maxBound]
	
instance Size n => QC.Arbitrary (X1_ n) where
	arbitrary = QC.elements [minBound .. maxBound]	
