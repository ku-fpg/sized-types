{-# LANGUAGE TemplateHaskell  #-}
-- Utils for Template Haskell generation
module Data.Sized.QC.TH where

import Language.Haskell.TH
import Test.QuickCheck

qcTH :: Integer -> Q [Dec]
qcTH maxVal = do
	decs <- sequence [ qcInstanceGen (fromIntegral n)				-- count
			| n <- [0..maxVal]
			]
	return $ concat decs

qcInstanceGen :: Integer -> Q [Dec]
qcInstanceGen v = do
     let ty   = mkName ("X" ++ show v)
     return 
       [InstanceD [] (AppT (ConT ''Arbitrary) (ConT ty))
                  [ ValD (VarP (mkName "arbitrary"))
		       (NormalB (AppE (VarE 'elements)
                                      (ArithSeqE (FromToR (VarE 'minBound) (VarE 'maxBound)))
                       )) []
                  ]
      ]
