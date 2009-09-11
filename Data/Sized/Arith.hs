{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls  #-}
module Data.Sized.Arith where

data N1

data X0 = X0
	deriving (Eq,Ord)

data X0_ a = X0_ Int
data X1_ a = X1_ Int


type family ADD a b
type instance ADD N1 N1 = APP0 N1
type instance ADD N1 X0 = N1
type instance ADD N1 (X0_ b) = APP1 (ADD N1 b)
type instance ADD N1 (X1_ b) = APP0 b
type instance ADD X0 N1 = N1					-- MIR
type instance ADD X0 X0 = X0
type instance ADD X0 (X0_ b) = X0_ b
type instance ADD X0 (X1_ b) = APP1 b
type instance ADD (X0_ a) N1 = APP1 (ADD a N1)			-- MIR
type instance ADD (X0_ a) X0 = APP0 a				-- MIR
type instance ADD (X0_ a) (X0_ b) = APP0 (ADD a b)
type instance ADD (X0_ a) (X1_ b) = APP1 (ADD a b)
type instance ADD (X1_ a) N1 = APP0 a				-- MIR
type instance ADD (X1_ a) X0 = APP1 a				-- MIR
type instance ADD (X1_ a) (X0_ b) = APP1 (ADD a b)		-- MIR
type instance ADD (X1_ a) (X1_ b) = APP0 (SUCC (ADD a b))

type family NOT a
type instance NOT N1 = X0
type instance NOT X0 = N1
type instance NOT (X0_ a) = APP1 (NOT a)  
type instance NOT (X1_ a) = APP0 (NOT a)


type SUB a b = ADD a (SUCC (NOT b))


type family SUCC a
type instance SUCC N1 = X0
type instance SUCC X0 = X1_ X0
type instance SUCC (X0_ a) = APP1 a
type instance SUCC (X1_ a) = APP0 (SUCC a)

type family APP1 a
type instance APP1 N1 = N1
type instance APP1 X0 = X1_ X0
type instance APP1 (X0_ a) = X1_ (X0_ a)
type instance APP1 (X1_ a) = X1_ (X1_ a)

type family APP0 a
type instance APP0 N1 = X0_ N1
type instance APP0 X0 = X0
type instance APP0 (X0_ a) = X0_ (X0_ a)
type instance APP0 (X1_ a) = X0_ (X1_ a)

