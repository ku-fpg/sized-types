-- | Basic type-level arithmetic, using base two.
-- 
-- Copyright: (c) 2009 University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc


{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances  #-}
module Data.Sized.Arith where

import Data.Ix

data N1

data X0 = X0
	deriving (Eq,Ord)

data X0_ a = X0_ Int		-- times 2 plus 0
data X1_ a = X1_ Int		-- times 2 plus 1

type family ADD a b
type instance ADD N1 N1 = APP0 N1
--type instance ADD N1 X0 = N1
type instance ADD N1 (X0_ b) = APP1 (ADD N1 b)
type instance ADD N1 (X1_ b) = APP0 b
type instance ADD X0 N1 = N1					-- MIR
--type instance ADD X0 X0 = X0
--type instance ADD X0 (X0_ b) = X0_ b
--type instance ADD X0 (X1_ b) = APP1 b
type instance ADD (X0_ a) N1 = APP1 (ADD a N1)			-- MIR
--type instance ADD (X0_ a) X0 = APP0 a				-- MIR
type instance ADD (X0_ a) (X0_ b) = APP0 (ADD a b)
type instance ADD (X0_ a) (X1_ b) = APP1 (ADD a b)
type instance ADD (X1_ a) N1 = APP0 a				-- MIR
--type instance ADD (X1_ a) X0 = APP1 a				-- MIR
type instance ADD (X1_ a) (X0_ b) = APP1 (ADD a b)		-- MIR
type instance ADD (X1_ a) (X1_ b) = APP0 (SUCC (ADD a b))

type instance ADD a X0 = a
type instance ADD X0 a = a


type family NOT a
type instance NOT N1 = X0
type instance NOT X0 = N1
type instance NOT (X0_ a) = APP1 (NOT a)  
type instance NOT (X1_ a) = APP0 (NOT a)

type SUB a b = ADD a (SUCC (NOT b))

type family MUL a b
type instance MUL x X0      = X0
type instance MUL x (X0_ b) = ADD x (MUL x (ADD (X0_ b) N1))
type instance MUL x (X1_ b) = ADD x (MUL x (ADD (X1_ b) N1))
type instance MUL x N1      = SUB X0 x


type family SUCC a
type instance SUCC N1 = X0
type instance SUCC X0 = X1_ X0
type instance SUCC (X0_ a) = APP1 a
type instance SUCC (X1_ a) = APP0 (SUCC a)


type family LOG a
type instance LOG X0 = X0
type instance LOG (X0_ a) = ADD (X1_ X0) (LOG a)
type instance LOG (X1_ a) = ADD (X1_ X0) (LOG a)

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

