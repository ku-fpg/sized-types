{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DataKinds, KindSignatures, TypeOperators #-}
module Data.Sized.TypeNatInstances where

import GHC.TypeLits


-- ----------------------------------------------------------
--  Some common type instances that greatly facilitate
--  type reasoning about Sized types.

type instance (n * 1) = n
type instance (1 * n) = n
type instance (0 * n) = 0
type instance (n * 0) = 0


