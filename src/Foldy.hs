{-# LANGUAGE DataKinds, TemplateHaskell, TypeApplications #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Foldy where

import           Refined

type FiveToTenPred = And (SizeGreaterThan 4) (SizeLessThan 11)

type TenElems = Refined (SizeEqualTo 10) [Int]
type FiveToTenElems = Refined FiveToTenPred [Int]
type Asc = Refined Ascending [Int]

fixed :: TenElems
fixed = $$(refineTH [1..10])

range :: FiveToTenElems
range = $$(refineTH [1..5])

ordering :: Asc
ordering = $$(refineTH [1,2])

instance Weaken (SizeEqualTo 10) FiveToTenPred

weakening :: FiveToTenElems
weakening = weaken fixed
