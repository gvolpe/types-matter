{-# LANGUAGE DataKinds, TemplateHaskell, TypeApplications #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Foldy where

import           Refined

type TenElems = Refined (SizeEqualTo 10) [Int]
type FiveToTenElems = Refined (And (SizeGreaterThan 4) (SizeLessThan 11)) [Int]
type Asc = Refined Ascending [Int]

fixed :: TenElems
fixed = $$(refineTH [1..10])

range :: FiveToTenElems
range = $$(refineTH [1..5])

ordering :: Asc
ordering = $$(refineTH [1,2])

instance Weaken (SizeEqualTo 10) (And (SizeGreaterThan 4) (SizeLessThan 11))

weakening :: FiveToTenElems
weakening = weaken fixed
