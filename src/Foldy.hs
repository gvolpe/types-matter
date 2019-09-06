{-# LANGUAGE DataKinds, TemplateHaskell, TypeApplications #-}

module Foldy where

import           Refined

type TenElems = Refined (SizeEqualTo 10) [Int]
type FiveToTenElems = Refined (And (SizeGreaterThan 4) (SizeLessThan 11)) [Int]

fixed :: TenElems
fixed = $$(refineTH [1..10])

range :: FiveToTenElems
range = $$(refineTH [1..5])
