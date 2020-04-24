{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Refined.Instances where

import           Control.Monad                  ( when )
import           Data.Text                      ( Text
                                                , null
                                                , unpack
                                                )
import           Data.Typeable                  ( typeOf )
import           Prelude                 hiding ( null )
import           Refined

instance Predicate NonEmpty Text where
  --validate p value = validate p (unpack value) -- validate using String instance
  validate p value = when (null value)
    $ throwRefineOtherException (typeOf p) "Text cannot be empty"
