{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeOperators #-}

module Control.Parallel.Class where

import           Control.Applicative            ( ZipList(..) )
import           Control.Natural                ( (:~>)(..) )
import           Data.Validation                ( Validation
                                                , toEither
                                                , fromEither
                                                )

{-
 - The `Parallel` class abstracts over monads which support
 -  parallel composition via some related `Applicative`.
 -}
class (Monad m, Applicative f) => Parallel f m | m -> f, f -> m where
  parallel :: m :~> f
  sequential :: f :~> m

instance Semigroup e => Parallel (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

instance Parallel ZipList [] where
  parallel   = NT ZipList
  sequential = NT getZipList
