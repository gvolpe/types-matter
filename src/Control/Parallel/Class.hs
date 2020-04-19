{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeOperators #-}

module Control.Parallel.Class where

import           Control.Natural                ( (:~>)(..) )
import           Data.Validation

-- | The `Parallel` class abstracts over monads which support
-- | parallel composition via some related `Applicative`.
class (Monad m, Applicative f) => Parallel f m | m -> f, f -> m where
  parallel :: m :~> f
  sequential :: f :~> m

instance Semigroup e => Parallel (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither
