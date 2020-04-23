module Control.Parallel where

import           Control.Natural                ( unwrapNT )
import           Control.Parallel.Class
import           Data.Functor                   ( void )

{-
 - TODO: Abstract over its arity
 -}
parMapN
  :: (Applicative f, Monad m, Parallel f m)
  => m a0
  -> m a1
  -> (a0 -> a1 -> a)
  -> m a
parMapN ma0 ma1 f =
  unwrapNT sequential (f <$> unwrapNT parallel ma0 <*> unwrapNT parallel ma1)

parTupled
  :: (Applicative f, Monad m, Parallel f m) => m a0 -> m a1 -> m (a0, a1)
parTupled ma0 ma1 = parMapN ma0 ma1 (,)

parTraverse
  :: (Traversable t, Applicative f, Monad m, Parallel f m)
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f xs =
  let g a = unwrapNT parallel (f a)
      res = sequenceA $ fmap g xs
  in  unwrapNT sequential res

parTraverse_
  :: (Traversable t, Applicative f, Monad m, Parallel f m)
  => (a -> m b)
  -> t a
  -> m ()
parTraverse_ f xs = void $ parTraverse f xs
