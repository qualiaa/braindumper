module Util
  ( whenNothing
  ) where

whenNothing :: (Monad m) => Maybe a -> m () -> m (Maybe a)
whenNothing Nothing action = action >> return Nothing
whenNothing x _ = return x
