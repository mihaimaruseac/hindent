-- | Helper functions to handle 'Applicative's
module HIndent.Applicative
  ( whenJust
  ) where

-- | If the first argument is a 'Just' value, this function applies its
-- internal value to the function passed as the second argument. Otherwise,
-- this function returne a 'pure ()'.
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = pure ()
whenJust (Just x) f = f x
