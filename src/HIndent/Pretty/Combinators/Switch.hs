{-# LANGUAGE LambdaCase #-}

-- | Printer combinators for switching printers depending on situations.
module HIndent.Pretty.Combinators.Switch
  ( (<-|>)
  ) where

import           Control.Applicative
import           Control.Monad.State
import           HIndent.Types

-- | This function runs the first printer if the result of running it fits
-- in a single line. Otherwise, it runs the second printer.
(<-|>) :: Printer a -> Printer a -> Printer a
fit <-|> notFit = do
  before <- get
  put before {psFitOnOneLine = True}
  fmap Just fit <|> return Nothing >>= \case
    Just r -> do
      modify $ \st -> st {psFitOnOneLine = psFitOnOneLine before}
      return r
    Nothing -> do
      put before
      guard $ not $ psFitOnOneLine before
      notFit
