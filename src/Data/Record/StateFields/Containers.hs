
module Data.Record.StateFields.Containers
  ( module Data.Record.StateFields
  , module Data.Record.StateFields.Containers
  ) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Record.StateFields

enterMapKey f k m = do
  b <- getf f
  let (x, c') = runState m $ b Map.! k
  putf f $ Map.insert k c' b
  return x

enterMapKeyT f k m = do
  b <- getf f
  (x, c') <- lift $ runStateT m $ b Map.! k
  putf f $ Map.insert k c' b
  return x

enterMap f m = getf f >>= mapM (\k -> enterMapKey f k m) . Map.keys

enterMapT f m = getf f >>= mapM (\k -> enterMapKeyT f k m) . Map.keys

