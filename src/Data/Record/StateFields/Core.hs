
module Data.Record.StateFields.Core where

-- | A primitive field descriptor.
data IdField a b = IdField
  { getField :: a -> b
  , putField :: b -> a -> a
  }

