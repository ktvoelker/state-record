
module Data.Record.StateFields
  ( Core.Field()
  , Core.record
  , SomeField(..)
  , modField
  , FieldPath()
  , (//)
  , getf
  , putf
  , modf
  , enter
  , enterT
  ) where

import Control.Monad.State
import qualified Data.Record.StateFields.Core as Core
import Data.Record.StateFields.Core (Field(), record)

-- | The class of field descriptors. A descriptor of type 'f a b' refers to a
--   field of type 'b' nested somewhere within a record of type 'a'.
class SomeField f where
  -- | Get the value of a field.
  getField :: f a b -> a -> b
  -- | Put a value into a field.
  putField :: f a b -> b -> a -> a

instance SomeField Field where
  getField = Core.getField
  putField = Core.putField

-- | Modify the value of a field by applying a function.
modField :: (SomeField f) => f s a -> (a -> a) -> s -> s
modField f g c = putField f (g $ getField f c) c

-- | A compound field descriptor.
data FieldPath f g b a c = FieldPath (f a b) (g b c)

instance (SomeField f, SomeField g) => SomeField (FieldPath f g b) where
  getField (FieldPath x y) = getField y . getField x
  putField (FieldPath x y) v s = modField x (\a -> putField y v a) s

infixl 9 //

-- | Join two field descriptors into a compound.
--   '//' is left-associative with precedence level 9.
(//) :: (SomeField f, SomeField g) => f a b -> g b c -> FieldPath f g b a c
(//) = FieldPath

-- | Get the value of a field from the state.
getf :: (MonadState s m, SomeField f) => f s a -> m a
getf f = gets $ getField f

-- | Put a value into a field in the state.
putf :: (MonadState s m, SomeField f) => f s a -> a -> m ()
putf f = modify . putField f

-- | Modify the value of a field in the state by applying a function.
modf :: (MonadState s m, SomeField f) => f s a -> (a -> a) -> m ()
modf f g = modify $ modField f g

-- | Enter the context of a field and run a stateful computation there.
enter :: (MonadState s m, SomeField f) => f s a -> State a b -> m b
enter f s = do
  x <- getf f
  let (y, x') = runState s x
  putf f x'
  return y

-- | Like 'enter', but allows the stateful computation on the field to
--   share the same underlying monad as the enclosing record.
enterT :: (Monad m, SomeField f) => f s a -> StateT a m b -> StateT s m b
enterT f s = do
  x <- getf f
  (y, x') <- lift $ runStateT s x
  putf f x'
  return y

