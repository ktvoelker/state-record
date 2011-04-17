
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

class SomeField p where
  getField :: p s a -> s -> a
  putField :: p s a -> a -> s -> s

instance SomeField Field where
  getField = Core.getField
  putField = Core.putField

modField :: (SomeField f) => f s a -> (a -> a) -> s -> s
modField f g c = putField f (g $ getField f c) c

data FieldPath f g b a c = FieldPath (f a b) (g b c)

instance (SomeField f, SomeField g) => SomeField (FieldPath f g b) where
  getField (FieldPath x y) = getField y . getField x
  putField (FieldPath x y) v s = modField x (\a -> putField y v a) s

infixl 9 //

(//) :: (SomeField f, SomeField g) => f a b -> g b c -> FieldPath f g b a c
(//) = FieldPath

getf :: (MonadState s m, SomeField f) => f s a -> m a
getf f = gets $ getField f

putf :: (MonadState s m, SomeField f) => f s a -> a -> m ()
putf f = modify . putField f

modf :: (MonadState s m, SomeField f) => f s a -> (a -> a) -> m ()
modf f g = modify $ modField f g

enter :: (MonadState s m, SomeField f) => f s a -> State a b -> m b
enter f s = do
  x <- getf f
  let (y, x') = runState s x
  putf f x'
  return y

enterT :: (Monad m, SomeField f) => f s a -> StateT a m b -> StateT s m b
enterT f s = do
  x <- getf f
  (y, x') <- lift $ runStateT s x
  putf f x'
  return y

