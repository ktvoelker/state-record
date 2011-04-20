
module Data.Record.StateFields
  (
  ) where

import Data.Record.StateFields.Core
import Data.Record.StateFields.Templates

import Control.Monad.Reader
import Control.Monad.State

{-- New Stuff --}

-- | A field descriptor with monadic effects.
data MonadField m a b = MonadField
  { getFieldM :: m b
  , putFieldM :: b -> m ()
  }

-- | A compound field descriptor.
data FieldPath f g b a c = FieldPath (f b) (g c)

infixl 9 //

-- | Join two field descriptors into a compound.
--   '//' is left-associative with precedence level 9.
(//) :: f b -> g c -> FieldPath f g b a c
(//) = FieldPath

class Field f m a | f -> a, m -> a where
  getf :: f b -> m b
  putf :: f b -> b -> m ()

instance Field (IdField a) (State a) a where
  getf = gets . getField
  putf f b = get >>= put . putField f b

instance (Monad m) => Field (IdField a) (StateT a m) a where
  getf = gets . getField
  putf f b = get >>= put . putField f b

instance (MonadState a m) => Field (MonadField (State a) a) m a where
  getf f = do
    a <- get
    let (b, a') = runState (getFieldM f) a
    put a'
    return b
  putf f b = do
    a <- get
    let a' = execState (putFieldM f b) a
    put a'

instance (Monad m) => Field (MonadField (StateT a m) a) (StateT a m) a where
  getf = getFieldM
  putf = putFieldM

instance (MonadState a m, Field f m a, Field g (State b) b)
  => Field (FieldPath f g b a) m a where
  getf (FieldPath f g) = do
    b <- getf f
    let (c, b') = runState (getf g) b
    putf f b'
    return c
  putf (FieldPath f g) c = do
    b <- getf f
    let b' = execState (putf g c) b
    putf f b'

instance (Monad m, Field f (StateT a m) a, Field g (StateT b m) b)
  => Field (FieldPath f g b a) (StateT a m) a where
  getf (FieldPath f g) = do
    b <- getf f
    (c, b') <- lift $ runStateT (getf g) b
    putf f b'
    return c
  putf (FieldPath f g) c = do
    b <- getf f
    b' <- lift $ execStateT (putf g c) b
    putf f b'

