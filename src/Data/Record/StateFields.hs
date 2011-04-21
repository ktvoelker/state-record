
module Data.Record.StateFields
  ( IdField()
  , MonadField()
  , FieldPath()
  , (//)
  , RField(..)
  , WField(..)
  , RWField(..)
  , modf
  , enter
  , enterT
  , proj
  , projT
  ) where

import Data.Record.StateFields.Core
import Data.Record.StateFields.Templates

import Control.Monad.Reader
import Control.Monad.State

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

-- | The class of readable field descriptors.
class RField f m a | f -> a, m -> a where
  getf :: f b -> m b

-- | The class of writable field descriptors.
class WField f m a | f -> a, m -> a where
  putf :: f b -> b -> m ()

-- | The class of field descriptors which are both readable and writable.
class (RField f m a, WField f m a) => RWField f m a | f -> a, m -> a

instance (RField f m a, WField f m a) => RWField f m a

instance RField (IdField a) (State a) a where
  getf = gets . getField

instance WField (IdField a) (State a) a where
  putf f b = get >>= put . putField f b

instance (Monad m) => RField (IdField a) (StateT a m) a where
  getf = gets . getField

instance (Monad m) => WField (IdField a) (StateT a m) a where
  putf f b = get >>= put . putField f b

instance (MonadState a m) => RField (MonadField (State a) a) m a where
  getf f = do
    a <- get
    let (b, a') = runState (getFieldM f) a
    put a'
    return b

instance (MonadState a m) => WField (MonadField (State a) a) m a where
  putf f b = do
    a <- get
    let a' = execState (putFieldM f b) a
    put a'

instance (Monad m) => RField (MonadField (StateT a m) a) (StateT a m) a where
  getf = getFieldM

instance (Monad m) => WField (MonadField (StateT a m) a) (StateT a m) a where
  putf = putFieldM

instance (MonadState a m, RWField f m a, RWField g (State b) b)
  => RField (FieldPath f g b a) m a where
  getf (FieldPath f g) = do
    b <- getf f
    let (c, b') = runState (getf g) b
    putf f b'
    return c

instance (MonadState a m, RWField f m a, RWField g (State b) b)
  => WField (FieldPath f g b a) m a where
  putf (FieldPath f g) c = do
    b <- getf f
    let b' = execState (putf g c) b
    putf f b'

instance (Monad m, RWField f (StateT a m) a, RWField g (StateT b m) b)
  => RField (FieldPath f g b a) (StateT a m) a where
  getf (FieldPath f g) = do
    b <- getf f
    (c, b') <- lift $ runStateT (getf g) b
    putf f b'
    return c

instance (Monad m, RWField f (StateT a m) a, RWField g (StateT b m) b)
  => WField (FieldPath f g b a) (StateT a m) a where
  putf (FieldPath f g) c = do
    b <- getf f
    b' <- lift $ execStateT (putf g c) b
    putf f b'

instance (Monad m) => RField (IdField a) (ReaderT a m) a where
  getf = asks . getField

instance (MonadReader a m) => RField (MonadField (Reader a) a) m a where
  getf f = do
    a <- ask
    let b = runReader (getFieldM f) a
    return b

instance (Monad m) => RField (MonadField (ReaderT a m) a) (ReaderT a m) a where
  getf = getFieldM

instance (Monad m, RField f (ReaderT a m) a, RField g (ReaderT b m) b)
  => RField (FieldPath f g b a) (ReaderT a m) a where
  getf (FieldPath f g) = do
    b <- getf f
    c <- lift $ runReaderT (getf g) b
    return c

-- | Modify the value of a field by applying a function.
modf :: (Monad m, RWField f m a) => f b -> (b -> b) -> m ()
modf f g = getf f >>= putf f . g

-- | Run a stateful computation with a field as the state.
enter :: (Monad m, RWField f m a) => f b -> State b x -> m x
enter f m = do
  b <- getf f
  let (x, b') = runState m b
  putf f b'
  return x

-- | Like 'enter', but allows the subcomputation to share the underlying
--   monad of the enclosing computation.
enterT
  :: (Monad m, RWField f (StateT a m) a)
  => f b -> StateT b m x
  -> StateT a m x
enterT f m = do
  b <- getf f
  (x, b') <- lift $ runStateT m b
  putf f b'
  return x

-- | Run a computation with a field as the environment.
proj :: (Monad m, RField f m a) => f b -> Reader b x -> m x
proj f m = do
  b <- getf f
  let x = runReader m b
  return x

-- | Like 'proj', but allows the subcomputation to share the underlying
--   monad of the enclosing computation.
projT
  :: (Monad m, RField f (ReaderT a m) a)
  => f b -> ReaderT b m x
  -> ReaderT a m x
projT f m = do
  b <- getf f
  x <- lift $ runReaderT m b
  return x

