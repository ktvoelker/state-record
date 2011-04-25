
module Data.Record.StateFields
  ( IdField(..)
  , StateField()
  , mkStateField
  , idStateField
  , ReaderField()
  , mkReaderField
  , idReaderField
  , FieldPath(..)
  , RField(..)
  , WField(..)
  , RWField(..)
  , MonadGet(..)
  , getStoreProj
  , modf
  , enter
  , enterT
  , proj
  , projT
  , record
  ) where

import Data.Record.StateFields.Core
import Data.Record.StateFields.Templates

import Control.Monad.Reader
import Control.Monad.State

-- | A field descriptor with stateful effects.
data StateField m a b = StateField
  { getFieldM :: m b
  , putFieldM :: b -> m ()
  }

-- | Make a StateField.
mkStateField :: (MonadState a m) => m b -> (b -> m ()) -> StateField m a b
mkStateField = StateField

-- | Make a StateField from an IdField.
idStateField :: (MonadState a m) => IdField a b -> StateField m a b
idStateField f =
  StateField
  { getFieldM = gets $ getField f
  , putFieldM = \b -> get >>= \a -> put $ putField f b a
  }

-- | A field descriptor which operates in an environment.
data ReaderField m a b = ReaderField
  { getFieldR :: m b
  }

mkReaderField :: (MonadReader a m) => m b -> ReaderField m a b
mkReaderField = ReaderField

-- | Make a ReaderField from an IdField.
idReaderField :: (MonadReader a m) => IdField a b -> ReaderField m a b
idReaderField f =
  ReaderField
  { getFieldR = asks $ getField f
  }

infixl 9 //

-- | The class of field descriptors which can be chained into paths.
class FieldPath f g h | f g -> h where
  -- | Join two field descriptors into a compound.
  --   '//' is left-associative with precedence level 9.
  (//) :: f -> g -> h

instance FieldPath (IdField a b) (IdField b c) (IdField a c) where
  ab // bc =
    IdField
    { getField = getField bc . getField ab
    , putField = \c a -> putField ab (putField bc c $ getField ab a) a
    }

instance FieldPath (IdField a b) (StateField (State b) b c)
  (StateField (State a) a c) where
  ab // bc = (idStateField ab :: StateField (State a) a b) // bc

instance (Monad m) => FieldPath (IdField a b) (StateField (StateT b m) b c)
  (StateField (StateT a m) a c) where
  ab // bc = (idStateField ab :: StateField (StateT a m) a b) // bc

instance FieldPath (StateField (State a) a b) (IdField b c)
  (StateField (State a) a c) where
  ab // bc = ab // (idStateField bc :: StateField (State b) b c)

instance (Monad m) => FieldPath (StateField (StateT a m) a b) (IdField b c)
  (StateField (StateT a m) a c) where
  ab // bc = ab // (idStateField bc :: StateField (StateT b m) b c)

instance (Monad m) =>
  FieldPath (StateField (StateT a m) a b) (StateField (StateT b m) b c)
    (StateField (StateT a m) a c) where
  ab // bc =
    StateField
    { getFieldM = enterT ab $ getFieldM bc
    , putFieldM = enterT ab . putFieldM bc
    }

instance (Monad m) =>
  FieldPath (StateField (StateT a m) a b) (StateField (State b) b c)
    (StateField (StateT a m) a c) where
  ab // bc =
    StateField
    { getFieldM = enter ab $ getFieldM bc
    , putFieldM = enter ab . putFieldM bc
    }

instance FieldPath (StateField (State a) a b) (StateField (State b) b c)
    (StateField (State a) a c) where
  ab // bc =
    StateField
    { getFieldM = enter ab $ getFieldM bc
    , putFieldM = enter ab . putFieldM bc
    }

instance FieldPath (IdField a b) (ReaderField (Reader b) b c)
  (ReaderField (Reader a) a c) where
  ab // bc = (idReaderField ab :: ReaderField (Reader a) a b) // bc

instance (Monad m) => FieldPath (IdField a b) (ReaderField (ReaderT b m) b c)
  (ReaderField (ReaderT a m) a c) where
  ab // bc = (idReaderField ab :: ReaderField (ReaderT a m) a b) // bc

instance FieldPath (ReaderField (Reader a) a b) (IdField b c)
  (ReaderField (Reader a) a c) where
  ab // bc = ab // (idReaderField bc :: ReaderField (Reader b) b c)

instance (Monad m) => FieldPath (ReaderField (ReaderT a m) a b) (IdField b c)
  (ReaderField (ReaderT a m) a c) where
  ab // bc = ab // (idReaderField bc :: ReaderField (ReaderT b m) b c)

instance (Monad m) =>
  FieldPath (ReaderField (ReaderT a m) a b) (ReaderField (ReaderT b m) b c)
    (ReaderField (ReaderT a m) a c) where
  ab // bc =
    ReaderField
    { getFieldR = projT ab $ getFieldR bc
    }

instance (Monad m) =>
  FieldPath (ReaderField (ReaderT a m) a b) (ReaderField (Reader b) b c)
    (ReaderField (ReaderT a m) a c) where
  ab // bc =
    ReaderField
    { getFieldR = proj ab $ getFieldR bc
    }

instance FieldPath (ReaderField (Reader a) a b) (ReaderField (Reader b) b c)
    (ReaderField (Reader a) a c) where
  ab // bc =
    ReaderField
    { getFieldR = proj ab $ getFieldR bc
    }

-- | The class of monads which have an internal store that can be retrieved.
class (Monad m) => MonadGet s m | m -> s where
  getStore :: m s

instance MonadGet s (Reader s) where
  getStore = ask

instance MonadGet s (State s) where
  getStore = get

instance (Monad m) => MonadGet s (ReaderT s m) where
  getStore = ask

instance (Monad m) => MonadGet s (StateT s m) where
  getStore = get

-- | Get a projection of the monadic store.
getStoreProj :: (MonadGet s m) => (s -> a) -> m a
getStoreProj f = getStore >>= return . f

-- | The class of readable field descriptors.
class RField f m a | f -> a, m -> a where
  getf :: f b -> m b

instance (MonadGet a m) => RField (IdField a) m a where
  getf = getStoreProj . getField

instance (MonadState a m) => RField (StateField m a) m a where
  getf = getFieldM

instance (MonadReader a m) => RField (ReaderField m a) m a where
  getf = getFieldR

-- | The class of writable field descriptors.
class WField f m a | f -> a, m -> a where
  putf :: f b -> b -> m ()

instance WField (IdField a) (State a) a where
  putf f b = get >>= put . putField f b

instance (Monad m) => WField (IdField a) (StateT a m) a where
  putf f b = get >>= put . putField f b

instance (MonadState a m) => WField (StateField m a) m a where
  putf = putFieldM

-- | The class of field descriptors which are both readable and writable.
class (RField f m a, WField f m a) => RWField f m a | f -> a, m -> a

instance (RField f m a, WField f m a) => RWField f m a

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

