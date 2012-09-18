
module Data.Record.StateFields
  ( IdField(..)
  , StateField(..)
  , idStateField
  , StateTField(..)
  , idStateTField
  , ReaderField(..)
  , idReaderField
  , FieldPath(..)
  , FieldState(..)
  , modf
  , enter
  , enterT
  , FieldReader(..)
  , proj
  , projT
  , record
  , (!/)
  , (&/)
  ) where

import Data.Record.StateFields.Core
import Data.Record.StateFields.Templates

import Control.Monad.Reader
import Control.Monad.State

-- | A field descriptor with stateful effects.
data StateField a b = StateField
  { getFieldM :: (MonadState a m) => m b
  , putFieldM :: (MonadState a m) => b -> m ()
  }

-- | Make a StateField from an IdField.
idStateField :: IdField a b -> StateField a b
idStateField f =
  StateField
  { getFieldM = gets $ getField f
  , putFieldM = \b -> get >>= \a -> put $ putField f b a
  }

-- | A field descriptor with stateful and other effects.
data StateTField m a b = StateTField
  { getFieldT :: StateT a m b
  , putFieldT :: b -> StateT a m ()
  }

-- | Make a StateTField from an IdField.
idStateTField :: (Monad m) => IdField a b -> StateTField m a b
idStateTField f =
  StateTField
  { getFieldT = gets $ getField f
  , putFieldT = \b -> get >>= \a -> put $ putField f b a
  }

-- | A field descriptor which operates in an environment.
data ReaderField a b = ReaderField
  { askFieldM :: (MonadReader a m) => m b
  }

-- | Make a ReaderField from an IdField.
idReaderField :: IdField a b -> ReaderField a b
idReaderField f =
  ReaderField
  { askFieldM = asks $ getField f
  }

infixl 9 //, !/, &/

(!/) :: a -> IdField a b -> b
(!/) = flip getField

(&/) :: a -> (IdField a b, b) -> a
(&/) rec (f, val) = putField f val rec

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

instance FieldPath (IdField a b) (StateField b c) (StateField a c) where
  ab // bc = idStateField ab // bc

instance FieldPath (StateField a b) (IdField b c) (StateField a c) where
  ab // bc = ab // idStateField bc

instance (Monad m) => FieldPath (IdField a b) (StateTField m b c) (StateTField m a c) where
  ab // bc = (idStateTField ab :: StateTField m a b) // bc

instance (Monad m) => FieldPath (StateTField m a b) (IdField b c) (StateTField m a c) where
  ab // bc = ab // (idStateTField bc :: StateTField m b c)

instance FieldPath (IdField a b) (ReaderField b c) (ReaderField a c) where
  ab // bc = idReaderField ab // bc

instance FieldPath (ReaderField a b) (IdField b c) (ReaderField a c) where
  ab // bc = ab // idReaderField bc

instance FieldPath (StateField a b) (StateField b c) (StateField a c) where
  ab // bc =
    StateField
    { getFieldM = enter ab $ getFieldM bc
    , putFieldM = enter ab . putFieldM bc
    }

instance (Monad m) =>
  FieldPath (StateTField m a b) (StateTField m b c) (StateTField m a c) where
  ab // bc =
    StateTField
    { getFieldT = enterT ab $ getFieldT bc
    , putFieldT = enterT ab . putFieldT bc
    }

instance FieldPath (ReaderField a b) (ReaderField b c) (ReaderField a c) where
  ab // bc =
    ReaderField
    { askFieldM = proj ab $ askFieldM bc
    }

-- | The class of field descriptors usable with a State monad.
class (MonadState a m) => FieldState f a m | f -> a where
  getf :: f b -> m b
  putf :: f b -> b -> m ()

instance (MonadState a m) => FieldState (IdField a) a m where
  getf = gets . getField
  putf f b = get >>= put . putField f b

instance (MonadState a m) => FieldState (StateField a) a m where
  getf = getFieldM
  putf = putFieldM

instance (Monad m) => FieldState (StateTField m a) a (StateT a m) where
  getf = getFieldT
  putf = putFieldT

-- | Modify the value of a field by applying a function.
modf :: (FieldState f a m, MonadState a m) => f b -> (b -> b) -> m ()
modf f g = getf f >>= putf f . g

-- | Run a stateful computation with a field as the state.
enter :: (FieldState f a m, MonadState a m) => f b -> State b x -> m x
enter f m = do
  b <- getf f
  let (x, b') = runState m b
  putf f b'
  return x

-- | Like 'enter', but allows the subcomputation to share the underlying
--   monad of the enclosing computation.
enterT :: (Monad m, FieldState f a (StateT a m)) => f b -> StateT b m x -> StateT a m x
enterT f m = do
  b <- getf f
  (x, b') <- lift $ runStateT m b
  putf f b'
  return x

-- | The class of field descriptors usable with a Reader monad.
class FieldReader f where
  askf :: (MonadReader a m) => f a b -> m b

instance FieldReader IdField where
  askf = asks . getField

instance FieldReader ReaderField where
  askf = askFieldM

-- | Run a computation with a field as the environment.
proj :: (FieldReader f, MonadReader a m) => f a b -> Reader b x -> m x
proj f m = do
  b <- askf f
  let x = runReader m b
  return x

-- | Like 'proj', but allows the subcomputation to share the underlying
--   monad of the enclosing computation.
projT :: (Monad m, FieldReader f) => f a b -> ReaderT b m x -> ReaderT a m x
projT f m = do
  b <- askf f
  x <- lift $ runReaderT m b
  return x

