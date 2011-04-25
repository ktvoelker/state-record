
module Data.Record.StateFields
  ( IdField(..)
  , StateField(..)
  , idStateField
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
idStateField f =
  StateField
  { getFieldM = gets $ getField f
  , putFieldM = \b -> get >>= \a -> put $ putField f b a
  }

-- | A field descriptor which operates in an environment.
data ReaderField a b = ReaderField
  { askFieldM :: (MonadReader a m) => m b
  }

-- | Make a ReaderField from an IdField.
idReaderField f =
  ReaderField
  { askFieldM = asks $ getField f
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

instance FieldPath (IdField a b) (StateField b c) (StateField a c) where
  ab // bc = idStateField ab // bc

instance FieldPath (StateField a b) (IdField b c) (StateField a c) where
  ab // bc = ab // idStateField bc

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

instance FieldPath (ReaderField a b) (ReaderField b c) (ReaderField a c) where
  ab // bc =
    ReaderField
    { askFieldM = proj ab $ askFieldM bc
    }

-- | The class of field descriptors usable with a State monad.
class FieldState f where
  getf :: (MonadState a m) => f a b -> m b
  putf :: (MonadState a m) => f a b -> b -> m ()

instance FieldState IdField where
  getf = gets . getField
  putf f b = get >>= put . putField f b

instance FieldState StateField where
  getf = getFieldM
  putf = putFieldM

-- | Modify the value of a field by applying a function.
modf :: (FieldState f, MonadState a m) => f a b -> (b -> b) -> m ()
modf f g = getf f >>= putf f . g

-- | Run a stateful computation with a field as the state.
enter :: (FieldState f, MonadState a m) => f a b -> State b x -> m x
enter f m = do
  b <- getf f
  let (x, b') = runState m b
  putf f b'
  return x

-- | Like 'enter', but allows the subcomputation to share the underlying
--   monad of the enclosing computation.
enterT :: (Monad m, FieldState f) => f a b -> StateT b m x -> StateT a m x
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

