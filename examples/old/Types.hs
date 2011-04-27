module Types (module Types, module Control.Monad.State) where
import Control.Monad.State
data Foo = Foo { a :: Int, b :: Int } deriving Show
data Bar = Bar { x :: Foo, y :: Foo } deriving Show
