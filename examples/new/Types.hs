{-# LANGUAGE TemplateHaskell #-}
module Types
  ( module Control.Monad.State
  , module Data.Record.StateFields
  , module Types) where
import Control.Monad.State
import Data.Record.StateFields
record "" [d| data Foo = Foo { a :: Int, b :: Int } deriving Show |]
record "" [d| data Bar = Bar { x :: Foo, y :: Foo } deriving Show |]
