
{-# LANGUAGE TemplateHaskell #-}
module Data.Record.StateFields.Templates where

import Data.Record.StateFields.Core

import Data.Char
import Language.Haskell.TH

-- | Modify the given 'data' or 'newtype' declaration so that all field names
--   are prefixed with an underscore followed by the given string, and
--   generate declarations of field descriptors for all fields, each bound to
--   the corresponding field name prefixed with the given string (but no
--   underscore).
--
--   Example usage (this goes at the top level of a module):
--
--   > record "foo" [d| data Foo = Foo { bar :: Int, baz :: Int } |]
--
--   Note: the second parameter is Q [Dec] because this is what the [d| |]
--   form returns, which is the most convenient way to use this function.
--   However, the list must contain exactly one declaration, and it must be
--   a 'data' or 'newtype' declaration.
--
--   Note: in addition to adding the given prefix to each name, the first
--   character of the original name is capitalized (unless the prefix is empty).
record :: String -> Q [Dec] -> Q [Dec]
record pre ds = ds >>= \ds -> case ds of
  [DataD cxt name tvs cons dvs] ->
    sequence
    $ return (DataD cxt name tvs (map mkCon cons) dvs)
    : concatMap mkFields cons
  [NewtypeD cxt name tvs con dvs] ->
    sequence
    $ return (NewtypeD cxt name tvs (mkCon con) dvs)
    : mkFields con
  _ ->
    fail
    $ "A `record' declaration must be given exactly one "
    ++ "`data' or `newtype' declaration."
  where
    ucFirst [] = []
    ucFirst (x : xs) = if pre == "" then x : xs else toUpper x : xs
    rawName name = mkName $ '_' : pre ++ ucFirst (nameBase name)
    fieldName name = mkName $ pre ++ ucFirst (nameBase name)
    mkCon (RecC name vs) = RecC name $ map mkVar vs
    mkCon x = x
    mkVar (name, str, ty) = (rawName name, str, ty)
    mkFields (RecC _ vs) = map mkField vs
    mkFields _ = []
    mkField (name, str, ty) = do
      r <- newName "r"
      v <- newName "v"
      valD
        (varP fName)
        (normalB [|
          IdField
          { getField = $(varE rName)
          , putField = $(lamE [varP v, varP r]
            $ recUpdE (varE r) [return (rName, VarE v)])
          }
        |])
        []
      where
        fName = fieldName name
        rName = rawName name

record' :: IO ()
record' =
  runQ (record "bar" [d| data Bar = Bar { bar :: Int } |])
  >>= print . ppr

