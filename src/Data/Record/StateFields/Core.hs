
{-# LANGUAGE TemplateHaskell #-}
module Data.Record.StateFields.Core (Field(..), record) where

import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Field c a = Field
  { getField :: c -> a
  , putField :: a -> c -> c
  }

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
    ucFirst (x : xs) = toUpper x : xs
    rawName name = mkName $ '_' : pre ++ ucFirst (showName name)
    fieldName name = mkName $ pre ++ ucFirst (showName name)
    mkCon (RecC name vs) = RecC name $ map mkVar vs
    mkCon x = x
    mkVar (name, str, ty) = (rawName name, str, ty)
    mkFields (RecC name vs) = map mkField vs
    mkFields _ = []
    mkField (name, str, ty) = do
      r <- newName "r"
      v <- newName "v"
      valD
        (varP fName)
        (normalB [|
          Field
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

