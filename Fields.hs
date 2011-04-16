
{-# LANGUAGE TemplateHaskell #-}
module Fields (Field(..), record, upd, enter) where

import Control.Monad.State
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Field c a = Field
  { getField :: c -> a
  , putField :: c -> a -> c
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
          , putField = $(lamE [varP r, varP v]
            $ recUpdE (varE r) [return (rName, VarE v)])
          }
        |])
        []
      where
        fName = fieldName name
        rName = rawName name

upd :: Name -> Q Exp
upd n = do
  p0 <- newName "f"
  p1 <- newName "x"
  return $
    LamE [VarP p0, VarP p1] $
    RecUpdE (VarE p1) [(n, AppE (VarE p0) (AppE (VarE n) (VarE p1)))]

enter :: Name -> Q Exp
enter n = do
  s <- newName "s"
  v <- newName "v"
  r <- newName "r"
  w <- newName "w"
  t <- newName "t"
  let u = return (n, VarE w)
  let body = [| runState $(varE s) $(varE v) |]
  lamE [varP s] $ doE 
    [ bindS (varP v) [| gets $(varE n) |]
    , letS [valD (tupP [varP r, varP w]) (normalB body) []]
    , noBindS [| modify $(lamE [varP t] $ recUpdE (varE t) [u]) |]
    , noBindS [| return $(varE r) |]
    ]

data Foo = Foo { foo :: Int }

record' :: IO ()
record' =
  runQ (record "bar" [d| data Bar = Bar { bar :: Int } |])
  >>= print . ppr

upd' :: IO ()
upd' = runQ (upd 'foo) >>= print . ppr

enter' :: IO ()
enter' = runQ (enter 'foo) >>= print . ppr

