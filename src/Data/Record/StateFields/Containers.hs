
module Data.Record.StateFields.Containers
  ( module Data.Record.StateFields
  , module Data.Record.StateFields.Containers
  ) where

import qualified Data.Array.IArray as A
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Record.StateFields

mapKey :: (Ord k) => k -> IdField (M.Map k a) (Maybe a)
mapKey k = IdField
  { getField = M.lookup k
  , putField = flip M.alter k . const
  }

mapKeyDef :: (Ord k) => a -> k -> IdField (M.Map k a) a
mapKeyDef def k = IdField
  { getField = maybe def id . M.lookup k
  , putField = M.insert k
  }

setElem :: (Ord a) => a -> IdField (S.Set a) Bool
setElem x = IdField
  { getField = S.member x
  , putField = \b -> (if b then S.insert else S.delete) x
  }

arrIdx :: (A.Ix i, A.IArray a e) => i -> IdField (a i e) (Maybe e)
arrIdx i = IdField
  { getField = \a -> if A.inRange (A.bounds a) i then Just (a A.! i) else Nothing
  , putField =
      maybe id
      $ \e a -> if A.inRange (A.bounds a) i then a A.// [(i, e)] else a
  }

