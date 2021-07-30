module Graph
  ( findBacklinks
  , findConnectedNodes
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Types (FileId, FileData(..))

findConnectedNodes :: M.Map FileId FileData -> FileId -> S.Set FileId
findConnectedNodes idToData = add S.empty
    where add :: S.Set FileId -> FileId -> S.Set FileId
          add s id =
            if id `M.member` idToData && id `S.notMember` s
            then foldl' add s' (links ++ backlinks)
            else s
              where links = S.toList . fileLinkedIds $ idToData M.! id
                    backlinks = map fileId $ findBacklinks idToData id
                    s' = S.insert id s

findBacklinks :: M.Map FileId FileData -> FileId -> [FileData]
findBacklinks idToFile id = M.foldl (\a v ->
    if id `S.member` fileLinkedIds v then v:a else a) [] idToFile
