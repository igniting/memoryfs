module Data.MemoryFS where

import           Data.List.Split          (splitOn)
import qualified Data.Map.Strict          as M
import           Data.MemoryFS.FileSystem

type NodeId = String

data Node a = Leaf NodeId a
            | Node NodeId (M.Map NodeId (Node a))
            deriving (Show, Eq)

data MemoryFS a = MemoryFS { diskView     :: Node a
                           , osView       :: Node a
                           , processView  :: Node a
                           } deriving (Show, Eq)

getNodeIds :: Path -> [NodeId]
getNodeIds = splitOn "/"

instance FileSystem MemoryFS where
  createFile fs name contents = do
    node <- createFile' (processView fs) (getNodeIds name) contents
    return $ MemoryFS node node node
    where
      createFile' :: Node a -> [NodeId] -> a -> Maybe (Node a)
      createFile' _ [] _      = Nothing
      createFile' Leaf{} _ _  = Nothing
      createFile' _ [_] _     = Nothing
      createFile' (Node nid m) (x:y:xs) c
        | nid /= x  = Nothing
        | null xs   = case M.lookup y m of
                           Nothing  -> Just $ Node nid (M.insert y (Leaf y c) m)
                           Just _   -> Nothing
        | otherwise = do n <- M.lookup y m
                         n' <- createFile' n (y:xs) c
                         return $ Node nid (M.insert y n' m)

  renameFile = undefined
  createDirectory = undefined
