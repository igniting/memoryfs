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
    return $ MemoryFS (diskView fs) node node
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

  createDirectory fs name = do
    node <- createDirectory' (processView fs) (getNodeIds name)
    return $ MemoryFS (diskView fs) node node
    where
      createDirectory' :: Node a -> [NodeId] -> Maybe (Node a)
      createDirectory' _ []     = Nothing
      createDirectory' Leaf{} _ = Nothing
      createDirectory' _ [_]    = Nothing
      createDirectory' (Node nid m) (x:y:xs)
        | nid /= x = Nothing
        | null xs  = case M.lookup y m of
                          Nothing -> Just $ Node nid (M.insert y (Node y M.empty) m)
                          Just _  -> Nothing
        | otherwise = do n <- M.lookup y m
                         n' <- createDirectory' n (y:xs)
                         return $ Node nid (M.insert y n' m)

  fsync fs = MemoryFS (processView fs) (processView fs) (processView fs)
