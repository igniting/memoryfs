module Data.MemoryFS where

import qualified Data.Map.Strict          as M
import           Data.MemoryFS.FileSystem

type NodeId = String

data Node a = Leaf NodeId a
            | Node NodeId (M.Map NodeId (Node a))
            deriving (Show, Eq)

data MemoryFS a = MemoryFS { diskView     :: Node a
                           , osView       :: Node a
                           , processView  :: Node a
                           }

instance FileSystem MemoryFS where
  createFile = undefined
  renameFile = undefined
  createDirectory = undefined
