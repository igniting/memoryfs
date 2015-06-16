module Data.MemoryFS.FileSystem where

type Path = String

class FileSystem a where
  createFile      :: a b -> Path -> b -> Maybe (a b)
  renameFile      :: a b -> Path -> Path -> Maybe (a b)
  createDirectory :: a b -> Path -> Maybe (a b)
  fsync           :: a b -> a b
