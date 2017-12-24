{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLens where

import           Control.Lens

import           System.Directory.Tree (AnchoredDirTree (dirTree),
                                        readDirectoryWith)
import qualified System.Directory.Tree as D
import           System.FilePath       (splitExtension)

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }
    deriving (Show)

scanFS :: FilePath -> IO FS
scanFS root = mapTree . dirTree <$> readDirectoryWith (\_ -> return ()) root
  where
    mapTree :: D.DirTree () -> FS
    mapTree (D.File n _)      = File n
    mapTree (D.Dir n content) = Dir n (map mapTree content)
    mapTree (D.Failed _ err)  = error (show err)

makeLenses ''FS
makePrisms ''FS


subdirs :: FS -> [FS]
subdirs fs = fs ^.. (contents.traversed.filtered(isn't _File))

dirName :: FS -> Maybe FilePath
dirName fs = fs^?_Dir._1

cd :: String -> Traversal' FS FS
cd path = contents.traversed.filtered(\d -> isn't _File d && (_name d) == path)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

file :: String -> Traversal' FS String
file fname = contents.traversed.filtered(\d -> isn't _Dir d && (_name d) == fname).name

chngext :: String -> FS -> FS
chngext newext fs = fs & contents.traversed.filtered(isn't _Dir).name %~ replaceExt
  where
    replaceExt p = let (nm, _) = splitExtension p in nm ++ "." ++ newext

tree :: FS -> [FilePath]
tree fs = fs^.name :
    concatMap (\nfs -> map ((fs^.name ++) "/" ++) (tree nfs)) (fs ^.. contents.traversed)

rmempty :: String -> FS -> FS
rmempty dirname root = root & contents %~
    filter (\fs -> isn't _Dir fs || (_name fs /= dirname) || (not $ null (fs^.contents)))

dirCount :: FS -> Int
dirCount fs = length $ fs^..contents.traversed.filtered(isn't _File)

fileCount :: FS -> Int
fileCount fs = length $ fs^..contents.traversed.filtered(isn't _Dir)

