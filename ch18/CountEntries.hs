module CountEntries where

import Control.Monad       (join, guard, liftM2)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Function       (on)
import Data.Traversable    (traverse)
import System.Directory    (doesDirectoryExist, getDirectoryContents)
import System.FilePath     ((</>))


listDirectory :: FilePath -> MaybeT IO [FilePath]
listDirectory dir = do
  isDir    <- liftIO (doesDirectoryExist dir)
  guard isDir
  contents <- liftIO $ getDirectoryContents dir
  return $ filter notDots contents
    where notDots = (liftM2 (&&) `on` (/=)) "." ".."

countEntries :: FilePath -> MaybeT IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest     <- traverse (countEntries . (path </>)) contents
  return $ (path, length contents) : join rest
