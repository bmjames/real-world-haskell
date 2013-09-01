module CountEntries where

import Control.Monad       (guard, liftM2, forM_, void, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Function       (on)
import Data.Maybe          (catMaybes)
import System.Directory    (doesDirectoryExist, getDirectoryContents)
import System.FilePath     ((</>))


listDirectory :: FilePath -> MaybeT IO [FilePath]
listDirectory dir = do
  isDir    <- liftIO $ doesDirectoryExist dir
  guard isDir
  contents <- liftIO $ getDirectoryContents dir
  return $ filter notDots contents
    where notDots = (liftM2 (&&) `on` (/=)) "." ".."

countEntries :: FilePath -> MaybeT IO [(FilePath, Int)]
countEntries path = do
  contents  <- listDirectory path
  children  <- liftIO $ catMaybeTs $ map (countEntries . (path </>)) contents
  return $ (path, length contents) : join children

catMaybeTs :: (Functor f, Monad f) => [MaybeT f a] -> f [a]
catMaybeTs = fmap catMaybes . mapM runMaybeT

printEntries :: FilePath -> IO ()
printEntries path = void $ runMaybeT $ do
  entries <- countEntries path
  forM_ entries $ liftIO . print
