module Database (Database, init, read, modify, modify_) where

import Prelude hiding (init, read)
import Control.Concurrent
import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO.Error (isDoesNotExistError)

data Database a = Database String (MVar a)

init :: (MonadIO m, Default a, ToJSON a, FromJSON a) => String -> m (Database a)
init fileName = liftIO $ do
    db <- newMVar def
    tryReadFileToMvar fileName db
    return (Database fileName db)

read :: MonadIO m => Database a -> m a
read (Database _ mv) = liftIO $ readMVar mv

modify :: (MonadIO m, ToJSON a) => Database a -> (a -> IO (a,b)) -> m b
modify (Database fileName mv) fn = liftIO $ do
    modifyMVar mv $ \a -> do
        out <- fn a
        writeMVarToFile fileName mv
        return out

modify_ :: (MonadIO m, ToJSON a) => Database a -> (a -> IO a) -> m ()
modify_ db fn = modify db (\a -> fn a >>= \newA -> return (newA,()))

--
-- helpers to read/write to file on mvar init/change
--

tryReadFileToMvar :: (MonadIO m, FromJSON v) => String -> MVar v -> m ()
tryReadFileToMvar fileName mv = liftIO $ do
    tryInitialRead `catch` doesNotExist
  where
    doesNotExist e
        | isDoesNotExistError e = do
            putStrLn (fileName <> " does not exist; will create as needed")
            return ()
        | otherwise = throwIO e
    tryInitialRead = do
        file <- BL.readFile fileName
        case decode file of
            Just c  -> void (swapMVar mv c)
            Nothing -> error "JSON file for DB doesn't match expected schema; quitting."

writeMVarToFile :: (MonadIO m, ToJSON v) => String -> MVar v -> m ()
writeMVarToFile fileName mv = liftIO $ do
    curVal <- readMVar mv
    BL.writeFile fileName (encode curVal)
