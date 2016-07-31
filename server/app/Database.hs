module Database (Database, init, read, modify) where

import Prelude hiding (init, read)
import Control.Concurrent
import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Default (Default, def)
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

modify :: (MonadIO m, ToJSON a) => Database a -> (a -> IO a) -> m (Database a)
modify db@(Database fileName mv) fn = liftIO $ do
    modifyMVar_ mv fn
    writeMVarToFile fileName mv
    return db

--
-- helpers to read/write to file on mvar init/change
--

tryReadFileToMvar :: (MonadIO m, FromJSON v) => String -> MVar v -> m ()
tryReadFileToMvar fileName mv = liftIO $ do
    tryInitialRead `catch` doesNotExist
  where
    doesNotExist e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
    tryInitialRead = do
        file <- BL.readFile fileName
        case decode file of
            Just c  -> void (swapMVar mv c)
            Nothing -> error "JSON file FB doesn't match expected schema; quitting."

writeMVarToFile :: (MonadIO m, ToJSON v) => String -> MVar v -> m ()
writeMVarToFile fileName mv = liftIO $ do
    curVal <- readMVar mv
    BL.writeFile fileName (encode curVal)
