{-# LANGUAGE RecordWildCards #-}

module Database.Persist (
    Options(..),
    opts,
    start,
    stop
) where

import           Control.Concurrent
import           Data.Aeson                 (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.IO                  as IO
import           Control.Monad              (forever, void)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Exception
import           System.IO.Error            (isDoesNotExistError)

data Options = Options
    { poOnInterval :: Maybe Int  -- seconds
    , poFilename   :: FilePath
    }

opts :: FilePath -> Options
opts name = Options
    { poOnInterval = Just 1
    , poFilename   = name
    }

start :: (MonadIO m, Eq v, ToJSON v, FromJSON v) => Options -> MVar v -> m Persist
start Options{..} mv = liftIO $ do

    -- initialise persist object to control this later.
    isStopped <- newMVar False

    let controls = Persist
          { pIsStopped = isStopped
          }

    -- load in file contents initially if asked to (and possible to):
    tryInitialRead poFilename mv `catch` doesNotExist

    -- write back to file if asked to on interval:
    case poOnInterval of
        Just i  -> void $ forkIO $ writeLoop controls poFilename (i * 1000000) mv
        Nothing -> return ()

    return controls

  where
    doesNotExist e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
    tryInitialRead name mv = do
        file <- BL.readFile name
        case decode file of
            Just c  -> void $ do
                isEmpty <- isEmptyMVar mv
                if isEmpty then putMVar mv c else void (swapMVar mv c)
            Nothing -> error "JSON file FB doesn't match expected schema; quitting."

writeLoop :: (MonadIO m, ToJSON v, Eq v) => Persist -> FilePath -> Int -> MVar v -> m ()
writeLoop p name i mv = liftIO $ do
    curVal <- readMVar mv
    loop p mv curVal
  where
    loop p@Persist{..} mv curVal = do
      newVal <- readMVar mv
      if newVal /= curVal
          then BL.writeFile name (encode newVal)
          else return ()
      threadDelay i
      bStopped <- readMVar pIsStopped
      if not bStopped
          then loop p mv newVal
          else return ()

--
-- A persisted object. use this to alter/stop persistance.
-- don't expose this externally; no pattern matching on it.
--
data Persist = Persist
    { pIsStopped :: MVar Bool
    }

--
-- functions to operate on our persist object. expose these.
--
stop :: MonadIO m => Persist -> m ()
stop Persist{..} = liftIO $ void $ swapMVar pIsStopped True