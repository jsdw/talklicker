module Database (Database(..), init, read, modify) where

import Prelude hiding (init, read)
import qualified Database.Persist as Persist
import Control.Concurrent (newEmptyMVar, readMVar, modifyMVar_, MVar)
import Data.Aeson (FromJSON, ToJSON)

newtype Database a = Database (MVar a)

init :: (ToJSON a, FromJSON a, Eq a) => String -> IO (Database a)
init fileName = do
    db <- newEmptyMVar
    Persist.start (Persist.opts fileName) db
    return (Database db)

read :: Database a -> IO a
read (Database mv) = readMVar mv

modify :: Database a -> (a -> IO a) -> IO (Database a)
modify (Database mv) fn = modifyMVar_ mv fn >> return (Database mv)