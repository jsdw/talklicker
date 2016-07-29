module Sessions (Sessions, exists, get, remove, create) where

import Control.Concurrent
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Trans (MonadIO, liftIO)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString.Char8 as ByteChars
import qualified Data.ByteString as Bytes

type SessionId = String

newtype Sessions a = Sessions (MVar (Map SessionId a))

exists :: MonadIO m => SessionId -> Sessions a -> m Bool
exists id (Sessions m) = liftIO $ readMVar m >>= return . Map.member id

get :: MonadIO m => SessionId -> Sessions a -> m (Maybe a)
get id (Sessions m) = liftIO $ readMVar m >>= return . Map.lookup id

remove :: MonadIO m => SessionId -> Sessions a -> m ()
remove id (Sessions m) = liftIO $ modifyMVar_ m $ \map -> return (Map.delete id map)

create :: MonadIO m => a -> Sessions a -> m SessionId
create a (Sessions m) = liftIO $ modifyMVar m $ \map -> do
    id <- generateSessionId
    return (Map.insert id a map, id)

generateSessionId :: IO SessionId
generateSessionId = do
    bytes <- getRandomBytes 30
    return $ ByteChars.unpack $ Bytes.map toAZ bytes
  where
    toAZ n = (n `mod` 26) + 97