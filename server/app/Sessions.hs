module Sessions (Sessions, Id, empty, exists, get, remove, create) where

import Control.Concurrent
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Trans (MonadIO, liftIO)
import Crypto.Random (getRandomBytes, MonadRandom)
import qualified Data.ByteString.Char8 as ByteChars
import qualified Data.ByteString as Bytes

type Id = String

newtype Sessions a = Sessions (MVar (Map Id a))

empty :: MonadIO m => m (Sessions a)
empty = liftIO $ newMVar Map.empty >>= return . Sessions

exists :: MonadIO m => Id -> Sessions a -> m Bool
exists id (Sessions m) = liftIO $ readMVar m >>= return . Map.member id

get :: MonadIO m => Id -> Sessions a -> m (Maybe a)
get id (Sessions m) = liftIO $ readMVar m >>= return . Map.lookup id

remove :: MonadIO m => Id -> Sessions a -> m ()
remove id (Sessions m) = liftIO $ modifyMVar_ m $ \map -> return (Map.delete id map)

create :: MonadIO m => a -> Sessions a -> m Id
create a (Sessions m) = liftIO $ modifyMVar m $ \map -> do
    id <- generateSessionId
    return (Map.insert id a map, id)

-- generate a session ID frm alphanumeric chars
generateSessionId :: MonadRandom m => m Id
generateSessionId = do
    bytes <- getRandomBytes 30
    return $ ByteChars.unpack $ Bytes.map toAZ bytes
  where
    toAZ n = let c = n `mod` 62 in
             if c < 10 then c + 48 else if c < 36 then c + (65 - 10) else c + (97 - 36)