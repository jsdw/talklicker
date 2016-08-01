module Sessions (Sessions, Id, empty, exists, get, remove, create) where

import Prelude hiding (id)
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
exists sessId (Sessions m) = liftIO $ readMVar m >>= return . Map.member sessId

get :: MonadIO m => Id -> Sessions a -> m (Maybe a)
get sessId (Sessions m) = liftIO $ readMVar m >>= return . Map.lookup sessId

remove :: MonadIO m => Id -> Sessions a -> m ()
remove sessId (Sessions m) = liftIO $ modifyMVar_ m $ \sessMap -> return (Map.delete sessId sessMap)

create :: MonadIO m => a -> Sessions a -> m Id
create a (Sessions m) = liftIO $ modifyMVar m $ \sessMap -> do
    sessId <- generateSessionId
    return (Map.insert sessId a sessMap, sessId)

-- generate a session ID frm alphanumeric chars
generateSessionId :: MonadRandom m => m Id
generateSessionId = do
    bytes <- getRandomBytes 30
    return $ ByteChars.unpack $ Bytes.map toAZ bytes
  where
    toAZ n = let c = n `mod` 62 in
             if c < 10 then c + 48 else if c < 36 then c + (65 - 10) else c + (97 - 36)