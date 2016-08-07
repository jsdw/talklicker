module Id (generate) where

import Crypto.Random (getRandomBytes)
import Control.Monad.Trans (liftIO, MonadIO)

import qualified Data.ByteString.Char8 as ByteChars
import qualified Data.ByteString as Bytes

-- generate an ID from alphanumeric chars
generate :: MonadIO m => m String
generate = liftIO $ do
    bytes <- getRandomBytes 30
    return $ ByteChars.unpack $ Bytes.map toAZ bytes
  where
    toAZ n = let c = n `mod` 62 in
             if c < 10 then c + 48 else if c < 36 then c + (65 - 10) else c + (97 - 36)