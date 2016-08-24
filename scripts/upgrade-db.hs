#!/usr/bin/env stack
-- stack --resolver lts-6.12 --install-ghc runghc --package lens-aeson --package lens

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import qualified Data.ByteString.Lazy as Bytes
import qualified Data.Map as Map
import qualified Control.Monad.State as State

import System.IO (stdout)
import Control.Monad.State (State)
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Aeson (Value(..), decode, encode)
import System.Environment (getArgs)
import Data.Foldable (foldl')
import Control.Lens
import Data.Aeson.Lens

--
-- Provide a version and a transformation to apply to the JSON:
--
upgraders = do

    -- EXAMPLE: set "email" of each user to blank string and "subscribed" of each user to false
    0 ==> set (key "users" . _Array . each . _Object . at "email") (Just $ String "")
        . set (key "users" . _Array . each . _Object . at "subscribed") (Just $ Bool False)

    -- EXAMPLE: update "subscribed" of each user to true (key must exist to be updated)
    1 ==> set (key "users" . _Array . each . key "subscribed") (Bool True)

    -- EXAMPLE: rename "fullName" to "description" for each user (add description then remove fullName)
    2 ==> set (key "users" . _Array . each . _Object . at "fullName") Nothing
        . over (key "users" . _Array . each . _Object) (\u -> set (at "description") (preview (ix "fullName") u) u)

--
-- Load our upgraders, running those that are necessary given the version:
--
main = do

    fileName <- getArgs >>= \a -> case a of
        [f] -> return f
        _ -> error "Exactly one argument expected; the JSON file to upgrade"

    (initialJson :: Value) <- (decode <$> Bytes.readFile fileName) >>= \r -> case r of
        Just json -> return json
        Nothing -> error "Failed to decode JSON in file"

    let Just initialVersion = preview (key "schemaVersion" . _Integral) initialJson <|> Just (-1)
        finalJson = foldl' applyJson initialJson (getUpgraders upgraders)
        applyJson json (version,fn) =
            if initialVersion >= version
            then json
            else set (_Object . at "schemaVersion") (Just $ Number $ fromIntegral version) (fn json)

    Bytes.hPut stdout (encode finalJson)

getUpgraders :: Changes () -> [(Int, Value -> Value)]
getUpgraders upgraders = Map.toAscList $ snd $ State.execState upgraders (0, Map.empty)

infixl 0 ==>
(==>) :: Int -> (Value -> Value) -> Changes ()
(==>) version fn = State.modify $ \(lastV, map) ->
    if version < lastV then error ("Versions specified out of order ("++show version++" after "++show lastV++")")
    else if Map.member version map then error ("Version "++show version++" listed more than once")
    else (version, Map.insert version (\json -> fn json) map)

type Changes = State (Int, Map Int (Value -> Value))
