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

    0 =>> id

    1 =>> (\json -> json)

    2 =>> id

    4 =>> id

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
    return ()

getUpgraders :: Changes () -> [(Int, Value -> Value)]
getUpgraders upgraders =
  let
    (_, upgradersMap) = State.execState upgraders (0, Map.empty)
  in
    Map.toAscList upgradersMap

(=>>) :: Int -> (Value -> Value) -> Changes ()
(=>>) version fn = State.modify $ \(lastV, map) ->
    if version < lastV then error ("Versions specified out of order ("++show version++" after "++show lastV++")")
    else if Map.member version map then error ("Version "++show version++" listed more than once")
    else (version, Map.insert version (\json -> fn json) map)

type Changes = State (Int, Map Int (Value -> Value))
