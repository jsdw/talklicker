module Args (parsed) where

import Data.Map (Map)
import System.Environment (getArgs)
import Data.Monoid ((<>))

import qualified Data.Map as Map

parsed :: IO (Map String String)
parsed = parse <$> getArgs

parse :: [String] -> Map String String
parse (('-' : '-' : key) : rest) = parse (('-' : key) : rest)
parse (('-' : key) : val : rest) = Map.singleton key val <> parse rest
parse _ = Map.empty

