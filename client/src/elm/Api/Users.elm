module Api.Users exposing (login, LoginError(..), logout, get, set, current, User, UserType(..))

import Json.Encode as Enc exposing (Value, null)
import Json.Decode as Dec exposing (Decoder, (:=))
import Task exposing (Task)
import String
import Dict exposing (Dict)

import Api exposing (..)

--
-- auth:
--
login : String -> String -> Task LoginError User
login name pass =
  let
    value = Enc.object
        [ ("name", Enc.string name)
        , ("pass", Enc.string pass) ]
  in
    request Post ("core" :> "login") (Just value) userDecoder
        `Task.onError` (\err -> case err of
            ClientError 401 "BAD_USER" -> Task.fail LoginBadUser
            ClientError 401 "BAD_PASSWORD" -> Task.fail LoginBadPassword
            err -> Task.fail (LoginOther err))

logout : Task Error ()
logout = request Post ("core" :> "logout") Nothing noResult

type LoginError = LoginBadUser | LoginBadPassword | LoginOther Error

--
-- Get current user (Nothing if not logged in, Just User if we are)
--

current : Task Error (Maybe User)
current = request Get ("users" :> "current") Nothing userDecoder
    `Task.andThen` (\u -> Task.succeed (Just u))
    `Task.onError` (\err -> case err of
        ClientError 401 _ -> Task.succeed Nothing
        err -> Task.fail err)

--
-- Get users:
--

get : Task Error (Dict String User)
get =
  let
    toDict = \us -> List.foldl (\u d -> Dict.insert u.name u d) Dict.empty us
  in
    Task.map toDict (request Get "users" Nothing usersDecoder)

type alias User =
    { name : String
    , fullName : String
    , userType : UserType
    }

type UserType = Admin | NormalUser

usersDecoder : Decoder (List User)
usersDecoder = Dec.list userDecoder

userDecoder : Decoder User
userDecoder = Dec.object3 User
    ("name" := Dec.string)
    ("fullName" := Dec.string)
    ("type" := userTypeDecoder)

userTypeDecoder : Decoder UserType
userTypeDecoder = Dec.customDecoder Dec.string toUserType

toUserType : String -> Result String UserType
toUserType str =
    case String.toLower str of
        "admin" -> Ok Admin
        "normaluser" -> Ok NormalUser
        _ -> Err (str++" is not a valid UserType")

--
-- Set user
--

set : String -> { a | fullName : String, pass : Maybe String, userType : String } -> Task Error User
set username details =
  let
    value = Enc.object
        [ ("fullName", Enc.string details.fullName)
        , ("pass", Enc.string ?= details.pass)
        , ("type", Enc.string details.userType) ]
  in
    request Post ("users" :> username) (Just value) userDecoder

(?=) : (a -> Value)-> Maybe a -> Value
(?=) enc a = case a of
    Nothing -> Enc.null
    Just v -> enc v
