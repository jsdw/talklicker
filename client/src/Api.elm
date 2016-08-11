module Api exposing (request, Verb(..), Error(..), Path)

import Http
import Json.Encode as JsonEnc exposing (Value, null)
import Json.Decode as JsonDec exposing (Decoder)
import Task exposing (Task)

type Verb = Get | Post | Delete

type Error
    = BasicError Http.RawError
    | DecodeError String
    | ServerError String
    | ClientError String

type alias Path = String

request : Verb -> Path -> Maybe Value -> Decoder res -> Task Error res
request verb path mData decoder =
  let

    headers =
        [ ("Content-Type", "application/json")
        ]

    req = Http.send Http.defaultSettings
        { verb = verbToString verb
        , headers = headers
        , url = path
        , body = reqBody
        }

    reqBody = case mData of
        Nothing -> Http.empty
        Just data -> Http.string (JsonEnc.encode 0 data)

    handleResponse res' =
      let
        status = res'.statusText

        bodyString = case res'.value of
            Http.Text s -> s
            _ -> ""

        handleResponseError res =
            if res.status >= 200 && res.status < 300 then Task.succeed res
            else if res.status >= 400 && res.status < 500 then Task.fail (ClientError status)
            else Task.fail (ServerError status)

        handleJsonDecoding res = case JsonDec.decodeString decoder bodyString of
            Err e -> Task.fail (DecodeError e)
            Ok r -> Task.succeed r

      in
        handleResponseError res' `Task.andThen` handleJsonDecoding

    handleError err =
        Task.fail (BasicError err)

  in
    req `Task.onError` handleError `Task.andThen` handleResponse


verbToString : Verb -> String
verbToString verb = case verb of
    Get -> "GET"
    Post -> "POST"
    Delete -> "DELETE"
