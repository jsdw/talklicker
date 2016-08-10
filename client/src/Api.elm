module Api exposing (init, request, Verb(..), Api, Path)

import Http
import Json.Encode as JsonEnc exposing (Value)
import Json.Decode as JsonDec
import Task exposing (Task)
import String
import Dict

--
-- request : Verb -> Path -> Json.Value -> Decoder resp -> Result Err resp
--

type Verb = Get | Post

type Error
    = BasicError Http.RawError
    | DecodeError String
    | ServerError String
    | ClientError String

type alias Path = String

type Api = Api String

init : Api
init = Api ""

request : Api -> Verb -> Path -> Value -> JsonDec.Decoder res -> Task Error (res, Api)
request (Api sessId) verb path data decoder =
  let

    headers =
        [ ("Content-Type", "application/json")
        ] ++ if String.isEmpty sessId then [(talklickerHeader, sessId)] else []

    req = Http.send Http.defaultSettings
        { verb = verbToString verb
        , headers = headers
        , url = path
        , body = Http.string (JsonEnc.encode 0 data)
        }

    handleResponse res' =
      let
        status = res'.statusText

        bodyString = case res'.value of
            Http.Text s -> s
            _ -> ""

        newApi = case Dict.get talklickerHeader res'.headers of
            Nothing -> Api sessId
            Just newSessId -> Api newSessId

        handleResponseError res =
            if res.status >= 200 && res.status < 300 then Task.succeed res
            else if res.status >= 400 && res.status < 500 then Task.fail (ClientError status)
            else Task.fail (ServerError status)

        handleJsonDecoding res = case JsonDec.decodeString decoder bodyString of
            Err e -> Task.fail (DecodeError e)
            Ok r -> Task.succeed (r, newApi)

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

talklickerHeader : String
talklickerHeader = "Talklicker-Session"