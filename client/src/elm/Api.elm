module Api exposing (request, Verb(..), Error(..), Path, noResult, (:>))

import Http
import Json.Encode as JsonEnc exposing (Value, null)
import Json.Decode as JsonDec exposing (Decoder)
import Task exposing (Task)


(:>) : String -> String -> String
(:>) a b =
    a ++ "/" ++ b


noResult : Decoder ()
noResult =
    (JsonDec.succeed ())


type Verb
    = Get
    | Post
    | Delete


type Error
    = DecodeError String
    | ServerError Int String
    | ClientError Int String
    | BadNetworkError
    | OtherError String


type alias Path =
    String


request : Verb -> Path -> Maybe Value -> Decoder res -> Task Error res
request verb path mData decoder =
    let
        req =
            Http.toTask (Http.request
                { method = verbToString verb
                , headers = []
                , url = path
                , body = reqBody
                , expect = Http.expectJson decoder
                , timeout = Nothing
                , withCredentials = False })

        reqBody =
            case mData of
                Nothing ->
                    Http.emptyBody

                Just data ->
                    Http.jsonBody data

        handleError err =
            case err of
                Http.BadPayload err _ ->
                    Task.fail (DecodeError err)

                Http.BadStatus res ->
                    if res.status.code >= 400 && res.status.code < 500 then
                        Task.fail (ClientError res.status.code res.status.message)
                    else
                        Task.fail (ServerError res.status.code res.status.message)

                Http.BadUrl url ->
                    Task.fail (OtherError ("invalid url: "++url))

                Http.Timeout ->
                    Task.fail BadNetworkError

                Http.NetworkError ->
                    Task.fail BadNetworkError

    in
        req |> Task.onError handleError


verbToString : Verb -> String
verbToString verb =
    case verb of
        Get ->
            "GET"

        Post ->
            "POST"

        Delete ->
            "DELETE"
