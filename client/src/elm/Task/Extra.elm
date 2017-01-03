module Task.Extra exposing (perform)

import Task

perform : (x -> msg) -> (a -> msg) -> Task.Task x a -> Cmd msg
perform fail succ task = task |> Task.attempt (\res -> case res of
    Ok a -> succ a
    Err x -> fail x)