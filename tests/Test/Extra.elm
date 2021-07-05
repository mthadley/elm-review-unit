module Test.Extra exposing (createElmJson)

import Elm.Project exposing (Project)
import Json.Decode as Decode


createElmJson : String -> Project
createElmJson =
    assertOk << Decode.decodeString Elm.Project.decoder


assertOk : Result e a -> a
assertOk result =
    case result of
        Ok a ->
            a

        Err _ ->
            Debug.todo "Encountered an Err, but expected an Ok"
