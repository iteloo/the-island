module Main exposing (main)

import Html
import Model exposing (Model)
import Msg
import Update
import View


type alias Flags =
    { hostname : String
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( Model.initModel flags.hostname, Cmd.none )


main : Program Flags Model Msg.Msg
main =
    Html.programWithFlags
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
