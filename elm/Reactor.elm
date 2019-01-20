module Reactor exposing (main)

import Model exposing (Model)
import Msg
import Update
import View
import Html


hostname : String
hostname =
    "localhost:8080"


main : Program Never Model Msg.Msg
main =
    Html.program
        { init = ( Model.initModel hostname, Cmd.none )
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
