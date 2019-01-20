module Server exposing (..)

import WebSocket
import Msg exposing (..)
import Api exposing (..)


type alias SendToServer =
    Api.ServerAction -> Cmd Msg


wsURL : String -> String -> String
wsURL hostname gameName =
    "ws://" ++ hostname ++ "/join?game=" ++ gameName


send :
    { m | hostname : String }
    -> String
    -> Api.ServerAction
    -> Cmd Msg
send { hostname } gameName =
    WebSocket.send (wsURL hostname gameName) << Api.encodeToMessage


listen :
    { m | hostname : String }
    -> String
    -> (Result String Api.Action -> Msg)
    -> Sub Msg
listen { hostname } gameName handler =
    WebSocket.listen (wsURL hostname gameName) (handler << Api.decodeMessage)
