module Msg exposing
    ( AppMsg(..)
    , GameMsg(..)
    , JoinGameMsg(..)
    , Msg(..)
    , SiteSelectionMsg(..)
    , SiteVisitMsg(..)
    , WaitMsg(..)
    )

import Api
import BaseType exposing (..)
import Material exposing (Material, Resource)
import Time exposing (Time)


type Msg
    = AppMsg AppMsg


type AppMsg
    = JoinGameMsg JoinGameMsg
    | GameMsg GameMsg
    | ServerMsgReceived (Result String Api.Action)


type JoinGameMsg
    = JoinGameButton
    | GameNameInputChange String


type WaitMsg
    = -- [tmp] unused right now
      Ready Bool
    | NameInputChange String


type GameMsg
    = WaitMsg WaitMsg
    | SiteSelectionMsg SiteSelectionMsg
    | SiteVisitMsg SiteVisitMsg
    | UpdateTimer Time
    | MoveToBasket Resource Int
    | EmptyBasket
    | Shake


type SiteSelectionMsg
    = SiteSelected Site
    | ReadyButton


type SiteVisitMsg
    = OkButton
    | UseResourceButton
    | ActionButton
