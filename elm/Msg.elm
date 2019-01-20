module Msg exposing (AppMsg(..), AuctionMsg(..), GameMsg(..), Msg(..), ProductionMsg(..), ReadyMsg(..), TradeMsg(..), WelcomeMsg(..))

import Api
import Card exposing (Card)
import Material exposing (Fruit, Material)
import Time exposing (Time)
import ZoomList exposing (ZoomList)


type Msg
    = AppMsg AppMsg


type AppMsg
    = WelcomeMsg WelcomeMsg
    | GameMsg GameMsg
    | ServerMsgReceived (Result String Api.Action)


type WelcomeMsg
    = JoinGameButton
    | GameNameInputChange String


type GameMsg
    = ReadyMsg ReadyMsg
    | AuctionMsg AuctionMsg
    | TradeMsg TradeMsg
    | ActivateButton
    | UpdateTimer Time
    | UpdateCards (ZoomList Card)
    | DismissCardDetailView


type ReadyMsg
    = -- [tmp] unused right now
      Ready Bool
    | NameInputChange String


type TradeMsg
    = Yield
    | MoveToBasket Fruit Int
    | EmptyBasket
    | Shake
    | YieldRoll (Material Int)


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = BidButton
    | ClockUpdated Int
