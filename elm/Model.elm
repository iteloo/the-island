module Model exposing
    ( AppModel(..)
    , Auction
    , AuctionModel
    , Bid
    , GameModel
    , Model
    , ReadyModel
    , Stage(..)
    , TradeModel
    , WelcomeModel
    , initAppModel
    , initAuctionModel
    , initGameModel
    , initModel
    , initReadyModel
    , initTradeModel
    , initWelcomeModel
    , timer
    )

import BaseType exposing (..)
import Card exposing (Card)
import Lens exposing (PureLens)
import Material exposing (Fruit, Material)
import Time exposing (Time)
import Timer exposing (Timer)
import ZoomList exposing (ZoomList)


type alias Model =
    { hostname : String
    , app : AppModel
    }


type AppModel
    = WelcomeScreen WelcomeModel
    | Game GameModel


type alias WelcomeModel =
    { gameNameInput : String

    {- [tmp] [hack] right now this is needed
       because we can only listen to ws
       once we know the name, otherwise
       the server will add us to a random
       game

       [note] we aren't using a bool, in case
       input changes while waiting for server
       response (though unlikely)
    -}
    , submittedName : Maybe String
    }


type alias GameModel =
    { gameName : String
    , stage : Stage
    , name : String
    , gold : Int
    , inventory : Material Int
    , factories : List Factory
    , effects : List Effect
    , cards : ZoomList Card
    }


type Stage
    = ReadyStage ReadyModel
    | AuctionStage AuctionModel
    | TradeStage TradeModel


type alias ReadyModel =
    { ready : Bool
    , playerInfo : List PlayerInfo
    }


type alias AuctionModel =
    { auction : Maybe Auction }


type alias Auction =
    { card : Card
    , highestBid : Maybe Bid
    , timer : Timer
    }


type alias Bid =
    { bidder : String
    , bid : Int
    }


type alias TradeModel =
    { basket : Material Int
    , timer : Timer
    }


type alias Factory =
    { name : String
    , fruit : Fruit
    , number : Int
    }


type alias Effect =
    { name : String
    , author : String
    , yieldRateModifier : Material Float
    , roundsLeft : Int
    }


initModel : String -> Model
initModel hostname =
    { hostname = hostname
    , app = initAppModel
    }


initAppModel : AppModel
initAppModel =
    WelcomeScreen initWelcomeModel


initWelcomeModel : WelcomeModel
initWelcomeModel =
    { gameNameInput = ""
    , submittedName = Nothing
    }


initGameModel : String -> GameModel
initGameModel name =
    { gameName = name
    , stage = ReadyStage initReadyModel
    , name = "Anonymous"
    , gold = 25
    , inventory = Material.empty
    , factories = []

    -- [note] should perhaps use Maybe since
    -- we are using this to represent server pushed vallue
    , effects = []
    , cards = ZoomList.empty
    }


initReadyModel : ReadyModel
initReadyModel =
    { ready = False
    , playerInfo = []
    }


initTradeModel : TradeModel
initTradeModel =
    { basket = Material.empty
    , timer = Timer.init (10 * Time.second)
    }


initAuctionModel : AuctionModel
initAuctionModel =
    { auction = Nothing }



-- GETTER & SETTERS


timer : PureLens Timer Stage
timer =
    let
        get stage =
            case stage of
                ReadyStage _ ->
                    Nothing

                AuctionStage m ->
                    m.auction |> Maybe.map .timer

                TradeStage m ->
                    Just m.timer

        set timer stage =
            case stage of
                ReadyStage _ ->
                    Nothing

                TradeStage m ->
                    Just <| TradeStage { m | timer = timer }

                AuctionStage m ->
                    Just <|
                        AuctionStage
                            { m
                                | auction =
                                    Maybe.map
                                        (\a -> { a | timer = timer })
                                        m.auction
                            }
    in
    { get = get, set = set }
