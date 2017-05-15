module Types exposing (..)

import Array exposing (Array)
import Time exposing (Time)


type alias Model =
    { rows : Array Row
    , gameRunning : Bool
    }


type alias Row =
    { boxes : Array Box }


type alias Box =
    { alive : Bool }


type Msg
    = Tick Time
    | ToggleBox Int Int
    | ToggleGame
