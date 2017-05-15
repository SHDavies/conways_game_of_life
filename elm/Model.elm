module Model exposing (..)

import Types exposing (..)
import Array


emptyRow : Row
emptyRow =
    { boxes = (Array.repeat 100 (Box False)) }


init : ( Model, Cmd Msg )
init =
    { rows = (Array.repeat 60 emptyRow)
    , gameRunning = False
    }
        ! []
