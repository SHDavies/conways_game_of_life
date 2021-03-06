module Main exposing (..)

import Html
import Model exposing (..)
import View exposing (..)
import Update exposing (..)
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
