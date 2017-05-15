module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Array


view : Model -> Html Msg
view model =
    div []
        ((Array.toList (Array.indexedMap renderRow model.rows))
            ++ [ button [ onClick ToggleGame ] [ text "Toggle Game" ] ]
        )


renderRow : Int -> Row -> Html Msg
renderRow index row =
    div [ class "row" ] (Array.toList (Array.indexedMap (renderBox index) row.boxes))


renderBox : Int -> Int -> Box -> Html Msg
renderBox row index box =
    div
        [ classList
            [ ( "box", True )
            , ( "box--alive", box.alive )
            ]
        , onClick (ToggleBox row index)
        ]
        []
