module Update exposing (..)

import Types exposing (..)
import Array
import Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                checkBox : Int -> Int -> Box -> Box
                checkBox row box boxObj =
                    let
                        mThisRow =
                            Array.get row model.rows

                        mAboveRow =
                            Array.get (row - 1) model.rows

                        mBelowRow =
                            Array.get (row + 1) model.rows

                        thisBox =
                            mThisRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get box)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        aboveLeftBox =
                            mAboveRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box - 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        aboveBox =
                            mAboveRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get box)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        aboveRightBox =
                            mAboveRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box + 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        leftBox =
                            mThisRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box - 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        rightBox =
                            mThisRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box + 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        belowLeftBox =
                            mBelowRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box - 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        belowBox =
                            mBelowRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get box)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        belowRightBox =
                            mBelowRow
                                |> Maybe.map .boxes
                                |> Maybe.andThen (Array.get <| box + 1)
                                |> Maybe.withDefault (Box False)
                                |> .alive

                        allNeighbors =
                            [ aboveLeftBox, aboveBox, aboveRightBox, leftBox, rightBox, belowLeftBox, belowBox, belowRightBox ]

                        numOfAliveNeighbors =
                            List.foldr
                                (\isAlive count_ ->
                                    if isAlive then
                                        (count_ + 1)
                                    else
                                        count_
                                )
                                0
                                allNeighbors
                    in
                        if boxObj.alive then
                            if numOfAliveNeighbors < 2 then
                                { boxObj | alive = False }
                            else if numOfAliveNeighbors > 3 then
                                { boxObj | alive = False }
                            else
                                boxObj
                        else if numOfAliveNeighbors == 3 then
                            { boxObj | alive = True }
                        else
                            boxObj

                mapOverRow : Int -> Row -> Row
                mapOverRow index row_ =
                    let
                        updatedBoxes =
                            Array.indexedMap (checkBox index) row_.boxes
                    in
                        { row_ | boxes = updatedBoxes }

                updatedGame =
                    Array.indexedMap mapOverRow model.rows
            in
                { model | rows = updatedGame } ! []

        ToggleBox row box ->
            if not model.gameRunning then
                let
                    mRow =
                        Array.get row model.rows

                    box_ =
                        mRow
                            |> Maybe.map .boxes
                            |> Maybe.andThen (Array.get box)
                            |> Maybe.map (\b -> { b | alive = (not b.alive) })
                            |> Maybe.withDefault (Box False)

                    mUpdatedBoxes =
                        mRow
                            |> Maybe.map .boxes
                            |> Maybe.map (Array.set box box_)

                    mUpdatedRow =
                        case mUpdatedBoxes of
                            Just boxes ->
                                Maybe.map (\row -> { row | boxes = boxes }) mRow

                            Nothing ->
                                mRow

                    updatedGame =
                        case mUpdatedRow of
                            Just updatedRow ->
                                Array.set row updatedRow model.rows

                            Nothing ->
                                model.rows
                in
                    { model | rows = updatedGame } ! []
            else
                model ! []

        ToggleGame ->
            { model | gameRunning = (not model.gameRunning) } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameRunning then
        Time.every (Time.millisecond * 100) Tick
    else
        Sub.none
