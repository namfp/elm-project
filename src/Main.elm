module Main exposing (Board, Cell(..), Model, Msg(..), evolve, init, lookForNeighborhoods, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (align, height, style, width)
import Html.Events exposing (onClick)
import Tools


type Cell
    = Dead
    | Alive


type alias Board =
    Array (Array Cell)

evolve : Cell -> List Cell -> Cell
evolve cell neighborhoods =
    let
        size =
            List.length neighborhoods
    in
    case ( cell, size ) of
        ( Alive, x ) ->
            if x < 2 then
                Dead

            else if x == 2 || x == 3 then
                Alive

            else
                Dead

        ( Dead, x ) ->
            if x == 3 then
                Alive

            else
                cell


lookForNeighborhoods : Int -> Int -> Board -> Result String (List Cell)
lookForNeighborhoods x y board =
    let
        positions =
            [ ( x - 1, y - 1 )
            , ( x - 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x + 1, y + 1 )
            ]

        width =
            Array.get 0 board |> Maybe.map Array.length

        height =
            Array.length board

        filter ( i, j ) =
            width
                |> Maybe.map (\widthValue -> i >= 0 && i < widthValue && j >= 0 && j < height)
                |> Maybe.withDefault False

        get ( i, j ) =
            board
                |> Array.get j
                |> Maybe.andThen (Array.get i)
                |> Result.fromMaybe ("Cannot get elements at" ++ Debug.toString i ++ Debug.toString j)

        filteredPositions =
            positions |> List.filter filter |> List.map get
    in
    Tools.multipleResult filteredPositions


step : Board -> Board
step currentBoard =
    let
        zipIndex j line f =
            Array.indexedMap (\i v -> f i j v) line

        boardWithIndex f =
            Array.indexedMap (\j line -> zipIndex j line f) currentBoard

        nextCell i j cell =
            evolve cell (Result.withDefault [] (lookForNeighborhoods i j currentBoard))
    in
    boardWithIndex nextCell


update message board =
    case message of
        NoOp -> board
        ChangeState i j ->
            let
                row = Array.get j board
                currentCell = Maybe.andThen (Array.get i) row
                updatedCell =
                    case currentCell of
                        Just Alive -> Dead
                        Just Dead -> Alive
                        Nothing -> Dead
                updatedRow = Array.set i updatedCell <| Maybe.withDefault Array.empty row
                updatedBoard = Array.set j updatedRow board

            in
            updatedBoard


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Board


init : Model
init =
    Array.repeat 50 (Array.repeat 50 Dead)


-- UPDATE


type Msg
    = NoOp
    | ChangeState Int Int



-- VIEW


renderCell : String -> Int -> Int -> Html Msg
renderCell color i j =
    td
        [ style "backgroundColor" color
        , style "border" "3px solid white"
        , style "font-size" "80px"
        , style "color" "#ffffff"
        , style "border-radius" "2px 2px 2px 2px"
        , height 20
        , width 20
        , align "center"
        , style "valign" "center"
        , onClick (ChangeState i j)
        ]
        []


renderTable : Board -> Html Msg
renderTable board =
    let
        renderLine j line =
            line
                |> Array.indexedMap
                    (\i v ->
                        case v of
                            Dead ->
                                renderCell "black" i j

                            Alive ->
                                renderCell "white" i j
                    )
                |> Array.toList
                |> tr []

        renderBoard =
            board
                |> Array.indexedMap
                    (\j line ->
                        renderLine j line
                    )
                |> Array.toList
                |> table
                    [
                        style "border-collapse" "collapse",
                        style "border" "1",
                        style "cellspacing" "0"
                    ]
    in
        renderBoard


view : Model -> Html Msg
view model =
    renderTable model
