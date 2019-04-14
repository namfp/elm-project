module Main exposing (Board, Cell(..), Model, Msg(..), evolve, init, lookForNeighborhoods, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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
            Array.get 0 board  |> Maybe.map Array.length

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
    filteredPositions


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
