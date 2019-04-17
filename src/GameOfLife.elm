module GameOfLife exposing (Board, Cell(..), Model, Msg(..), evolve, init, lookForNeighborhoods, main, step, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (align, height, style, width)
import Html.Events exposing (onClick)
import Time
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
            List.length (List.filter (\x -> x == Alive) neighborhoods)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            case model.state of
                Started ->
                    ( { model | board = step model.board }, Cmd.none )

                Stopped ->
                    ( model, Cmd.none )

        ChangeState i j ->
            let
                row =
                    Array.get j model.board

                currentCell =
                    Maybe.andThen (Array.get i) row

                updatedCell =
                    case currentCell of
                        Just Alive ->
                            Dead

                        Just Dead ->
                            Alive

                        Nothing ->
                            Dead

                updatedRow =
                    Array.set i updatedCell <| Maybe.withDefault Array.empty row

                updatedBoard =
                    Array.set j updatedRow model.board

                updatedModel =
                    { model | board = updatedBoard }
            in
            ( updatedModel, Cmd.none )

        StartStop ->
            case model.state of
                Stopped ->
                    ( { model | state = Started }, Cmd.none )

                Started ->
                    ( { model | state = Stopped }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Stopped ->
            Sub.none

        Started ->
            Time.every 50 (\_ -> Tick)


main =
    Browser.element { init = \() -> ( init, Cmd.none ), view = view, update = update, subscriptions = subscriptions }



-- MODEL


type GameState
    = Stopped
    | Started


type alias Model =
    { state : GameState, board : Board }


init : Model
init =
    { state = Stopped
    , board = Array.repeat 10 (Array.repeat 10 Dead)
    }



-- UPDATE


type Msg
    = NoOp
    | ChangeState Int Int
    | Tick
    | StartStop



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
                    [ style "border-collapse" "collapse"
                    , style "border" "1"
                    , style "cellspacing" "0"
                    ]
    in
    renderBoard


view : Model -> Html Msg
view model =
    let
        buttonText =
            case model.state of
                Stopped ->
                    "Run"

                Started ->
                    "Stop"
    in
    div []
        [ renderTable model.board
        , button [ onClick StartStop ] [ text buttonText ]
        , text (Debug.toString model.state)
        ]
