module TicTacToe exposing (Cell(..), Game, GameResult(..), Player(..), evaluate)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Tools


type Cell
    = Played Player
    | NotPlayed


type Player
    = Self
    | Opponent


type alias Game =
    { board : Array (Array Cell)
    , player : Player
    , gameResult : GameResult
    }


type alias Move =
    { x : Int, y : Int }


findWinner : Game -> Maybe Player
findWinner game =
    let
        winningPositions =
            [ [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
            , [ ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) ]
            , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
            , [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
            , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
            , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
            ]

        checkPosition positions player =
            positions
                |> List.all
                    (\position ->
                        case position of
                            ( i, j ) ->
                                game.board
                                    |> Array.get j
                                    |> Maybe.andThen (Array.get i)
                                    |> Maybe.map
                                        (\x ->
                                            case x of
                                                Played p ->
                                                    p == player

                                                NotPlayed ->
                                                    False
                                        )
                                    |> Maybe.withDefault False
                    )

        isWinner player =
            winningPositions |> List.any (\positions -> checkPosition positions player)

        winner =
            if isWinner Self then
                Just Self

            else if isWinner Opponent then
                Just Opponent

            else
                Nothing
    in
    winner


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Self ->
            Opponent

        Opponent ->
            Self


allPossibleMoves : Game -> List Game
allPossibleMoves game =
    let
        allEmptyPositions : List ( Int, Int )
        allEmptyPositions =
            game.board
                |> Array.indexedMap
                    (\j v ->
                        Array.indexedMap
                            (\i c ->
                                case c of
                                    NotPlayed ->
                                        Just ( i, j )

                                    _ ->
                                        Nothing
                            )
                            v
                            |> Array.toList
                            |> Tools.flatten
                    )
                |> Array.foldl List.append []

        boards =
            allEmptyPositions
                |> List.map
                    (\x ->
                        case x of
                            ( i, j ) ->
                                let
                                    currentLine =
                                        Array.get j game.board

                                    updatedLine =
                                        currentLine |> Maybe.map (\l -> Array.set i (Played game.player) l)
                                in
                                updatedLine |> Maybe.map (\l -> Array.set j l game.board)
                    )
                |> Tools.flatten
                |> List.map (\board -> { board = board, player = nextPlayer game.player, gameResult = Playing })
    in
    boards


type GameResult
    = Playing
    | Winner Player
    | Draw


renderResult : GameResult -> String
renderResult gameResult =
    case gameResult of
        Playing ->
            "Playing"

        Winner player ->
            "Player " ++ Debug.toString player ++ " won"

        Draw ->
            "Draw"


type alias GameState =
    { score : Int
    , originalGame : Game
    , nextMove : Maybe Game
    }


evaluate : Int -> Game -> GameState
evaluate currentDepth game =
    case findWinner game of
        Just Self ->
            { score = 1, originalGame = game, nextMove = Nothing }

        Just Opponent ->
            { score = -1, originalGame = game, nextMove = Nothing }

        Nothing ->
            let
                possibleMoves : List Game
                possibleMoves =
                    allPossibleMoves game

                evaluatedMoves =
                    possibleMoves |> List.map (evaluate (currentDepth + 1))

                scored : Maybe GameState
                scored =
                    case game.player of
                        Self ->
                            evaluatedMoves |> Tools.maxBy .score

                        Opponent ->
                            evaluatedMoves |> Tools.minBy .score

                result =
                    scored
                        |> Maybe.map
                            (\s ->
                                { score = s.score, originalGame = game, nextMove = Just s.originalGame }
                            )
            in
            Maybe.withDefault { score = 0, originalGame = game, nextMove = Nothing } result



--Message


type Msg
    = Play Int Int
    | PlaySelf



-- View


renderCell mark i j =
    td
        [ style "backgroundColor" "white"
        , style "border" "3px solid black"
        , style "font-size" "100px"
        , style "color" "black"
        , style "border-radius" "2px 2px 2px 2px"
        , height 150
        , width 150
        , align "center"
        , style "valign" "center"
        , onClick (Play i j)
        ]
        [ text mark ]


renderGame : Array (Array Cell) -> Html Msg
renderGame board =
    let
        renderLine j line =
            line
                |> Array.indexedMap
                    (\i v ->
                        case v of
                            Played Self ->
                                renderCell "X" i j

                            Played Opponent ->
                                renderCell "O" i j

                            NotPlayed ->
                                renderCell "" i j
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


view : Game -> Html Msg
view game =
    div []
        [ renderGame game.board
        , text (renderResult game.gameResult)
        ]



--Update


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case ( msg, game ) of
        ( Play i j, { board, player, gameResult } ) ->
            if gameResult == Playing && player == Opponent then
                let
                    currentLine =
                        Array.get j game.board

                    updatedCell : Maybe Cell
                    updatedCell =
                        currentLine
                            |> Maybe.andThen (Array.get i)
                            |> Maybe.map
                                (\cell ->
                                    case cell of
                                        NotPlayed ->
                                            Played Opponent

                                        current ->
                                            current
                                )

                    ( updatedBoard, updatedPlayer ) =
                        case ( currentLine, updatedCell ) of
                            ( Just line, Just c ) ->
                                ( Array.set j (Array.set i c line) board, nextPlayer player )

                            _ ->
                                ( board, player )

                    updatedGame =
                        { game | board = updatedBoard, player = updatedPlayer }
                in
                ( updatedGame, Task.perform (\_ -> PlaySelf) Time.now )

            else
                ( game, Cmd.none )

        ( PlaySelf, _ ) ->
            let
                gameComputed =
                    evaluate 0 game

                nextResult =
                    gameComputed.nextMove
                        |> Maybe.andThen findWinner
                        |> Maybe.map Winner
                        |> Maybe.withDefault Playing

                nextMove =
                    gameComputed.nextMove
                        |> Maybe.map (\m -> { m | gameResult = nextResult })

                next =
                    Maybe.withDefault { game | gameResult = nextResult } nextMove
            in
            ( next, Cmd.none )



-- Main


initGame =
    { board =
        Array.fromList
            [ Array.fromList [ NotPlayed, NotPlayed, NotPlayed ]
            , Array.fromList [ NotPlayed, NotPlayed, NotPlayed ]
            , Array.fromList [ NotPlayed, NotPlayed, NotPlayed ]
            ]
    , player = Opponent
    , gameResult = Playing
    }


subscriptions : Game -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Browser.element { init = \() -> ( initGame, Cmd.none ), view = view, update = update, subscriptions = subscriptions }
