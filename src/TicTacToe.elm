module TicTacToe exposing (Cell(..), Game, Player(..), evaluate)

import Array exposing (Array)
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
                |> List.map (\board -> { board = board, player = nextPlayer game.player })
    in
    boards


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
                evaluatedMoves = possibleMoves |> List.map (evaluate (currentDepth + 1))

                scored : Maybe GameState
                scored =
                    case game.player of
                        Self ->
                            evaluatedMoves |> Tools.maxBy .score

                        Opponent ->
                            evaluatedMoves |> Tools.minBy .score

                result =
                    scored |> Maybe.map (\s -> { score = s.score, originalGame = game, nextMove = Just s.originalGame })
            in
            Maybe.withDefault { score = 0, originalGame = game, nextMove = Nothing } result
