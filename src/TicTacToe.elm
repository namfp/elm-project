module TicTacToe exposing (Cell(..), Game, Player(..))

import Array exposing (Array)


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


allPossibleMoves :





evaluate : Game -> Int
evaluate game =
    case findWinner game of
        Just Self -> 1
        Just Opponent -> -1
        Nothing ->
            let
                possibleMoves: List Game
                possibleMoves = computeAllPossibleMoves game
                scored =
                    case .player of
                        Self -> possibleMoves |> List.map evaluate |> List.maximum
                        Opponent -> possibleMoves |> List.map evaluate |> List.minimum

            in
            scored
