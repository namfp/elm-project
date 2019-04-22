module TicTacToeTests exposing (suite)

import Array
import Expect
import Test exposing (..)
import TicTacToe exposing (GameResult(..), Player(..))


suite : Test
suite =
    let
        oO =
            TicTacToe.Played TicTacToe.Opponent

        xX =
            TicTacToe.Played TicTacToe.Self

        ii =
            TicTacToe.NotPlayed
    in
    describe "TicTacToe"
        [ let
            board =
                Array.fromList
                    [ Array.fromList [ xX, xX, xX ]
                    , Array.fromList [ oO, oO, ii ]
                    , Array.fromList [ ii, oO, ii ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            reference =
                { score = 1, originalGame = game, nextMove = Nothing }

            nextBoard =
                TicTacToe.evaluate 0 game
          in
          test "should find self winner" (\_ -> Expect.equal nextBoard reference)
        , let
            board =
                Array.fromList
                    [ Array.fromList [ xX, xX, oO ]
                    , Array.fromList [ oO, oO, oO ]
                    , Array.fromList [ ii, oO, ii ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            reference =
                { score = -1, originalGame = game, nextMove = Nothing }

            nextBoard =
                board
                    |> (\x -> { board = x, player = TicTacToe.Self, gameResult = Playing })
                    |> TicTacToe.evaluate 0
          in
          test "should find self winner with opponent" (\_ -> Expect.equal nextBoard reference)
        , let
            board =
                Array.fromList
                    [ Array.fromList [ xX, xX, oO ]
                    , Array.fromList [ oO, xX, oO ]
                    , Array.fromList [ oO, oO, xX ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            reference =
                { score = 1, originalGame = game, nextMove = Nothing }

            nextBoard =
                board
                    |> (\x -> { board = x, player = TicTacToe.Self, gameResult = Playing })
                    |> TicTacToe.evaluate 0
          in
          test "should find self winner with diagonal" (\_ -> Expect.equal nextBoard reference)
        , let
            board =
                Array.fromList
                    [ Array.fromList [ xX, xX, ii ]
                    , Array.fromList [ oO, xX, oO ]
                    , Array.fromList [ ii, oO, oO ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            nextGame =
                TicTacToe.evaluate 0 game

            referenceBoard =
                Array.fromList
                    [ Array.fromList [ xX, xX, xX ]
                    , Array.fromList [ oO, xX, oO ]
                    , Array.fromList [ ii, oO, oO ]
                    ]
          in
          test "should find one correct move for SELF"
            (\_ ->
                Expect.equal nextGame
                    { score = 1, originalGame = game, nextMove = Just { board = referenceBoard, player = TicTacToe.Opponent, gameResult = Playing } }
            )
        , let
            board =
                Array.fromList
                    [ Array.fromList [ xX, xX, oO ]
                    , Array.fromList [ oO, xX, oO ]
                    , Array.fromList [ ii, oO, ii ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            nextGame =
                TicTacToe.evaluate 0 game

            referenceBoard =
                Array.fromList
                    [ Array.fromList [ xX, xX, oO ]
                    , Array.fromList [ oO, xX, oO ]
                    , Array.fromList [ ii, oO, xX ]
                    ]
          in
          test "should find one correct move for diagonal"
            (\_ ->
                Expect.equal nextGame
                    { score = 1
                    , originalGame = game
                    , nextMove = Just { board = referenceBoard, player = TicTacToe.Opponent, gameResult = Playing }
                    }
            )
        , let
            board =
                Array.fromList
                    [ Array.fromList [ oO, xX, ii ]
                    , Array.fromList [ xX, oO, ii ]
                    , Array.fromList [ ii, ii, ii ]
                    ]

            game =
                { board = board, player = TicTacToe.Self, gameResult = Playing }

            nextGame =
                TicTacToe.evaluate 0 game

            referenceBoard =
                Array.fromList
                    [ Array.fromList [ oO, xX, ii ]
                    , Array.fromList [ xX, oO, ii ]
                    , Array.fromList [ ii, ii, xX ]
                    ]
          in
          test "should avoid loss"
            (\_ ->
                Expect.equal
                    nextGame
                    { score = 0
                    , originalGame = game
                    , nextMove = Just { board = referenceBoard, player = TicTacToe.Opponent, gameResult = Playing }
                    }
            )
        ]
