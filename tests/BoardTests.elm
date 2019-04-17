module BoardTests exposing(..)
import Array
import GameOfLife exposing (Cell(..))
import Test exposing (..)
import Expect

suite : Test
suite =
    let
        board = Array.fromList
                    [
                        Array.fromList [Dead, Dead, Dead, Dead],
                        Array.fromList [Dead, Alive, Alive, Dead],
                        Array.fromList [Dead, Alive, Alive, Dead],
                        Array.fromList [Dead, Dead, Dead, Dead]
                    ]
        updatedBoard = GameOfLife.step board
    in
        test "step: first case" (\_ -> Expect.equal updatedBoard board)

