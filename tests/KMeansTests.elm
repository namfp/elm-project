module KMeansTests exposing (suite)

import Expect
import KMeans
import Test exposing (..)


suite : Test
suite =
    describe "KMeans module"
        [ let
            result =
                KMeans.run [ { x = 1, y = 1 } ] [ { x = 1, y = 1 } ]
          in
          test "run" (\_ -> Expect.equal result [ { x = 1, y = 1 } ])
        , let
            result =
                KMeans.step [ { x = 1, y = 1 } ] [ { x = 1, y = 1 } ]
          in
          test "step" (\_ -> Expect.equal result [ { x = 1, y = 1 } ])
        , let
            result =
                KMeans.computeCentroid [ { x = 1, y = 1 } ]
          in
          test "computeCentroids" (\_ -> Expect.equal result { x = 1, y = 1 })
        ]
