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
          test "run 1" (\_ -> Expect.equal result [ { x = 1, y = 1 } ])

        ,  let
              result =
                  KMeans.run [ { x = 1, y = 1 },  { x = 3, y = 3 }] [ { x = 1, y = 1 } ]
            in
            test "run 2" (\_ -> Expect.equal result [ { x = 2, y = 2 } ])
        , let
            result =
                KMeans.step [ { x = 1, y = 1 } ] [ { x = 1, y = 1 } ]
          in
          test "step" (\_ -> Expect.equal result [ { x = 1, y = 1 } ])
        , let
            result =
                KMeans.computeCentroid [ { x = 1, y = 1 } ]
          in
          test "computeCentroids: 1" (\_ -> Expect.equal result { x = 1, y = 1 })
        , let
            result =
                KMeans.computeCentroid [ { x = 1, y = 1 }, { x = 3, y = 3 } ]
          in
          test "computeCentroids: 2" (\_ -> Expect.equal result { x = 2, y = 2 })
        , let
            result =
                KMeans.chooseCentroid { x = 0, y = 0 } [ { x = 1, y = 1 }, { x = 3, y = 3 } ]
          in
          test "chooseCentroids" (\_ -> Expect.equal result (Just { x = 1, y = 1 }))
        , let
            point1 =
                { x = 1, y = 1 }

            point3 =
                { x = 3, y = 3 }

            result =
                KMeans.createGroup [ point1, point3 ] [ point1, point3 ]
          in
          test "createGroup" (\_ -> Expect.equal result [ ( point1, [ point1 ] ), ( point3, [ point3 ] ) ])
        ]
