module KMeansTests exposing(..)
import Test exposing (..)
import Expect
import KMeans

suite : Test
suite =
    let
        result = KMeans.run [{x=1, y=1}] [{x=1, y=1}]
    in
        test "KMeans: first case" (\_ -> Expect.equal result [{x=1, y=1}])
