module ToolsTests exposing (suite)

import Expect
import Test exposing (..)
import Tools


suite : Test
suite =
    describe "Tools module"
        [ test "Tools.flatten" (\_ -> Expect.equal (Tools.flatten [ Just 1, Nothing ]) [ 1 ])
        ]
