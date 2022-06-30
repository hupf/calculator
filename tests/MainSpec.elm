module MainSpec exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "would be nice to have some tests" <| \_ -> Expect.equal 4 (2 + 2) ]
