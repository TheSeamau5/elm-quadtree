module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QuadTree exposing (boundingBox, intersectBoundingBoxes)


toIntervalTest ( ( low1, high1 ), ( low2, high2 ), expected ) =
    let
        title =
            "Interval "
                ++ toString low1
                ++ " to "
                ++ toString high1
                ++ " and "
                ++ toString low2
                ++ " to "
                ++ toString high2
                ++ " should "
                ++ (if expected then
                        "intersect"
                    else
                        "not intersect"
                   )

        box1 =
            boundingBox low1 high1 0 0

        box2 =
            boundingBox low2 high2 0 0
    in
        test title <| \() -> Expect.equal (intersectBoundingBoxes box1 box2) expected


intersectBoundingBoxTest : Test
intersectBoundingBoxTest =
    describe "Intersect Bounding Box"
        [ describe "1D Bounding Box (Interval)"
          {-
             Possible options for (low1, high1) as interval1, (low2, high2) as interval2:
             X ---->>>    0     1       2     3     4     5
             interval1    |-------------|
             interval2                        |-----------|

             interval1    |-------------|
             interval2    |-------------|

             interval1                        |-----------|
             interval2    |-------------|

             interval1    |-------------|
             interval2          |-------------|

             interval1          |-------------|
             interval2    |-------------|

             interval1          |-------|
             interval2    |-------------------|

             interval1    |-------------------|
             interval2          |-------|
          -}
          <|
            List.map
                toIntervalTest
                {- Interval1, Interval2, Intersect? -}
                [ ( ( 0, 2 ), ( 3, 5 ), False )
                , ( ( 0, 2 ), ( 0, 2 ), True )
                , ( ( 3, 5 ), ( 0, 2 ), False )
                , ( ( 0, 2 ), ( 1, 3 ), True )
                , ( ( 1, 3 ), ( 0, 2 ), True )
                , ( ( 1, 2 ), ( 0, 3 ), True )
                , ( ( 0, 3 ), ( 1, 2 ), True )
                ]
        ]
