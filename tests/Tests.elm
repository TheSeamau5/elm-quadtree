module Tests exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QuadTree exposing (boundingBox, emptyQuadTree, findIntersecting, findItems, getAllItems, insert, insertMany, intersectBoundingBoxes, length)


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


treeLimits =
    boundingBox -10 10 -10 10


quadTreeInsertTest : Test
quadTreeInsertTest =
    describe "Insert to Quad Tree"
        [ test "insert to empty" <|
            \() ->
                let
                    tree =
                        emptyQuadTree treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }
                in
                    Expect.equal (length (insert bounded tree)) 1
        , test "find in 1 element quad tree" <|
            \() ->
                let
                    tree =
                        emptyQuadTree treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    treeWithElements =
                        insert bounded tree
                in
                    Expect.equalLists (Array.toList <| getAllItems treeWithElements) [ bounded ]
        , test "Add multiple items" <|
            \() ->
                let
                    tree =
                        emptyQuadTree treeLimits 4

                    boundeds =
                        [ { boundingBox = boundingBox 0 1 0 1 }
                        , { boundingBox = boundingBox -1 0 0 1 }
                        , { boundingBox = boundingBox 0 1 -1 0 }
                        , { boundingBox = boundingBox -1 0 -1 0 }
                        ]

                    testTree =
                        List.foldl insert tree boundeds
                in
                    Expect.equalLists (Array.toList <| getAllItems testTree) boundeds
        ]


boundedItem minX maxX minY maxY =
    { boundingBox = boundingBox minX maxX minY maxY }


treeLookupTest : Test
treeLookupTest =
    describe "Find in tree"
        [ test "Find in 1 sized tree" <|
            \() ->
                let
                    tree =
                        emptyQuadTree treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    testTree =
                        insert bounded tree

                    searchBox =
                        { boundingBox = boundingBox 0.5 0.5 0.5 0.5 }
                in
                    Expect.equalLists [ bounded ] (Array.toList <| findItems searchBox testTree)
        , test "Find in a massive tree" <|
            \() ->
                let
                    tree =
                        emptyQuadTree treeLimits 4

                    items =
                        [ boundedItem 0 1 0 1
                        , boundedItem -1 0 -2 -1
                        , boundedItem 1 2 0 1
                        , boundedItem 2 4 -1 0
                        , boundedItem -3 -1 -1 0
                        ]

                    testTree =
                        insertMany (Array.fromList items) tree

                    searchBox =
                        boundedItem -2 -2 -0.5 -0.5
                in
                    Expect.equalLists (List.take 1 <| List.reverse items) (Array.toList <| findIntersecting searchBox testTree)
        ]
