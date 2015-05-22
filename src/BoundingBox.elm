module BoundingBox (
  BoundingBox,
  Interval,
  boundingBox,
  width,
  height,
  halfWidth,
  halfHeight,
  center,
  subdivideNW,
  subdivideNE,
  subdivideSW,
  subdivideSE,
  intersectBoundingBoxes) where

{-| AABB Bounding box implementation in Elm.

# Types
@docs BoundingBox, Interval

# Contruction and derived properties
@docs boundingBox, width, height, halfWidth, halfHeight, center

# Predicates
@docs intersectBoundingBoxes

# Modification
@docs subdivideNW, subdivideNE, subdivideSW, subdivideSE

-}

{-| Represents a one dimensional range.
-}
type alias Interval = {
  low : Float,
  high : Float
}

pointInInterval : Float -> Interval -> Bool
pointInInterval point interval =
  point < interval.high && point > interval.low

intersectIntervals : Interval -> Interval -> Bool
intersectIntervals interval1 interval2 =
  pointInInterval interval1.low interval2

{-| Represents a rectangle.
-}
type alias BoundingBox = {
  horizontal : Interval,
  vertical : Interval
}

{-| Function to determine if two bounding boxes intersect.
-}
intersectBoundingBoxes : BoundingBox -> BoundingBox -> Bool
intersectBoundingBoxes box1 box2 =
  intersectIntervals box1.horizontal box2.horizontal &&
  intersectIntervals box1.vertical box2.vertical


{-| Construct a bounding box.

    boundingBox minX maxX minY maxY
-}
boundingBox : Float -> Float -> Float -> Float -> BoundingBox
boundingBox minX maxX minY maxY =
  BoundingBox (Interval minX maxX) (Interval minY maxY)

{-| Get the width of a bounding box.
-}
width : BoundingBox -> Float
width box =
  box.horizontal.high - box.horizontal.low

{-| Get the height of a bounding box.
-}
height : BoundingBox -> Float
height box =
  box.vertical.high - box.vertical.low

{-| Get the half-width of a bounding box.
-}
halfWidth : BoundingBox -> Float
halfWidth box =
  width box / 2

{-| Get the half-height of a bounding box.
-}
halfHeight : BoundingBox -> Float
halfHeight box =
  height box / 2

{-| Get the center of a bounding box.
-}
center : BoundingBox -> {x : Float, y : Float}
center box = {
  x = box.horizontal.low + halfWidth box,
  y = box.vertical.low + halfHeight box }


{-| Quarters a bounding box in the North-East direction
-}
subdivideNE : BoundingBox -> BoundingBox
subdivideNE box =
  let minX = box.horizontal.low + halfWidth box
      minY = box.vertical.low + halfHeight box
  in
    boundingBox minX box.horizontal.high minY box.vertical.high

{-| Quarters a bounding box in the North-West direction
-}
subdivideNW : BoundingBox -> BoundingBox
subdivideNW box =
  let maxX = box.horizontal.high - halfWidth box
      minY = box.vertical.low + halfHeight box
  in
    boundingBox box.horizontal.low maxX minY box.vertical.high

{-| Quarters a bounding box in the South-West direction
-}
subdivideSW : BoundingBox -> BoundingBox
subdivideSW box =
  let maxX = box.horizontal.high - halfWidth box
      maxY = box.vertical.high - halfHeight box
  in
    boundingBox box.horizontal.low maxX box.vertical.low maxY

{-| Quarters a bounding box in the South-East direction
-}
subdivideSE : BoundingBox -> BoundingBox
subdivideSE box =
  let minX = box.horizontal.low + halfWidth box
      maxY = box.vertical.high - halfHeight box
  in
    boundingBox minX box.horizontal.high box.vertical.low maxY

