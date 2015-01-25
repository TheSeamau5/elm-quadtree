module QuadTree (
  QuadTree(..),
  Bounded,
  BoundingBox,
  boundingBox,
  width,
  height,
  halfWidth,
  halfHeight,
  center,
  intersectBoundingBoxes,
  emptyQuadTree,
  length,
  insert,
  insertMany,
  remove,
  update,
  getBoundingBox,
  getMaxSize,
  getAllItems,
  reset,
  findItems,
  apply,
  applySafe,
  map,
  mapSafe)where

{-| QuadTree implementation in Elm.

# Bounding Boxes
@docs boundingBox, intersectBoundingBoxes, width, height, halfWidth, halfHeight, center

# Bounded Extensible Type
@docs Bounded

# QuadTree
@docs QuadTree, emptyQuadTree

# Properties
@docs getMaxSize, getBoundingBox, lengthQuadTree

# Inserting items
@docs insert, insertMany

# Removing items
@docs remove

# Updating items
@docs update

# Querying
@docs findItems, getAllItems

# Applying functions
@docs apply, mapQuadTree, mapSafe

# Reset a QuadTree
@docs reset

-}


import Array
import Trampoline (..)

dropIf : (a -> Bool) -> Array.Array a -> Array.Array a
dropIf predicate = Array.filter (not << predicate)

flippedMap : (a -> Array.Array a -> a) -> Array.Array a -> Array.Array a
flippedMap f array =
  let g y x = f x y
  in
    Array.map (g array) array

loop : a -> (a -> Bool) -> (a -> a) -> (a -> b) -> b
loop start condition update return =
  trampoline <|
    loop' start condition update return

loop' : a -> (a -> Bool) -> (a -> a) -> (a -> b) -> Trampoline b
loop' start condition update return =
  case condition start of
    True -> Done (return start)
    False ->
      Continue (\() ->
        loop' (update start) condition update return
      )

--------

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

--------
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


subdivideNE : BoundingBox -> BoundingBox
subdivideNE box =
  let minX = box.horizontal.low + halfWidth box
      minY = box.vertical.low + halfHeight box
  in
    boundingBox minX box.horizontal.high minY box.vertical.high

subdivideNW : BoundingBox -> BoundingBox
subdivideNW box =
  let maxX = box.horizontal.high - halfWidth box
      minY = box.vertical.low + halfHeight box
  in
    boundingBox box.horizontal.low maxX minY box.vertical.high

subdivideSW : BoundingBox -> BoundingBox
subdivideSW box =
  let maxX = box.horizontal.high - halfWidth box
      maxY = box.vertical.high - halfHeight box
  in
    boundingBox box.horizontal.low maxX box.vertical.low maxY

subdivideSE : BoundingBox -> BoundingBox
subdivideSE box =
  let minX = box.horizontal.low + halfWidth box
      maxY = box.vertical.high - halfHeight box
  in
    boundingBox minX box.horizontal.high box.vertical.low maxY

---------
{-| Extend this record type in order to use the QuadTree.
-}
type alias Bounded a = {
  boundingBox : BoundingBox
}

---------
{-| QuadTree type. Keeps its elements in the leaves and
    keeps track of the maximum number of items that
    can be inserted in each leaf.
-}
type QuadTree a =
  Leaf BoundingBox Int (Array.Array a) |
  Node BoundingBox (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)

{-| Construct an empty QuadTree given a bounding box and
    a maxSize. The maxSize limits the number of elements
    that each leaf of the QuadTree can hold.
-}
emptyQuadTree : BoundingBox -> Int -> QuadTree a
emptyQuadTree boundingBox maxSize =
  Leaf boundingBox maxSize Array.empty

{-| Find the number of items in a quadTree. If elements are
    duplicated in different leaves, they will be counted
    multiple times.
-}
length : QuadTree a -> Int
length quadTree =
  case quadTree of
    Leaf box maxSize items -> Array.length items
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      length quadTreeNE +
      length quadTreeNW +
      length quadTreeSW +
      length quadTreeSE

{-| Insert an item into a quadTree.
-}
insert : Bounded a -> QuadTree (Bounded a) -> QuadTree (Bounded a)
insert item quadTree =
  case quadTree of
    Leaf box maxSize items ->
      if intersectBoundingBoxes item.boundingBox box then
        let allItems = Array.push item items
            insertNew quadrant =
              Array.foldr (\item quadTree -> insert item quadTree)
                    (emptyQuadTree quadrant maxSize)
                    allItems
            quadTreeNE = subdivideNE box
            quadTreeNW = subdivideNW box
            quadTreeSW = subdivideSW box
            quadTreeSE = subdivideSE box
        in
          if Array.length items < maxSize then Leaf box maxSize (Array.push item items)
          else
            Node box (insertNew quadTreeNE)
                     (insertNew quadTreeNW)
                     (insertNew quadTreeSW)
                     (insertNew quadTreeSE)
      else
        quadTree

    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      if intersectBoundingBoxes item.boundingBox box then
        Node box (insert item quadTreeNE)
                 (insert item quadTreeNW)
                 (insert item quadTreeSW)
                 (insert item quadTreeSE)
      else
        quadTree

{-| Insert an array of items into a quadTree.
-}
insertMany : Array.Array (Bounded a) -> QuadTree (Bounded a) -> QuadTree (Bounded a)
insertMany items quadTree =
  let stoppingCondition {items} = Array.get 0 items == Nothing
      loopBody ({items, quadTree} as variables) =
        case Array.get 0 items of
          Nothing -> variables
          Just item -> {
            items = Array.slice 1 (Array.length items) items,
            quadTree = insert item quadTree
          }
      returnFunction = .quadTree
  in
    loop {items = items, quadTree = quadTree}
         stoppingCondition
         loopBody
         returnFunction

{-| Remove an item from a quadTree and return the new quadTree.
    If an item is found in multiple leaves, then the item will
    be removed from all leaves.
-}
remove : a -> QuadTree a -> QuadTree a
remove item quadTree =
  case quadTree of
    Leaf box maxSize items -> Leaf box maxSize (dropIf (\it -> it == item) items)
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      Node box (remove item quadTreeNE)
               (remove item quadTreeNW)
               (remove item quadTreeSW)
               (remove item quadTreeSE)

{-| Update an item in a quadTree. This is useful if you just want to
    update a single item. This removes the item from the quadTree,
    applies the given updateFunction, and then inserts the updated
    item into the quadTree.
-}
update : (Bounded a -> Bounded a) -> Bounded a -> QuadTree (Bounded a) -> QuadTree (Bounded a)
update updateFunction item quadTree =
  insert (updateFunction item) (remove item quadTree)

{-| Get the bounding box of a quadTree.
-}
getBoundingBox : QuadTree a -> BoundingBox
getBoundingBox quadTree =
  case quadTree of
    Leaf box _ _ -> box
    Node box _ _ _ _ -> box

{-| Get the maxSize of a quadTree.
-}
getMaxSize : QuadTree a -> Int
getMaxSize quadTree =
  case quadTree of
    Leaf _ maxSize _ -> maxSize
    Node _ quadrant _ _ _ -> getMaxSize quadrant

{-| Get all items from a quadTree. Conserves duplicates.
-}
getAllItems : QuadTree a -> Array.Array a
getAllItems quadTree =
  case quadTree of
    Leaf box maxSize items -> items
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      getAllItems quadTreeNE `Array.append`
      getAllItems quadTreeNW `Array.append`
      getAllItems quadTreeSW `Array.append`
      getAllItems quadTreeSE

{-| Reset a quadTree. This function gets all items
    in a quadTree and pours them
    into an empty quadTree. Useful if the items in
    the quadTree find themselves in the wrong
    leaves.
-}
reset : QuadTree (Bounded a) -> QuadTree (Bounded a)
reset quadTree =
  insertMany (getAllItems quadTree)
             (emptyQuadTree (getBoundingBox quadTree) (getMaxSize quadTree))

{-| Find all items in the quadTree which share a leaf with the given
    item or would share a leaf with the given item were the item in
    the quadTree. Useful for finding items close to the given item.
-}
findItems : Bounded a -> QuadTree (Bounded a) -> Array.Array (Bounded a)
findItems item quadTree =
  case quadTree of
    Leaf box maxSize items ->
      if intersectBoundingBoxes item.boundingBox box
      then items
      else Array.empty
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      findItems item quadTreeNE `Array.append`
      findItems item quadTreeNW `Array.append`
      findItems item quadTreeSW `Array.append`
      findItems item quadTreeSE

{-| Apply a function, that takes an item and an array of items
    and returns an item, to a quadTree. This function is
    a useful helper for collision detection and response
    where the input function updates an object after colliding
    it with an array of objects.
-}
apply : (a -> Array.Array a -> a) -> QuadTree a -> QuadTree a
apply f quadTree =
  case quadTree of
    Leaf box maxSize items -> Leaf box maxSize (flippedMap f items)
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      Node box (apply f quadTreeNE)
               (apply f quadTreeNW)
               (apply f quadTreeSW)
               (apply f quadTreeSE)

{-| Safe version of apply. Automatically calls reset after applying
    the function on the quadTree.
-}
applySafe : (Bounded a -> Array.Array (Bounded a) -> Bounded a) -> QuadTree (Bounded a) -> QuadTree (Bounded a)
applySafe f quadTree =
  reset <| apply f quadTree

{-| The good 'ol map function.
    Maps a function over a quadTree and returns a new quadTree.
    Note: If your function modifies in any way the items'
    bounding boxes, consider using `mapSafe` or calling reset
    after you are done as the quadTree may have items in the
    wrong place. This function doesn't do the clean-up
    automatically. If you want such functionality, use `mapSafe`.
-}
map : (a -> b) -> QuadTree a -> QuadTree b
map f quadTree =
  case quadTree of
    Leaf box maxSize items -> Leaf box maxSize (Array.map f items)
    Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
      Node box (map f quadTreeNE)
               (map f quadTreeNW)
               (map f quadTreeSW)
               (map f quadTreeSE)


{-| Version of `map` where the quadTree is reset
    automatically after applying the function.
-}
mapSafe : (Bounded a -> Bounded b) -> QuadTree (Bounded a) -> QuadTree (Bounded b)
mapSafe f quadTree =
  reset <| map f quadTree
