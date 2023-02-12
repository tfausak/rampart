-- | This module provides types and functions for defining intervals and
-- determining how they relate to each other. This can be useful to determine
-- if an event happened during a certain time frame, or if two time frames
-- overlap (and if so, how exactly they overlap).
--
-- There are many other packages on Hackage that deal with intervals, notably
-- [data-interval](https://hackage.haskell.org/package/data-interval) and
-- [intervals](https://hackage.haskell.org/package/intervals). You may prefer
-- one of them for more general interval operations. This module is more
-- focused on how intervals relate to each other.
--
-- This module was inspired by James F. Allen's report,
-- /Maintaining Knowledge About Temporal Intervals/. It also uses terminology
-- from that report. You should not need to read the report in order to
-- understand this module, but if you want to read it you can find it here:
-- <https://hdl.handle.net/1802/10574>.
module Rampart
  ( Interval,
    toInterval,
    fromInterval,
    lesser,
    greater,
    isEmpty,
    isNonEmpty,
    Relation (..),
    relate,
    invert,
  )
where

-- | This type represents an interval bounded by two values, the 'lesser' and
-- the 'greater'. These values can be anything with an 'Ord' instance: numbers,
-- times, strings — you name it.
--
-- Use 'toInterval' to construct an interval and 'fromInterval' to deconstruct
-- one. Use 'relate' to determine how two intervals relate to each other.
newtype Interval a = Interval (a, a) deriving (Eq)

instance (Show a) => Show (Interval a) where
  show x = "toInterval " <> show (fromInterval x)

-- | Converts a tuple into an 'Interval'. Note that this requires an 'Ord'
-- constraint so that the 'Interval' can be sorted on construction.
--
-- Use 'fromInterval' to go in the other direction.
toInterval :: (Ord a) => (a, a) -> Interval a
toInterval (x, y) = if x > y then Interval (y, x) else Interval (x, y)

-- | Converts an 'Interval' into a tuple. Generally you can think of this as
-- the inverse of 'toInterval'. However the tuple returned by this function may
-- be swapped compared to the one originally passed to 'toInterval'.
--
-- @
-- fromInterval ('toInterval' (1, 2)) '==' (1, 2)
-- fromInterval ('toInterval' (2, 1)) '==' (1, 2)
-- @
--
-- prop> fromInterval (toInterval (x, y)) == (min x y, max x y)
fromInterval :: Interval a -> (a, a)
fromInterval (Interval x) = x

-- | Gets the lesser value from an 'Interval'.
--
-- @
-- lesser ('toInterval' (1, 2)) '==' 1
-- lesser ('toInterval' (2, 1)) '==' 1
-- @
--
-- prop> lesser (toInterval (x, y)) == min x y
lesser :: Interval a -> a
lesser = fst . fromInterval

-- | Gets the greater value from an 'Interval'.
--
-- @
-- greater ('toInterval' (1, 2)) '==' 2
-- greater ('toInterval' (2, 1)) '==' 2
-- @
--
-- prop> greater (toInterval (x, y)) == max x y
greater :: Interval a -> a
greater = snd . fromInterval

-- | Returns 'True' if the given 'Interval' is empty, 'False' otherwise. An
-- 'Interval' is empty if its 'lesser' equals its 'greater'.
--
-- @
-- isEmpty ('toInterval' (1, 1)) '==' 'True'
-- isEmpty ('toInterval' (1, 2)) '==' 'False'
-- @
--
-- See 'isNonEmpty' for the opposite check.
isEmpty :: (Eq a) => Interval a -> Bool
isEmpty = uncurry (==) . fromInterval

-- | Returns 'True' if the given 'Interval' is non-empty, 'False' otherwise. An
-- 'Interval' is non-empty if its 'lesser' is not equal to its 'greater'.
--
-- @
-- isNonEmpty ('toInterval' (1, 2)) '==' 'True'
-- isNonEmpty ('toInterval' (1, 1)) '==' 'False'
-- @
--
-- See 'isEmpty' for the opposite check.
isNonEmpty :: (Eq a) => Interval a -> Bool
isNonEmpty = not . isEmpty

-- | This type describes how two 'Interval's relate to each other. Each
-- constructor represents one of the 13 possible relations. Taken together
-- these relations are mutually exclusive and exhaustive.
--
-- Use 'relate' to determine the relation between two 'Interval's.
--
-- The following image shows all 13 possible 'Interval' relations. If for
-- whatever reason you can't see the image, each constructor for this type has
-- ASCII art showing the 'Interval' relation.
--
-- ![All 13 interval relation.](docs/interval-relations.svg)
data Relation
  = -- | 'Interval' @x@ is before 'Interval' @y@.
    --
    -- @
    -- 'greater' x '<' 'lesser' y
    -- @
    --
    -- > +---+
    -- > | x |
    -- > +---+
    -- >       +---+
    -- >       | y |
    -- >       +---+
    Before
  | -- | 'Interval' @x@ meets 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' x '&&'
    -- 'greater' x '==' 'lesser' y
    -- @
    --
    -- > +---+
    -- > | x |
    -- > +---+
    -- >     +---+
    -- >     | y |
    -- >     +---+
    Meets
  | -- | 'Interval' @x@ overlaps 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '<' 'lesser' y '&&'
    -- 'greater' x '>' 'lesser' y '&&'
    -- 'greater' x '<' 'greater' y
    -- @
    --
    -- > +---+
    -- > | x |
    -- > +---+
    -- >   +---+
    -- >   | y |
    -- >   +---+
    Overlaps
  | -- | 'Interval' @x@ is finished by 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' y '&&'
    -- 'lesser' x '<' 'lesser' y '&&'
    -- 'greater' x '==' 'greater' y
    -- @
    --
    -- > +-----+
    -- > |  x  |
    -- > +-----+
    -- >   +---+
    -- >   | y |
    -- >   +---+
    FinishedBy
  | -- | 'Interval' @x@ contains 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '<' 'lesser' y '&&'
    -- 'greater' x '>' 'greater' y
    -- @
    --
    -- > +-------+
    -- > |   x   |
    -- > +-------+
    -- >   +---+
    -- >   | y |
    -- >   +---+
    Contains
  | -- | 'Interval' @x@ starts 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' x '&&'
    -- 'lesser' x '==' 'lesser' y '&&'
    -- 'greater' x '<' 'greater' y
    -- @
    --
    -- > +---+
    -- > | x |
    -- > +---+
    -- > +-----+
    -- > |  y  |
    -- > +-----+
    Starts
  | -- | 'Interval' @x@ is equal to 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '==' 'lesser' y '&&'
    -- 'greater' x '==' 'greater' y
    -- @
    --
    -- > +---+
    -- > | x |
    -- > +---+
    -- > +---+
    -- > | y |
    -- > +---+
    Equal
  | -- | 'Interval' @x@ is started by 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' y '&&'
    -- 'lesser' x '==' 'lesser' y '&&'
    -- 'greater' x '>' 'greater' y
    -- @
    --
    -- > +-----+
    -- > |  x  |
    -- > +-----+
    -- > +---+
    -- > | y |
    -- > +---+
    StartedBy
  | -- | 'Interval' @x@ is during 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '>' 'lesser' y '&&'
    -- 'greater' x '<' 'greater' y
    -- @
    --
    -- >   +---+
    -- >   | x |
    -- >   +---+
    -- > +-------+
    -- > |   y   |
    -- > +-------+
    During
  | -- | 'Interval' @x@ finishes 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' x '&&'
    -- 'lesser' x '>' 'lesser' y '&&'
    -- 'greater' x '==' 'greater' y
    -- @
    --
    -- >   +---+
    -- >   | x |
    -- >   +---+
    -- > +-----+
    -- > |  y  |
    -- > +-----+
    Finishes
  | -- | 'Interval' @x@ is overlapped by 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '>' 'lesser' y '&&'
    -- 'lesser' x '<' 'greater' y '&&'
    -- 'greater' x '>' 'greater' y
    -- @
    --
    -- >   +---+
    -- >   | x |
    -- >   +---+
    -- > +---+
    -- > | y |
    -- > +---+
    OverlappedBy
  | -- | 'Interval' @x@ is met by 'Interval' @y@.
    --
    -- @
    -- 'isNonEmpty' y '&&'
    -- 'lesser' x '==' 'greater' y
    -- @
    --
    -- >     +---+
    -- >     | x |
    -- >     +---+
    -- > +---+
    -- > | y |
    -- > +---+
    MetBy
  | -- | 'Interval' @x@ is after 'Interval' @y@.
    --
    -- @
    -- 'lesser' x '>' 'greater' y
    -- @
    --
    -- >       +---+
    -- >       | x |
    -- >       +---+
    -- > +---+
    -- > | y |
    -- > +---+
    After
  deriving (Eq, Show)

-- | Relates two 'Interval's. Calling @relate x y@ tells you how 'Interval' @x@
-- relates to 'Interval' @y@. Consult the 'Relation' documentation for an
-- explanation of all the possible results.
--
-- @
-- relate ('toInterval' (1, 2)) ('toInterval' (3, 7)) '==' 'Before'
-- relate ('toInterval' (2, 3)) ('toInterval' (3, 7)) '==' 'Meets'
-- relate ('toInterval' (2, 4)) ('toInterval' (3, 7)) '==' 'Overlaps'
-- relate ('toInterval' (2, 7)) ('toInterval' (3, 7)) '==' 'FinishedBy'
-- relate ('toInterval' (2, 8)) ('toInterval' (3, 7)) '==' 'Contains'
-- relate ('toInterval' (3, 4)) ('toInterval' (3, 7)) '==' 'Starts'
-- relate ('toInterval' (3, 7)) ('toInterval' (3, 7)) '==' 'Equal'
-- relate ('toInterval' (3, 8)) ('toInterval' (3, 7)) '==' 'StartedBy'
-- relate ('toInterval' (4, 6)) ('toInterval' (3, 7)) '==' 'During'
-- relate ('toInterval' (6, 7)) ('toInterval' (3, 7)) '==' 'Finishes'
-- relate ('toInterval' (6, 8)) ('toInterval' (3, 7)) '==' 'OverlappedBy'
-- relate ('toInterval' (7, 8)) ('toInterval' (3, 7)) '==' 'MetBy'
-- relate ('toInterval' (8, 9)) ('toInterval' (3, 7)) '==' 'After'
-- @
--
-- Note that relating an empty interval with a non-empty interval may be
-- surprising when the intervals share an endpoint.
--
-- @
-- >>> relate ('toInterval' (3, 3)) ('toInterval' (3, 7)) '==' 'Overlaps'
-- >>> relate ('toInterval' (7, 7)) ('toInterval' (3, 7)) '==' 'OverlappedBy'
-- >>> relate ('toInterval' (3, 7)) ('toInterval' (3, 3)) '==' 'OverlappedBy'
-- >>> relate ('toInterval' (3, 7)) ('toInterval' (7, 7)) '==' 'Overlaps'
-- @
relate :: (Ord a) => Interval a -> Interval a -> Relation
relate x y =
  let lxly = compare (lesser x) (lesser y)
      lxgy = compare (lesser x) (greater y)
      gxly = compare (greater x) (lesser y)
      gxgy = compare (greater x) (greater y)
   in case (lxly, lxgy, gxly, gxgy) of
        (EQ, _, _, EQ) -> Equal
        (_, _, LT, _) -> Before
        (LT, _, EQ, LT) -> Meets
        (_, _, EQ, _) -> Overlaps
        (GT, EQ, _, GT) -> MetBy
        (_, EQ, _, _) -> OverlappedBy
        (_, GT, _, _) -> After
        (LT, _, _, LT) -> Overlaps
        (LT, _, _, EQ) -> FinishedBy
        (LT, _, _, GT) -> Contains
        (EQ, _, _, LT) -> Starts
        (EQ, _, _, GT) -> StartedBy
        (GT, _, _, LT) -> During
        (GT, _, _, EQ) -> Finishes
        (GT, _, _, GT) -> OverlappedBy

-- | Inverts a 'Relation'. Every 'Relation' has an inverse.
--
-- @
-- invert 'Before'       '==' 'After'
-- invert 'After'        '==' 'Before'
-- invert 'Meets'        '==' 'MetBy'
-- invert 'MetBy'        '==' 'Meets'
-- invert 'Overlaps'     '==' 'OverlappedBy'
-- invert 'OverlappedBy' '==' 'Overlaps'
-- invert 'Starts'       '==' 'StartedBy'
-- invert 'StartedBy'    '==' 'Starts'
-- invert 'Finishes'     '==' 'FinishedBy'
-- invert 'FinishedBy'   '==' 'Finishes'
-- invert 'Contains'     '==' 'During'
-- invert 'During'       '==' 'Contains'
-- invert 'Equal'        '==' 'Equal'
-- @
--
-- Inverting a 'Relation' twice will return the original 'Relation'.
--
-- prop> invert (invert r) == r
--
-- Inverting a 'Relation' is like swapping the arguments to 'relate'.
--
-- prop> invert (relate x y) == relate y x
invert :: Relation -> Relation
invert x = case x of
  After -> Before
  Before -> After
  Contains -> During
  During -> Contains
  Equal -> Equal
  FinishedBy -> Finishes
  Finishes -> FinishedBy
  Meets -> MetBy
  MetBy -> Meets
  OverlappedBy -> Overlaps
  Overlaps -> OverlappedBy
  StartedBy -> Starts
  Starts -> StartedBy
