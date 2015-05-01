module Check.Specifier where
{-| Sub-module containing the Specifier type used by elm-check.

This sub-module contains several predefined specifier generators and means of
composing them to create your own. Note that most generators provided are
only well-suited to local development. Property-based testing is by its nature
a very slow process and is best paired with some sort of continuous integration
service. Consider making your own, more general specifier generators when
migrating from local to cloud-based.

# Specifier Definition
@docs Specifier, specifier

# Basic Specifier Generators
@docs void, bool, order, int, float, char, string, maybe, list, array, tuple, tuple3, tuple4, tuple5

-}
import Array  exposing (Array)
import Shrink exposing (Shrinker)
import Random exposing (Generator)
import Random.Extra as Random
import Random.Bool
import Random.Order
import Random.Char
import Random.String
import Random.Maybe
import Random.Result
import Random.List
import Random.Array

{-| An Specifier type is a Random Generator paired with a shrinking strategy,
or Shrinker. Shrinkers are defined in `elm-shrink`.
-}
type alias Specifier a =
  { generator : Generator a
  , shrinker  : Shrinker a
  }

{-| Specifier constructor. Construct an Specifier from a generator and
a shrinker.
-}
specifier : Generator a -> Shrinker a -> Specifier a
specifier generator shrinker =
  { generator = generator
  , shrinker  = shrinker
  }


{-| Specifier void. Uses a constant generator and the `void` shrinker from
elm-shrink.
-}
void : Specifier ()
void =
  specifier (Random.constant ()) Shrink.void

{-| Specifier bool. Uses the bool generator from elm-random-extra and the
`bool` shrinker from elm-shrink.
-}
bool : Specifier Bool
bool =
  specifier (Random.Bool.bool) Shrink.bool

{-| Specifier order. Uses the order generator from elm-random-extra and the
`order` shrinker from elm-shrink.
-}
order : Specifier Order
order =
  specifier (Random.Order.order) Shrink.order


{-| Specifier int. Generates random ints between -50 and 50 and the `int`
shrinker from elm-shrink. Ideal for local testing.
-}
int : Specifier Int
int =
  let generator =
        Random.frequency
          [ (3, Random.int -50 50)
          , (1, Random.int Random.minInt Random.maxInt)
          ] (Random.int -50 50)
  in
      specifier generator Shrink.int


{-| Specifier int constructor. Generates random ints between a given `min`
and a given `max` value.
-}
rangeInt : Int -> Int -> Specifier Int
rangeInt min max =
  specifier (Random.int min max) Shrink.int

{-| Specifier int. Generates random ints between Random.minInt and
Random.maxInt.
-}
anyInt : Specifier Int
anyInt =
  rangeInt Random.minInt Random.maxInt

{-| Specifier float. Generates random floats between -50 and 50 and the `float`
shrinker from elm-shrink. Ideal for local testing.
-}
float : Specifier Float
float =
  specifier (Random.float -50 50) Shrink.float

{-| Specifier char. Generates random ascii chars using the `ascii` generator
from elm-random-extra and the `char` shrinker from elm-shrink. Ideal for local
testing or if your domain deals exclusively with ascii.
-}
char : Specifier Char
char =
  specifier (Random.Char.ascii) Shrink.char

{-| Specifier string. Generates random ascii strings of size between 0 and 10
using the `rangeLengthString` generator from elm-random-extra and the `string`
shrinker from elm-shrink. Ideal for local testing.
-}
string : Specifier String
string =
  specifier
    (Random.String.rangeLengthString 0 10 Random.Char.ascii)
    (Shrink.string)


{-| Specifier maybe constructor. Generates random maybe values from a given
specifier generator using the `maybe` generator constructor from
elm-random-extra and the `maybe` shrinker constructor from elm-shrink.
-}
maybe : Specifier a -> Specifier (Maybe a)
maybe arby =
  specifier
    (Random.Maybe.maybe arby.generator)
    (Shrink.maybe arby.shrinker)

{-| Specifier result constructor. Generates random result values from a given
specifier generator using the `result` generator constructor from
elm-random-extra and the `result` shrinker constrctor from elm-shrink.
-}
result : Specifier error -> Specifier value -> Specifier (Result error value)
result errSpec valSpec =
  specifier
    (Random.Result.result errSpec.generator valSpec.generator)
    (Shrink.result errSpec.shrinker valSpec.shrinker)

{-| Specifier list constructor. Generates random lists of values of size
between 0 and 10 from a given specifier generator using the `rangeLengthList`
generator constructor from elm-random-extra and the `list` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
list : Specifier a -> Specifier (List a)
list arby =
  specifier
    (Random.List.rangeLengthList 0 10 arby.generator)
    (Shrink.list arby.shrinker)


{-| Specifier array constructor. Generates random arrays of values of size
between 0 and 10 from a given specifier generator using the `rangeLengthArray`
generator constructor from elm-random-extra and the `array` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
array : Specifier a -> Specifier (Array a)
array arby =
  specifier
    (Random.Array.rangeLengthArray 0 10 arby.generator)
    (Shrink.array arby.shrinker)


{-| Specifier 2-tuple constructor. Generates random 2-tuples from a 2-tuple
of specifier generators. Uses the `tuple` shrinker constructor from elm-shrink.
-}
tuple : (Specifier a, Specifier b) -> Specifier (a, b)
tuple (arbyA, arbyB) =
  specifier
    (Random.zip arbyA.generator arbyB.generator)
    (Shrink.tuple (arbyA.shrinker, arbyB.shrinker))




{-| Specifier 3-tuple constructor. Generates random 3-tuples from a 3-tuple
of specifier generators. Uses the `tuple3` shrinker constrctor from elm-shrink.
-}
tuple3 : (Specifier a, Specifier b, Specifier c) -> Specifier (a, b, c)
tuple3 (arbyA, arbyB, arbyC) =
  specifier
    (Random.zip3 arbyA.generator arbyB.generator arbyC.generator)
    (Shrink.tuple3 (arbyA.shrinker, arbyB.shrinker, arbyC.shrinker))

{-| Specifier 4-tuple constructor. Generates random 4-tuples from a 4-tuple
of specifier generators. Uses the `tuple4` shrinker constrctor from elm-shrink.
-}
tuple4 : (Specifier a, Specifier b, Specifier c, Specifier d) -> Specifier (a, b, c, d)
tuple4 (arbyA, arbyB, arbyC, arbyD) =
  specifier
    (Random.zip4 arbyA.generator arbyB.generator arbyC.generator arbyD.generator)
    (Shrink.tuple4 (arbyA.shrinker, arbyB.shrinker, arbyC.shrinker, arbyD.shrinker))


{-| Specifier 5-tuple constructor. Generates random 5-tuples from a 5-tuple
of specifier generators. Uses the `tuple5` shrinker constrctor from elm-shrink.
-}
tuple5 : (Specifier a, Specifier b, Specifier c, Specifier d, Specifier e) -> Specifier (a, b, c, d, e)
tuple5 (arbyA, arbyB, arbyC, arbyD, arbyE) =
  specifier
    (Random.zip5 arbyA.generator arbyB.generator arbyC.generator arbyD.generator arbyE.generator)
    (Shrink.tuple5 (arbyA.shrinker, arbyB.shrinker, arbyC.shrinker, arbyD.shrinker, arbyE.shrinker))


{-| Shrink a value from an Specifier generator.

    int : Specifier Int

    shrink int 10 == [0,5,7,8,9]
-}
shrink : Specifier a -> Shrinker a
shrink = .shrinker

{-| Extract a Random Generator from and Specifier generator.
-}
random : Specifier a -> Generator a
random = .generator

{-} Simple example


type alias Vector =
  { x : Float
  , y : Float
  , z : Float
  }

vector : Specifier Vector
vector =
  let
      shrinker {x,y,z} =
        Vector
          `Shrink.map`    shrink float x
          `Shrink.andMap` shrink float y
          `Shrink.andMap` shrink float z

      generator =
        Vector
          `Random.map`    random float
          `Random.andMap` random float
          `Random.andMap` random float
  in
      specifier generator shrinker
-}
