module Check.Investigator where
{-| Sub-module containing the Investigator type used by elm-check.

This sub-module contains several predefined investigator generators and means of
composing them to create your own. Note that most generators provided are
only well-suited to local development. Property-based testing is by its nature
a very slow process and is best paired with some sort of continuous integration
service. Consider making your own, more general investigator generators when
migrating from local to cloud-based.

# Investigator Definition
@docs Investigator, investigator

# Basic Investigator Generators
@docs void, bool, order, int, float, char, ascii, unicode, string, maybe, result, list, array, tuple, tuple3, tuple4, tuple5

-}
import Array  exposing (Array)
import Shrink exposing (Shrinker)
import Random exposing (Generator)
import Random.Extra as Random
import Random.Bool
import Random.Function
import Random.Order
import Random.Char
import Random.String
import Random.Maybe
import Random.Result
import Random.List
import Random.Array

{-| An Investigator type is a Random Generator paired with a shrinking strategy,
or Shrinker. Shrinkers are defined in `elm-shrink`.
-}
type alias Investigator a =
  { generator : Generator a
  , shrinker  : Shrinker a
  }

{-| Investigator constructor. Construct an Investigator from a generator and
a shrinker.
-}
investigator : Generator a -> Shrinker a -> Investigator a
investigator generator shrinker =
  { generator = generator
  , shrinker  = shrinker
  }


{-| Investigator void. Uses a constant generator and the `void` shrinker from
elm-shrink.
-}
void : Investigator ()
void =
  investigator (Random.constant ()) Shrink.void

{-| Investigator bool. Uses the bool generator from elm-random-extra and the
`bool` shrinker from elm-shrink.
-}
bool : Investigator Bool
bool =
  investigator (Random.Bool.bool) Shrink.bool

{-| Investigator order. Uses the order generator from elm-random-extra and the
`order` shrinker from elm-shrink.
-}
order : Investigator Order
order =
  investigator (Random.Order.order) Shrink.order


{-| Investigator int. Generates random ints between -50 and 50 and the `int`
shrinker from elm-shrink. Ideal for local testing.
-}
int : Investigator Int
int =
  let generator =
        Random.frequency
          [ (3, Random.int -50 50)
          , (1, Random.int Random.minInt Random.maxInt)
          ] (Random.int -50 50)
  in
      investigator generator Shrink.int


{-| Investigator int constructor. Generates random ints between a given `min`
and a given `max` value.
-}
rangeInt : Int -> Int -> Investigator Int
rangeInt min max =
  investigator (Random.int min max) Shrink.int


{-| Investigator float. Generates random floats between -50 and 50 and the `float`
shrinker from elm-shrink. Ideal for local testing.
-}
float : Investigator Float
float =
  let generator =
        Random.frequency
          [ (3, Random.float -50 50)
          , (1, Random.float (toFloat Random.minInt) (toFloat Random.maxInt))
          ] (Random.float -50 50)
  in
      investigator (Random.float -50 50) Shrink.float

{-| Investigator char. Generates random ascii chars using the `ascii` generator
from elm-random-extra and the `char` shrinker from elm-shrink. Ideal for local
testing or if your domain deals exclusively with ascii.
-}
ascii : Investigator Char
ascii =
  investigator (Random.Char.ascii) Shrink.char


{-| Investigator char. Generates random ascii chars disregarding the control
characters using the `char 32 127` generator from elm-random-extra and the
`character` shrinker from elm-shrink. Ideal for local testing or if your
domain deals exclusively with ascii and you do not care about control
characters.
-}
char : Investigator Char
char =
  investigator (Random.Char.char 32 127) Shrink.character


{-| Investigator char. Generates a random UTF-8 character using the
`unicode` generator from elm-random-extra and the `char` shrinker from
elm-shrink.
-}
unicode : Investigator Char
unicode =
  investigator (Random.Char.unicode) Shrink.char

{-| Investigator string. Generates random ascii strings of size between 0 and 10
using the `rangeLengthString` generator from elm-random-extra and the `string`
shrinker from elm-shrink. Ideal for local testing.
-}
string : Investigator String
string =
  investigator
    (Random.String.rangeLengthString 0 10 Random.Char.ascii)
    (Shrink.string)


{-| Investigator maybe constructor. Generates random maybe values from a given
investigator generator using the `maybe` generator constructor from
elm-random-extra and the `maybe` shrinker constructor from elm-shrink.
-}
maybe : Investigator a -> Investigator (Maybe a)
maybe inv =
  investigator
    (Random.Maybe.maybe inv.generator)
    (Shrink.maybe inv.shrinker)

{-| Investigator result constructor. Generates random result values from a given
investigator generator using the `result` generator constructor from
elm-random-extra and the `result` shrinker constrctor from elm-shrink.
-}
result : Investigator error -> Investigator value -> Investigator (Result error value)
result errSpec valSpec =
  investigator
    (Random.Result.result errSpec.generator valSpec.generator)
    (Shrink.result errSpec.shrinker valSpec.shrinker)

{-| Investigator list constructor. Generates random lists of values of size
between 0 and 10 from a given investigator generator using the `rangeLengthList`
generator constructor from elm-random-extra and the `list` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
list : Investigator a -> Investigator (List a)
list inv =
  investigator
    (Random.List.rangeLengthList 0 10 inv.generator)
    (Shrink.list inv.shrinker)


{-| Investigator array constructor. Generates random arrays of values of size
between 0 and 10 from a given investigator generator using the `rangeLengthArray`
generator constructor from elm-random-extra and the `array` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
array : Investigator a -> Investigator (Array a)
array inv =
  investigator
    (Random.Array.rangeLengthArray 0 10 inv.generator)
    (Shrink.array inv.shrinker)


{-| Investigator 2-tuple constructor. Generates random 2-tuples from a 2-tuple
of investigator generators. Uses the `tuple` shrinker constructor from elm-shrink.
-}
tuple : (Investigator a, Investigator b) -> Investigator (a, b)
tuple (invA, invB) =
  investigator
    (Random.zip invA.generator invB.generator)
    (Shrink.tuple (invA.shrinker, invB.shrinker))




{-| Investigator 3-tuple constructor. Generates random 3-tuples from a 3-tuple
of investigator generators. Uses the `tuple3` shrinker constrctor from elm-shrink.
-}
tuple3 : (Investigator a, Investigator b, Investigator c) -> Investigator (a, b, c)
tuple3 (invA, invB, invC) =
  investigator
    (Random.zip3 invA.generator invB.generator invC.generator)
    (Shrink.tuple3 (invA.shrinker, invB.shrinker, invC.shrinker))

{-| Investigator 4-tuple constructor. Generates random 4-tuples from a 4-tuple
of investigator generators. Uses the `tuple4` shrinker constrctor from elm-shrink.
-}
tuple4 : (Investigator a, Investigator b, Investigator c, Investigator d) -> Investigator (a, b, c, d)
tuple4 (invA, invB, invC, invD) =
  investigator
    (Random.zip4 invA.generator invB.generator invC.generator invD.generator)
    (Shrink.tuple4 (invA.shrinker, invB.shrinker, invC.shrinker, invD.shrinker))


{-| Investigator 5-tuple constructor. Generates random 5-tuples from a 5-tuple
of investigator generators. Uses the `tuple5` shrinker constrctor from elm-shrink.
-}
tuple5 : (Investigator a, Investigator b, Investigator c, Investigator d, Investigator e) -> Investigator (a, b, c, d, e)
tuple5 (invA, invB, invC, invD, invE) =
  investigator
    (Random.zip5 invA.generator invB.generator invC.generator invD.generator invE.generator)
    (Shrink.tuple5 (invA.shrinker, invB.shrinker, invC.shrinker, invD.shrinker, invE.shrinker))


{-| Investigator of functions. Takes an investigator for the return type
and returns an investigator of functions. Uses the `func` generator from
elm-random-extra and does not perform any shrinking.
-}
func : Investigator b -> Investigator (a -> b)
func invB =
  investigator
    (Random.Function.func invB.generator)
    (Shrink.noShrink)


func2 : Investigator c -> Investigator (a -> b -> c)
func2 invC =
  investigator
    (Random.Function.func2 invC.generator)
    (Shrink.noShrink)

func3 : Investigator d -> Investigator (a -> b -> c -> d)
func3 invD =
  investigator
    (Random.Function.func3 invD.generator)
    (Shrink.noShrink)

func4 : Investigator e -> Investigator (a -> b -> c -> d -> e)
func4 invE =
  investigator
    (Random.Function.func4 invE.generator)
    (Shrink.noShrink)

func5 : Investigator f -> Investigator (a -> b -> c -> d -> e -> f)
func5 invF =
  investigator
    (Random.Function.func5 invF.generator)
    (Shrink.noShrink)

{-| Shrink a value from an Investigator generator.

    int : Investigator Int

    shrink int 10 == [0,5,7,8,9]
-}
shrink : Investigator a -> Shrinker a
shrink = .shrinker

{-| Extract a Random Generator from and an Investigator generator.
-}
random : Investigator a -> Generator a
random = .generator

{-} Simple example


type alias Vector =
  { x : Float
  , y : Float
  , z : Float
  }

vector : Investigator Vector
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
      investigator generator shrinker
-}
