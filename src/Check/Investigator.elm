module Check.Investigator
  ( Investigator
  , investigator
  , constant
  , fromGenerator
  , applyShrinker
  , flatMap
  , andThen
  , map
  , map2
  , andMap
  , map3
  , map4
  , map5
  , zip
  , zip3
  , zip4
  , zip5
  , merge
  , frequency
  , oneOf
  , generate
  , void
  , bool
  , order
  , int
  , nonNegativeInt
  , sized
  , rangeInt
  , float
  , percentage
  , ascii
  , char
  , upperCaseChar
  , lowerCaseChar
  , unicode
  , string
  , maybe
  , result
  , lazylist
  , list
  , array
  , dict
  , set
  , tuple
  , tuple3
  , tuple4
  , tuple5
  , func
  , func2
  , func3
  , func4
  , func5
  ) where
{-| Sub-module containing the Investigator type used by elm-check.

This sub-module contains several predefined investigator generators and means of
composing them to create your own. Note that most generators provided are
only well-suited to local development. Property-based testing is by its nature
a very slow process and is best paired with some sort of continuous integration
service. Consider making your own, more general investigator generators when
migrating from local to cloud-based.

# Investigator Definition
@docs Investigator

# Construct Investigators
@docs investigator , constant , fromGenerator , applyShrinker

# Common Investigators
@docs void , bool , order , int , nonNegativeInt , sized , rangeInt , float , percentage , ascii , char , upperCaseChar , lowerCaseChar , unicode , string , maybe , result , lazylist , list , array , dict, set, tuple , tuple3 , tuple4 , tuple5 , func , func2 , func3 , func4 , func5

# Common operations
@docs merge , frequency , oneOf , generate

# Functional operations
@docs map , map2 , andMap , map3 , map4 , map5 , zip , zip3 , zip4 , zip5 , flatMap , andThen
-}
import Check.Utils exposing (..)
import RoseTree   exposing (RoseTree(..))
import Lazy       exposing (Lazy, lazy, force)
import Lazy.List  exposing (LazyList, (:::), (+++))
import Array      exposing (Array)
import Dict       exposing (Dict)
import Set        exposing (Set)
import Shrink     exposing (Shrinker)
import Random     exposing (Generator, Seed)
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



{-| An Investigator is a Random Generator that generates values along with
their corresponding shrink trees.
-}
type alias Investigator a = Generator (RoseTree a)

{-| Investigator constructor. Construct an Investigator from a generator and
a shrinker.
-}
investigator : Generator a -> Shrinker a -> Investigator a
investigator generator shrink =
  Random.map (shrinkTree shrink) generator


{-| Analog of Random.generate but for investigators
Allows one to generate a random value from an investigator.
-}
generate : Investigator a -> Seed -> (a, Seed)
generate investigator seed =
  let
      (tree, seed2) =
        Random.generate investigator seed

  in
      (RoseTree.root tree, seed2)


{-| Create an investigator from a constant value that does not shrink.
-}
constant : a -> Investigator a
constant =
  RoseTree.singleton >> Random.constant

{-| Create an investigator from a random generator without shrinking.
-}
fromGenerator : Generator a -> Investigator a
fromGenerator generator =
  Random.map RoseTree.singleton generator

{-| Apply a custom shrinker to an investigator.
-}
applyShrinker : Shrinker a -> Investigator a -> Investigator a
applyShrinker shrink investigator =
  Random.map (RoseTree.root >> shrinkTree shrink) investigator

{-| Map a function that creates an investigator onto an investigator.
-}
flatMap : (a -> Investigator b) -> Investigator a -> Investigator b
flatMap f investigator =
  Random.customGenerator <|
    \seed ->
      let
          -- treeOfAs : RoseTree a
          (treeOfAs, seed2) =
            Random.generate investigator seed

          -- generatorOfTrees : Generator (RoseTree b)
          generatorOfTrees =
            treeOfAs
            |> RoseTree.map f
            |> unwind
            |> Random.map RoseTree.flatten

      in
          Random.generate generatorOfTrees seed2

{-| Chain investigator constructors.
-}
andThen : Investigator a -> (a -> Investigator b) -> Investigator b
andThen =
  flip flatMap


{-| Map a function over an investigator.
-}
map : (a -> b) -> Investigator a -> Investigator b
map =
  RoseTree.map >> Random.map

{-|-}
map2 : (a -> b -> c) -> Investigator a -> Investigator b -> Investigator c
map2 =
  RoseTree.map2 >> Random.map2

{-| Chain mapping operations.
-}
andMap : Investigator (a -> b) -> Investigator a -> Investigator b
andMap =
  map2 (<|)

{-|-}
map3 : (a -> b -> c -> d) -> Investigator a -> Investigator b -> Investigator c -> Investigator d
map3 f l1 l2 l3 =
  f
    `map` l1
    `andMap` l2
    `andMap` l3

{-|-}
map4 : (a -> b -> c -> d -> e) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e
map4 f l1 l2 l3 l4 =
  f
    `map` l1
    `andMap` l2
    `andMap` l3
    `andMap` l4

{-|-}
map5 : (a -> b -> c -> d -> e -> f) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Investigator f
map5 f l1 l2 l3 l4 l5 =
  f
    `map`    l1
    `andMap` l2
    `andMap` l3
    `andMap` l4
    `andMap` l5

{-|-}
zip : Investigator a -> Investigator b -> Investigator (a, b)
zip =
  map2 (,)

{-|-}
zip3 : Investigator a -> Investigator b -> Investigator c -> Investigator (a, b, c)
zip3 =
  map3 (,,)

{-|-}
zip4 : Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator (a, b, c, d)
zip4 =
  map4 (,,,)

{-|-}
zip5 : Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Investigator (a, b, c, d, e)
zip5 =
  map5 (,,,,)

{-| Re-export of Random.Extra.merge.
Choose between two investigators with a 50-50 chance.
Useful for merging two generators that cover different areas of the same type.
-}
merge : Investigator a -> Investigator a -> Investigator a
merge =
  Random.merge

{-| Re-export of Random.Extra.frequency
Create a investigator that chooses an investigator from a list of
likelihood-investigator pairs based on the provided likelihood.
The likelihood of a given investigator being chosen is its likelihood divided
by the sum of all likelihoods. A default investigator must be provided
in the case that the list is empty or that the sum of the likelihoods is 0.
The default investigator is not selected otherwise.
Note that the absolute values of the likelihoods is always taken.
-}
frequency : List (Float, Investigator a) -> Investigator a -> Investigator a
frequency =
  Random.frequency


{-| Similar to `frequency` where all investigators are equally likely to be
selected. Note that, as is the case with `frequency`, in the case that the
provided list is non-empty, the default investigator will not be selected.
-}
oneOf : List (Investigator a) -> Investigator a -> Investigator a
oneOf list default =
  frequency
    (List.map (\gen -> (1, gen)) list)
    default


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
  investigator (Random.int -50 50) Shrink.int

{-| Generate non-negative integers.
-}
nonNegativeInt : Investigator Int
nonNegativeInt =
  rangeInt 0 (Random.maxInt)


{-| Create an investigator from an investigator creator that depends on a
size parameter.
-}
sized : (Int -> Investigator a) -> Investigator a
sized constructor =
  nonNegativeInt
    `andThen` \n -> constructor n

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
  investigator (Random.float -50 50) Shrink.float


{-| Investigator percentage. Generates random floats between 0.0 and 1.0 and the `float`
shrinker from elm-shrink. Useful in conjunction with `tuple` to facilitate
things like generating an array and then selecting one of its elements at random.
-}
percentage : Investigator Float
percentage =
  let generator =
        Random.frequency
          [ (3, Random.float 0 1)
          , (1, Random.constant 0)
          , (1, Random.constant 1)
          ] (Random.float 0 1)
  in
      investigator generator Shrink.float


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

{-| Investigator char. Generates random ascii chars using the `upperCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
upperCaseChar : Investigator Char
upperCaseChar =
  investigator Random.Char.upperCaseLatin Shrink.character

{-| Investigator char. Generates random ascii chars using the `lowerCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
lowerCaseChar : Investigator Char
lowerCaseChar =
  investigator Random.Char.lowerCaseLatin Shrink.character

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


{-| Maybe Investigator constructor.
Generates random maybe values from a given investigator.
-}
maybe : Investigator a -> Investigator (Maybe a)
maybe investigator =
  merge (map Just investigator) (constant Nothing)


{-| Result Investigator constructor.
Generates random result values from a given investigator.
-}
result : Investigator error -> Investigator value -> Investigator (Result error value)
result errorInvestigator successInvestigator =
  merge (map Err errorInvestigator) (map Ok successInvestigator)


{-| Lazy List Investigator constructor.
Generates random lazy lists from a given investigator.
-}
lazylist : Investigator a -> Investigator (LazyList a)
lazylist investigator =
    nonNegativeInt
      `andThen` \x -> rangeInt 0 x
      `andThen` \size -> replicateM size investigator
      `Random.andThen` \roses -> Random.constant (shrinkLazyList roses)


{-| List Investigator constructor.
Generates random lists from a given investigator.
-}
list : Investigator a -> Investigator (List a)
list investigator =
  investigator
  |> lazylist
  |> map Lazy.List.toList


{-| Dict Investigator constructor.
Generates random dicts from a given investigator.
-}
dict : Investigator comparable -> Investigator value -> Investigator (Dict comparable value)
dict invKeys invValues =
  list (tuple (invKeys, invValues))
  |> map Dict.fromList


{-| Set Investigator constructor.
Generates random sets from a given investigator.
-}
set : Investigator comparable -> Investigator (Set comparable)
set investigator =
  investigator
  |> list
  |> map Set.fromList

{-| Array Investigator constructor.
Generates random arrays from a given investigator.
-}
array : Investigator a -> Investigator (Array a)
array investigator =
  investigator
  |> lazylist
  |> map Lazy.List.toArray


{-| Generate an investigator of tuples from a tuple of investigators.
-}
tuple : (Investigator a, Investigator b) -> Investigator (a, b)
tuple (invA, invB) =
  invA
    `andThen` \a -> invB
    `andThen` \b -> constant (a, b)

{-|-}
tuple3 : (Investigator a, Investigator b, Investigator c) -> Investigator (a, b, c)
tuple3 (invA, invB, invC) =
  invA
    `andThen` \a -> invB
    `andThen` \b -> invC
    `andThen` \c -> constant (a, b, c)


{-|-}
tuple4 : (Investigator a, Investigator b, Investigator c, Investigator d) -> Investigator (a, b, c, d)
tuple4 (invA, invB, invC, invD) =
  invA
    `andThen` \a -> invB
    `andThen` \b -> invC
    `andThen` \c -> invD
    `andThen` \d -> constant (a, b, c, d)


{-|-}
tuple5 : (Investigator a, Investigator b, Investigator c, Investigator d, Investigator e) -> Investigator (a, b, c, d, e)
tuple5 (invA, invB, invC, invD, invE) =
  invA
    `andThen` \a -> invB
    `andThen` \b -> invC
    `andThen` \c -> invD
    `andThen` \d -> invE
    `andThen` \e -> constant (a, b, c, d, e)

{-| Generate a random constant function from a given investigator representing
then output.
-}
func : Investigator b -> Investigator (a -> b)
func investigator =
  map always investigator

{-|-}
func2 : Investigator c -> Investigator (a -> b -> c)
func2 investigator =
  map always (func investigator)

{-|-}
func3 : Investigator d -> Investigator (a -> b -> c -> d)
func3 investigator =
  map always (func2 investigator)

{-|-}
func4 : Investigator e -> Investigator (a -> b -> c -> d -> e)
func4 investigator =
  map always (func3 investigator)

{-|-}
func5 : Investigator f -> Investigator (a -> b -> c -> d -> e -> f)
func5 investigator =
  map always (func4 investigator)
