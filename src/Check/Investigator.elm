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
docs void, bool, order, int, float, percentage, char, ascii, unicode, string, maybe, result, list, array, tuple, tuple3, tuple4, tuple5, func, func2, func3, func4, func5

-}
import RoseTree exposing (RoseTree(..))
import Lazy exposing (Lazy, lazy, force)
import Lazy.List exposing (LazyList, (:::), (+++))
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

flattenGenerator : Generator (Generator a) -> Generator a
flattenGenerator genOfGens =
  Random.customGenerator <|
    \seed ->
        let
            (gen, seed2) = Random.generate genOfGens seed
            (value, seed3) = Random.generate gen seed2
        in
            (value, seed3)

{-| An Investigator is a Random Generator that generates values along with
their corresponding shrink trees.
-}
type alias Investigator a = Generator (RoseTree a)

{-| Create a shrink tree from a shrinker and a value.
-}
shrinkTree : Shrinker a -> a -> RoseTree a
shrinkTree shrink a =
  Rose a (Lazy.List.map (shrinkTree shrink) (shrink a))

{-| Investigator constructor. Construct an Investigator from a generator and
a shrinker.
-}
investigator : Generator a -> Shrinker a -> Investigator a
investigator generator shrink =
  Random.map (shrinkTree shrink) generator


{-| Turn a tree of generators into a generator of trees.
-}
unwind : RoseTree (Generator a) -> Generator (RoseTree a)
unwind treeOfGenerators =
  Random.customGenerator <|
    \seed ->
      let
          (values, seeds) = unzipTree <|
            RoseTree.map (\gen -> Random.generate gen seed) treeOfGenerators

          seed2 =
            RoseTree.root seeds

      in
          (values, seed2)

{-}
unwindList : List (RoseTree a) -> RoseTree (List a)
unwindList trees =
  let
      -- List a
      roots =
        List.map RoseTree.root trees

      -- List (LazyList (RoseTree a))
      children =
-}

unwindLazyList : LazyList (RoseTree a) -> RoseTree (LazyList a)
unwindLazyList trees =
  let
      -- LazyList a
      roots =
        Lazy.List.map RoseTree.root trees

      -- LazyList (LazyList (RoseTree a))
      children =
        Lazy.List.map RoseTree.children trees

      -- LazyList (RoseTree (LazyList a))
      unwoundChildren =
        Lazy.List.map unwindLazyList children

  in
      Rose roots unwoundChildren

unwindList : List (RoseTree a) -> RoseTree (List a)
unwindList trees =
  trees
  |> Lazy.List.fromList
  |> unwindLazyList
  |> RoseTree.map Lazy.List.toList


unwindArray : Array (RoseTree a) -> RoseTree (Array a)
unwindArray trees =
  trees
  |> Lazy.List.fromArray
  |> unwindLazyList
  |> RoseTree.map Lazy.List.toArray


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

--applyShrinkerConstructor : (Shrinker a -> Shrinker b) -> Investigator a -> Investigator b
--applyShrinkerConstructor constructor investigator =

getChildrenRoots : RoseTree a -> LazyList a
getChildrenRoots =
  RoseTree.children >> Lazy.List.map RoseTree.root


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

andThen : Investigator a -> (a -> Investigator b) -> Investigator b
andThen =
  flip flatMap


unzipTree : RoseTree (a, b) -> (RoseTree a, RoseTree b)
unzipTree (Rose (x, y) children) =
  let
      (xs, ys) = unzipList <|
        Lazy.List.map unzipTree children
  in
      (Rose x xs, Rose y ys)

unzipList : LazyList (a, b) -> (LazyList a, LazyList b)
unzipList list =
  let
      step (x,y) (xs, ys) =
        (x ::: xs, y ::: ys)
  in
      Lazy.List.reduce step (Lazy.List.empty, Lazy.List.empty) list


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
  let generator =
        Random.frequency
          [ (3, Random.int -50 50)
          , (1, Random.int Random.minInt Random.maxInt)
          ] (Random.int -50 50)
  in
      investigator generator Shrink.int

{-|
-}
positiveInt : Investigator Int
positiveInt =
  rangeInt 1 (Random.maxInt)

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


{-| Investigator maybe constructor.
Generates random maybe values from a given investigator generator.
-}
maybe : Investigator a -> Investigator (Maybe a)
maybe investigator =
  merge (map Just investigator) (constant Nothing)


{-| Investigator result constructor. Generates random result values from a given
investigator generator using the `result` generator constructor from
elm-random-extra and the `result` shrinker constrctor from elm-shrink.
-}
result : Investigator error -> Investigator value -> Investigator (Result error value)
result errorInvestigator successInvestigator =
  merge (map Err errorInvestigator) (map Ok successInvestigator)


--list : Investigator a -> Investigator (List a)
--list investigator =




{-}
lazylist : Investigator a -> Investigator (LazyList a)
lazylist investigator =
  let
      shrinkList l =
        let
            n =
              Lazy.List.length l

            shrinkOne l = lazy <| \() ->
              case force l of
                Lazy.List.Nil ->
                  force Lazy.List.empty

                Lazy.List.
-}
{-}
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
-}
