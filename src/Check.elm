module Check
  ( Property
  , TestOutput
  , property
  , property2
  , property3
  , property4
  , property5
  , property6
  , propertyN
  , property2N
  , property3N
  , property4N
  , property5N
  , check
  , simpleCheck
  , continuousCheck
  , continuousCheckEvery
  , deepContinuousCheck
  , deepContinuousCheckEvery
  , print
  , printVerbose
  , display
  , displayVerbose ) where
{-| Library for doing property-based testing in Elm

# Constructing properties
@docs property, propertyN

# Checking the properties
@docs check, simpleCheck

# Continuously checking properties
@docs continuousCheck, deepContinuousCheck, continuousCheckEvery, deepContinuousCheckEvery

# Printing and displaying results
@docs print, printVerbose, display, displayVerbose

# Properties for functions of multiple parameters
@docs property2, property3, property4, property5, property6, property2N, property3N, property4N, property5N

-}

import Random (Generator, list, generate, initialSeed, Seed, customGenerator)
import List (map, map2, filter, length, head, (::))
import Result (Result(..))
import Time (every, second, Time)
import Signal
import String (join)
import Graphics.Element (Element)
import Text (leftAligned, monospace, fromString)


type alias Error =
  { name : String
  , value : String
  , seed : Seed
  }

type alias Success =
  { name : String
  , value : String
  , seed : Seed
  }

type alias TestResult = List (Result Error Success)

type alias Property = (Seed) -> TestResult

type alias TestOutput = List TestResult

mergeTestResult : Result Error Success -> Result Error Success -> Result Error Success
mergeTestResult result1 result2 =
  case result1 of
    Err err1 -> Err err1
    Ok ok1 ->
      case result2 of
        Err err2 -> Err err2
        Ok ok2 -> Ok ok1

mergeTestResults : TestResult -> TestResult -> TestResult
mergeTestResults results1 results2 =
  let errorResults =
        filter
          (\result ->
            case result of
              Ok _ -> False
              Err _ -> True)
          results1
  in
    case length errorResults of
      0 ->
        map2 mergeTestResult results1 results2
      n ->
        errorResults

mergeTestOutputs : TestOutput -> TestOutput -> TestOutput
mergeTestOutputs output1 output2 =
  case output1 of
    [] -> output2
    x :: xs ->
      case output2 of
        [] -> output1
        y :: ys ->
          mergeTestResults x y :: mergeTestOutputs xs ys


generateTestCases : Generator (List a) -> Seed -> (List a, Seed)
generateTestCases listGenerator seed =
  generate listGenerator seed

test : String -> (a -> Bool) -> (List a, Seed) -> TestResult
test name predicate (tests, seed) =
  let testResults = map (\t -> (t, predicate t)) tests
      toResult (t, value) =
        case value of
          True  -> Ok { name = name, value = toString t, seed = seed }
          False -> Err { name = name, value = toString t, seed = seed }
  in
    map toResult testResults


{-| Create a property given a number of test cases, a name, a condition to test and a generator
Example :

    doubleNegativeIsPositive =
      propertyN 1000
                "Double Negative is Positive"
                (\number -> -(-number) == number)
                (float -300 400)
-}
propertyN : Int -> String -> (a -> Bool) -> Generator a -> Property
propertyN numberOfTests name predicate generator =
  \seed ->
      let listGenerator = list numberOfTests generator -- listGenerator : Generator (List a)
          testCases = generateTestCases listGenerator seed -- testCases : (List a, Seed)
      in
        test name predicate testCases


{-| Analog of `propertyN` for functions of two arguments
-}
property2N : Int -> String -> (a -> b -> Bool) -> Generator a -> Generator b -> Property
property2N numberOfTests name predicate generatorA generatorB =
  propertyN numberOfTests name (\(a,b) -> predicate a b) (rZip generatorA generatorB)

{-| Analog of `propertyN` for functions of three arguments
-}
property3N : Int -> String -> (a -> b -> c -> Bool) -> Generator a -> Generator b -> Generator c -> Property
property3N numberOfTests name predicate generatorA generatorB generatorC =
  propertyN numberOfTests name (\(a,b,c) -> predicate a b c) (rZip3 generatorA generatorB generatorC)

{-| Analog of `propertyN` for functions of four arguments
-}
property4N : Int -> String -> (a -> b -> c -> d -> Bool) -> Generator a -> Generator b -> Generator c -> Generator d -> Property
property4N numberOfTests name predicate generatorA generatorB generatorC generatorD =
  propertyN numberOfTests name (\(a,b,c,d) -> predicate a b c d) (rZip4 generatorA generatorB generatorC generatorD)

{-| Analog of `propertyN` for functions of five arguments
-}
property5N : Int -> String -> (a -> b -> c -> d -> e -> Bool) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Property
property5N numberOfTests name predicate generatorA generatorB generatorC generatorD generatorE =
  propertyN numberOfTests name (\(a,b,c,d,e) -> predicate a b c d e) (rZip5 generatorA generatorB generatorC generatorD generatorE)



{-| Create a property given a name, a condition to test and a generator
Example :

    doubleNegativeIsPositive =
      property "Double Negative is Positive"
               (\number -> -(-number) == number)
               (float -300 400)
Note : This property will create 100 test cases. If you want a different
number, use `propertyN`
-}
property : String -> (a -> Bool) -> Generator a -> Property
property = propertyN 100

{-| Analog of `property` for functions of two arguments
Example :

    property2 "Bad Addition Subtraction Inverse"
              (\a b -> (a - b - 1) == (a + (-b)))
              (float 0 100) (float 0 100)
-}
property2 : String -> (a -> b -> Bool) -> Generator a -> Generator b -> Property
property2 name predicate generatorA generatorB =
  property name (\(a,b) -> predicate a b) (rZip generatorA generatorB)

{-| Analog of `property` for functions of three arguments
-}
property3 : String -> (a -> b -> c -> Bool) -> Generator a -> Generator b -> Generator c -> Property
property3 name predicate generatorA generatorB generatorC =
  property name (\(a,b,c) -> predicate a b c) (rZip3 generatorA generatorB generatorC)

{-| Analog of `property` for functions of four arguments
-}
property4 : String -> (a -> b -> c -> d -> Bool) -> Generator a -> Generator b -> Generator c -> Generator d -> Property
property4 name predicate generatorA generatorB generatorC generatorD =
  property name (\(a,b,c,d) -> predicate a b c d) (rZip4 generatorA generatorB generatorC generatorD)

{-| Analog of `property` for functions of five arguments
-}
property5 : String -> (a -> b -> c -> d -> e -> Bool) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Property
property5 name predicate generatorA generatorB generatorC generatorD generatorE =
  property name (\(a,b,c,d,e) -> predicate a b c d e) (rZip5 generatorA generatorB generatorC generatorD generatorE)

{-| Analog of `property` for functions of six arguments
-}
property6 : String -> (a -> b -> c -> d -> e -> f -> Bool) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Property
property6 name predicate generatorA generatorB generatorC generatorD generatorE generatorF =
  property name (\(a,b,c,d,e,f) -> predicate a b c d e f) (rZip6 generatorA generatorB generatorC generatorD generatorE generatorF)


{-| Check a list of properties given a random seed.

    check
      [ prop_reverseReverseList
      , prop_numberIsOdd
      , prop_numberIsEqualToItself
      ]
      (initialSeed 1)
-}
check : List Property -> Seed -> TestOutput
check properties seed = map (\f -> f seed) properties


{-| Version of check with a default initialSeed of 1
-}
simpleCheck : List Property -> TestOutput
simpleCheck properties =
  check properties (initialSeed 1)

{-| Version of check which continuously runs every second
and uses the current time as its seed and merges test outputs.
-}
continuousCheck : List Property -> Signal TestOutput
continuousCheck =
  continuousCheckEvery second


{-| Version of check which continuously runs every given time interval
and uses the current time as its seed and merges test outputs.

    continuousCheck = continuousCheckEvery second
-}
continuousCheckEvery : Time -> List Property -> Signal TestOutput
continuousCheckEvery time properties =
  Signal.foldp mergeTestOutputs []
    (Signal.map ((check properties) << initialSeed << round) (every time))


{-| Version of check which continuously runs every second
and uses the current time as its seed and accumulates all test outputs.
-}
deepContinuousCheck : List Property -> Signal TestOutput
deepContinuousCheck =
  deepContinuousCheckEvery second


{-| Version of check which continuously runs every given time interval
and uses the current time as its seed and accumulates all test outputs.
-}
deepContinuousCheckEvery : Time -> List Property -> Signal TestOutput
deepContinuousCheckEvery time properties =
  Signal.foldp (++) []
    (Signal.map ((check properties) << initialSeed << round) (every time))

printWith : (List String -> String) -> TestResult -> String
printWith flattener results =
  let errorResults =
        filter
          (\result ->
            case result of
              Ok _ -> False
              Err _ -> True)
          results
  in
    if (length errorResults == 0)
    then
      case length results of
        0 -> ""
        n ->
          case head results of
            Ok {name} -> name ++ " has passed " ++ toString n ++ " tests!"
            Err _ -> ""
    else
      (flattener
        (map
          (\result ->
              case result of
                Ok {name, value, seed} ->
                  name ++ " has passed with the following input: " ++ value
                Err {name, value, seed} ->
                  name ++ " has failed with the following input: " ++ value)
          errorResults))

printOne : TestResult -> String
printOne = printWith head

printMany : TestResult -> String
printMany = printWith (join "\n")

{-| Print a test output as a string.
-}
print : TestOutput -> String
print results =
  join "\n" (map printOne results)

{-| Print a test output as a detailed string.
-}
printVerbose : TestOutput -> String
printVerbose results =
  join "\n" (map printMany results)

{-| Display a test output as an Element.
Useful for viewing in the browser.
-}
display : TestOutput -> Element
display output =
  let outputString = print output
  in
    leftAligned (monospace (fromString outputString))

{-| Display a detailed test output as an Element.
Useful for viewing in the browser.
-}
displayVerbose : TestOutput -> Element
displayVerbose output =
  let outputString = printVerbose output
  in
    leftAligned (monospace (fromString outputString))


------ From elm-random-extra -------
--- The following functions are copied from elm-random-extra
--- In order to not depend explicity on elm-random-extra
--- Hopefully, these functions will be merged with the core random module

rZip : Generator a -> Generator b -> Generator (a, b)
rZip = rMap2 (,)

rZip3 : Generator a -> Generator b -> Generator c -> Generator (a, b, c)
rZip3 = rMap3 (,,)

rZip4 : Generator a -> Generator b -> Generator c -> Generator d -> Generator (a, b, c, d)
rZip4 = rMap4 (,,,)

rZip5 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator (a, b, c, d, e)
rZip5 = rMap5 (,,,,)

rZip6 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator (a, b, c, d, e, f)
rZip6 = rMap6 (,,,,,)


rMap : (a -> b) -> Generator a -> Generator b
rMap f generator =
  customGenerator
    (\seed ->
        let (value, nextSeed) = generate generator seed
        in
          (f value, nextSeed))

rMap2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
rMap2 f generatorA generatorB =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
        in
          (f valueA valueB, seed2))

rMap3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
rMap3 f generatorA generatorB generatorC =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
        in
          (f valueA valueB valueC, seed3))

rMap4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
rMap4 f generatorA generatorB generatorC generatorD =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
        in
          (f valueA valueB valueC valueD, seed4))

rMap5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
rMap5 f generatorA generatorB generatorC generatorD generatorE =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
        in
          (f valueA valueB valueC valueD valueE, seed5))

rMap6 : (a -> b -> c -> d -> e -> f -> g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
rMap6 f generatorA generatorB generatorC generatorD generatorE generatorF =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            (valueF, seed6) = generate generatorF seed5
        in
          (f valueA valueB valueC valueD valueE valueF, seed6))

-----------------------------------
