module Check
  ( property
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
  , Property) where
{-| Library for doing property-based testing in Elm

#Constructing properties
@docs property, property2, property3, property4, property5, property6, propertyN, property2N, property3N, property4N, property5N

#Checking the properties
@docs check
-}

import Random (Generator, list, generate, initialSeed, customGenerator)
import List (map, filter, length)
import String (join)
import Result (Result(..))

type alias Property = Result (String, String) ()

generateTestCases : Generator (List a) -> List a
generateTestCases listGenerator =
  (fst
    (generate listGenerator
      (initialSeed 1)))


constructResultPair : (a -> Bool) -> a -> (a, Bool)
constructResultPair predicate value =
  (value, predicate value)


filterFailing : List (a, Bool) -> List (a, Bool)
filterFailing list =
  filter (\x -> (snd x) == False) list

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
property name predicate generator =
  propertyN 100 name predicate generator

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

{-| Create a property given a number of test cases, a name, a condition to test and a generator

Example :

    doubleNegativeIsPositive =
      propertyN 10000
                "Double Negative is Positive"
                (\number -> -(-number) == number)
                (float -300 400)

-}
propertyN : Int -> String -> (a -> Bool) -> Generator a -> Property
propertyN numberOfTests name predicate generator =
  let listGenerator   = list numberOfTests generator
      testCases       = generateTestCases listGenerator
      resultPairList  = map (constructResultPair predicate) testCases
      filteredResult  = filterFailing resultPairList
  in
    case filteredResult of
      [] -> Ok ()
      x :: xs -> Err (name, (toString (fst x)))


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

{-| Returns the result of checking a list of given properties as a string.

Example :

    check [
      property "Identity" (\x -> x == x) (int 5 10),
      property "Double Negative is Positive" (\n -> -(-n) == n) (float 20 100)
    ]
    -- Ok, passed all tests


    check [
      property "No Identity" (\x -> x /= x) (float 0 2)
    ]
    -- No Identity has failed with the following input: 0.7185091971695677
-}
check : List Property -> String
check properties =
  let errorProperties =
        filter
          (\property ->
              case property of
                Ok _ -> False
                Err _ -> True)
          properties
  in
    if (length errorProperties == 0)
    then "Ok, passed all tests."
    else
      (join "\n"
        (map
          (\property ->
              case property of
                Ok _ -> ""
                Err (name, value) ->
                  name ++ " has failed with the following input: " ++ value)
          errorProperties))


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
