module Check (property, propertyN, check, Property) where
{-| Library for doing property-based testing in Elm

#Constructing properties
@docs property, propertyN

#Checking the properties
@docs check
-}

import Random (Generator, list, generate, initialSeed)
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
