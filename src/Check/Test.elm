module Check.Test where
{-| Submodule providing integration with elm-test.

# Generate unit tests
@docs test, assert

# Multi-arity
@docs test2, test3, test4, test5, assert2, assert3, assert4, assert5

-}
import Check.Investigator as Investigator exposing (Investigator, tuple, tuple3, tuple4, tuple5)
import Trampoline exposing (Trampoline(..), trampoline)
import ElmTest.Test as Test exposing (Test)
import ElmTest.Assertion as Test
import Random exposing (Seed)
import List

{-| Analogous to `claim`. Will generate a given number of unit tests for given
actual and expected statements. If a unit tests fails, `test` will also generate
an additional test representing a minimal input using shrinking.
-}
test : String -> (a -> b) -> (a -> b) -> Investigator a -> Int -> Seed -> Test
test name actualStatement expectedStatement investigator numberOfTests seed =
  let
      generateTests seed currentNumberOfTests tests failed =
        if currentNumberOfTests >= numberOfTests
        then
          Done (tests, failed)
        else
          let
              (value, nextSeed) = Random.generate investigator.generator seed
              testname = (toString value)
              test' = Test.test testname (Test.assertEqual (expectedStatement value) (actualStatement value))
              failed' = case failed of
                Nothing ->
                  let
                      actual = actualStatement value
                      expected = expectedStatement value
                  in
                      if actual == expected
                      then
                        Nothing
                      else
                        Just value

                Just _ -> failed
          in
              Continue (\() -> generateTests nextSeed (currentNumberOfTests + 1) (tests ++ [test']) failed')


      (tests, failed) = trampoline (generateTests seed 0 [] Nothing)
  in case failed of
    Nothing ->
      Test.suite name tests
    Just value ->
      let
          shrink counterExample currentNumberOfShrinks =
            let
                shrunkenCounterExamples = investigator.shrinker counterExample

                failingShrunkenCounterExamples =
                  List.filter (\shrunk ->
                      not (actualStatement shrunk == expectedStatement shrunk)
                  ) shrunkenCounterExamples

            in case List.head failingShrunkenCounterExamples of
              Nothing ->
                Done (counterExample, currentNumberOfShrinks)

              Just failing ->
                Continue (\() -> shrink failing (currentNumberOfShrinks + 1))

          (minimal, numberOfShrinks) =
            trampoline (shrink value 0)

          testname =
            let op =
              if numberOfShrinks == 1
              then "operation"
              else "operations"

            in "After " ++ (toString numberOfShrinks) ++ " shrinking " ++ op ++ " " ++ (toString minimal) ++ " "

          shrinkTest = Test.test testname (Test.assertEqual (expectedStatement minimal) (actualStatement minimal))
      in
          Test.suite name (shrinkTest :: tests)


{-| Assert that a given predicate always yields true for a given investigator.
Analogous to `claimTrue`.
-}
assert : String -> (a -> Bool) -> Investigator a -> Int -> Seed -> Test
assert name predicate investigator numberOfTests seed =
  let
      generateTests seed currentNumberOfTests tests failed =
        if currentNumberOfTests >= numberOfTests
        then
          Done (tests, failed)
        else
          let
              (value, nextSeed) = Random.generate investigator.generator seed
              testname = (toString value)
              result = predicate value
              test' = Test.test testname (Test.assert result)
              failed' = case failed of
                Nothing ->
                  if result
                  then
                    Nothing
                  else
                    Just value
                Just _ -> failed
          in
              Continue (\() -> generateTests nextSeed (currentNumberOfTests + 1) (tests ++ [test']) failed')


      (tests, failed) = trampoline (generateTests seed 0 [] Nothing)
  in case failed of
    Nothing ->
      Test.suite name tests
    Just value ->
      let
          shrink counterExample currentNumberOfShrinks =
            let
                shrunkenCounterExamples = investigator.shrinker counterExample

                failingShrunkenCounterExamples =
                  List.filter (\shrunk ->
                      not (predicate shrunk)
                  ) shrunkenCounterExamples

            in case List.head failingShrunkenCounterExamples of
              Nothing ->
                Done (counterExample, currentNumberOfShrinks)

              Just failing ->
                Continue (\() -> shrink failing (currentNumberOfShrinks + 1))

          (minimal, numberOfShrinks) =
            trampoline (shrink value 0)

          testname =
            let op =
              if numberOfShrinks == 1
              then "operation"
              else "operations"

            in "After " ++ (toString numberOfShrinks) ++ " shrinking " ++ op ++ " " ++ (toString minimal) ++ " "

          shrinkTest = Test.test testname (Test.assert (predicate minimal))
      in
          Test.suite name (shrinkTest :: tests)


test2 : String -> (a -> b -> c) -> (a -> b -> c) -> Investigator a -> Investigator b -> Int -> Seed -> Test
test2 name actualStatement expectedStatement invA invB =
  test name (\(a, b) -> actualStatement a b) (\(a, b) -> expectedStatement a b) (tuple (invA, invB))


assert2 : String -> (a -> b -> Bool) -> Investigator a -> Investigator b -> Int -> Seed -> Test
assert2 name predicate invA invB =
  assert name (\(a, b) -> predicate a b) (tuple (invA, invB))

test3 : String -> (a -> b -> c -> d) -> (a -> b -> c -> d) -> Investigator a -> Investigator b -> Investigator c -> Int -> Seed -> Test
test3 name actualStatement expectedStatement invA invB invC =
  test name (\(a, b, c) -> actualStatement a b c) (\(a, b, c) -> expectedStatement a b c) (tuple3 (invA, invB, invC))

assert3 : String -> (a -> b -> c -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Int -> Seed -> Test
assert3 name predicate invA invB invC =
  assert name (\(a, b, c) -> predicate a b c) (tuple3 (invA, invB, invC))

test4 : String -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Int -> Seed -> Test
test4 name actualStatement expectedStatement invA invB invC invD =
  test name (\(a, b, c, d) -> actualStatement a b c d) (\(a, b, c, d) -> expectedStatement a b c d) (tuple4 (invA, invB, invC, invD))

assert4 : String -> (a -> b -> c -> d -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Int -> Seed -> Test
assert4 name predicate invA invB invC invD =
  assert name (\(a, b, c, d) -> predicate a b c d) (tuple4 (invA, invB, invC, invD))


test5 : String -> (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Int -> Seed -> Test
test5 name actualStatement expectedStatement invA invB invC invD invE =
  test name (\(a, b, c, d, e) -> actualStatement a b c d e) (\(a, b, c, d, e) -> expectedStatement a b c d e) (tuple5 (invA, invB, invC, invD, invE))


assert5 : String -> (a -> b -> c -> d -> e -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Int -> Seed -> Test
assert5 name predicate invA invB invC invD invE =
  assert name (\(a, b, c, d, e) -> predicate a b c d e) (tuple5 (invA, invB, invC, invD, invE))
