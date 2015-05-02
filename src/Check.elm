module Check where
{-| Property Based Testing module in Elm.

# Make a claim
@docs claim, claimTrue, claimFalse

# Check a claim
@docs quickCheck, check

# Group claims into a suite
@docs suite

# Types
@docs Claim, TestResult, UnitTestResult, SuccessOptions, FailureOptions

# Multi-arity claims
@docs claim2, claim2True, claim2False, claim3, claim3True, claim3False, claim4, claim4True, claim4False, claim5, claim5True, claim5False

-}

--------------------------
-- CORE LIBRARY IMPORTS --
--------------------------

import List
import Random     exposing (Seed, Generator)
import Signal     exposing (Address, Mailbox, mailbox, send)
import Task       exposing (Task)
import Trampoline exposing (Trampoline(..), trampoline)

-------------------------
-- THIRD PARTY IMPORTS --
-------------------------

import Check.Specifier  exposing (Specifier, tuple, tuple3, tuple4, tuple5)

-------------------
-- LOCAL IMPORTS --
-------------------

import Random.Extra as Random

-----------
-- TYPES --
-----------

type Claim
  = Claim (Int -> Seed -> TestResult)
  | Suite String (List Claim)

type TestResult
  = Unit UnitTestResult
  | Multiple String (List TestResult)

type alias UnitTestResult =
  Result FailureOptions SuccessOptions

type alias SuccessOptions =
  { name : String
  , seed : Seed
  , numberOfTests : Int
  }

type alias FailureOptions =
  { name : String
  , failingInput : String
  , actual    : String
  , expected  : String
  , unshrunk  :
    { failingInput  : String
    , actual        : String
    , expected      : String
    }
  , seed : Seed
  , numberOfTests   : Int
  , numberOfShrinks : Int
  }


------------------
-- MAKE A CLAIM --
------------------

claim : String -> (a -> b) -> (a -> b) -> Specifier a -> Claim
claim name claim1 claim2 specifier =
  Claim
    (\n seed ->
      let
          failingTestCase' seed accum =
            if accum >= n
            then
              Done (Ok n)
            else
              let
                  (value, nextSeed) = Random.generate specifier.generator seed
                  result1 = claim1 value
                  result2 = claim2 value
              in
                  if result1 == result2
                  then
                    Continue (\() -> failingTestCase' nextSeed (accum + 1))
                  else
                    Done (Err (value, nextSeed, accum + 1))

          failingTestCase =
            trampoline (failingTestCase' seed 0)

      in case failingTestCase of
        Ok n -> Unit <|
          Ok
            { name = name
            , seed = seed
            , numberOfTests = max 0 n
            }
        Err (failingValue, seed, n) ->
          let
              shrink value numberOfShrinks =
                let
                    shrinks = specifier.shrinker value

                    failingShrunks =
                      List.filter (\shrunk ->
                        not (claim1 shrunk == claim2 shrunk)
                      ) shrinks

                in case List.head failingShrunks of
                  Nothing ->
                    Done (value, numberOfShrinks)

                  Just failing ->
                    Continue (\() -> shrink failing (numberOfShrinks + 1))

              (minimal, numberOfShrinks) =
                trampoline (shrink failingValue 0)

              actual = claim1 minimal
              expected = claim2 minimal

          in Unit <|
            Err
              { name = name
              , seed = seed
              , failingInput = toString minimal
              , expected = toString expected
              , actual = toString actual
              , unshrunk =
                { failingInput = toString failingValue
                , actual    = toString (claim1 failingValue)
                , expected  = toString (claim2 failingValue)
                }
              , numberOfTests = n
              , numberOfShrinks = numberOfShrinks
              }
    )

claimTrue : String -> (a -> Bool) -> Specifier a -> Claim
claimTrue name predicate =
  claim name predicate (always True)

claimFalse : String -> (a -> Bool) -> Specifier a -> Claim
claimFalse name predicate =
  claim name predicate (always False)


-------------------
-- CHECK A CLAIM --
-------------------

check : Claim -> Int -> Seed -> TestResult
check claim n seed = case claim of
  Claim f ->
    f n seed
  Suite name claims ->
    Multiple name (List.map (\c -> check c n seed) claims)


quickCheck : Claim -> TestResult
quickCheck claim =
  check claim 100 (Random.initialSeed 1)


-------------------------------
-- GROUP CLAIMS INTO A SUITE --
-------------------------------

suite : String -> List Claim -> Claim
suite name claims =
  Suite name claims


------------------------
-- MULTI-ARITY CLAIMS --
------------------------

claim2 : String -> (a -> b -> c) -> (a -> b -> c) -> Specifier a -> Specifier b -> Claim
claim2 name claim1 claim2 specA specB =
  claim name (\(a, b) -> claim1 a b) (\(a, b) -> claim2 a b) (tuple (specA, specB))

claim2True : String -> (a -> b -> Bool) -> Specifier a -> Specifier b -> Claim
claim2True name predicate =
  claim2 name predicate (\_ _ -> True)

claim2False : String -> (a -> b -> Bool) -> Specifier a -> Specifier b -> Claim
claim2False name predicate =
  claim2 name predicate (\_ _ -> False)

claim3 : String -> (a -> b -> c -> d) -> (a -> b -> c -> d) -> Specifier a -> Specifier b -> Specifier c -> Claim
claim3 name claim1 claim2 specA specB specC =
  claim name (\(a, b, c) -> claim1 a b c) (\(a, b, c) -> claim2 a b c) (tuple3 (specA, specB, specC))

claim3True : String -> (a -> b -> c -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Claim
claim3True name predicate =
  claim3 name predicate (\_ _ _ -> True)

claim3False : String -> (a -> b -> c -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Claim
claim3False name predicate =
  claim3 name predicate (\_ _ _ -> False)

claim4 : String -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Claim
claim4 name claim1 claim2 specA specB specC specD =
  claim name (\(a, b, c, d) -> claim1 a b c d) (\(a, b, c, d) -> claim2 a b c d) (tuple4 (specA, specB, specC, specD))

claim4True : String -> (a -> b -> c -> d -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Claim
claim4True name predicate =
  claim4 name predicate (\_ _ _ _ -> True)

claim4False : String -> (a -> b -> c -> d -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Claim
claim4False name predicate =
  claim4 name predicate (\_ _ _ _ -> False)


claim5 : String -> (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Specifier e -> Claim
claim5 name claim1 claim2 specA specB specC specD specE =
  claim name (\(a, b, c, d, e) -> claim1 a b c d e) (\(a, b, c, d, e) -> claim2 a b c d e) (tuple5 (specA, specB, specC, specD, specE))

claim5True : String -> (a -> b -> c -> d -> e -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Specifier e -> Claim
claim5True name predicate =
  claim5 name predicate (\_ _ _ _ _ -> True)

claim5False : String -> (a -> b -> c -> d -> e -> Bool) -> Specifier a -> Specifier b -> Specifier c -> Specifier d -> Specifier e -> Claim
claim5False name predicate =
  claim5 name predicate (\_ _ _ _ _ -> False)
