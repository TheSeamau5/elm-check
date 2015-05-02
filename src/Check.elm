module Check where
{-| Property Based Testing module in Elm.

# Make a claim
@docs claim, claimTrue, claimFalse

# Check a claim
@docs quickCheck, check

# Group claims into a suite
@docs suite

# Types
@docs Claim, Evidence, UnitEvidence, SuccessOptions, FailureOptions

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

{-| A Claim is an object that makes a claim of truth about a system.
A claim is either a function which yields evidence regarding the claim
or a list of such claims.
-}
type Claim
  = Claim (Int -> Seed -> Evidence)
  | Suite String (List Claim)

{-| Evidence is the output from checking a claim or multiple claims.
-}
type Evidence
  = Unit UnitEvidence
  | Multiple String (List Evidence)

{-| UnitEvidence is the concrete type returned by checking a single claim.
A UnitEvidence can easily be converted to an assertion or can be considered
as the result of an assertion.
-}
type alias UnitEvidence =
  Result FailureOptions SuccessOptions

{-| SuccessOptions is the concrete type returned in case there is no evidence
found disproving a Claim.

SuccessOptions contains:
1. the `name` of the claim
2. the number of checks performed
3. the `seed` used in order to reproduce the check.
-}
type alias SuccessOptions =
  { name : String
  , seed : Seed
  , numberOfChecks : Int
  }

{-| FailureOptions is the concrete type returned in case evidence was found
disproving a Claim.

FailureOptions contains:
1. the `name` of the claim
2. the minimal `counterExample` which serves as evidence that the claim is false
3. the value `expected` to be returned by the claim
4. the `actual` value returned by the claim
5. the `seed` used in order to reproduce the results
6. the number of checks performed
7. the number of shrinking operations performed
8. the original `counterExample`, `actual`, and `expected` values found prior
to performing the shrinking operations.
-}
type alias FailureOptions =
  { name : String
  , counterExample : String
  , actual    : String
  , expected  : String
  , original  :
    { counterExample  : String
    , actual        : String
    , expected      : String
    }
  , seed : Seed
  , numberOfChecks   : Int
  , numberOfShrinks : Int
  }


------------------
-- MAKE A CLAIM --
------------------

{-| Make a claim about a system.

    claim nameOfClaim actualStatement expectedStatement specifier

1. The `nameOfClaim` is a string you pass in order to name your claim.
This is very useful when trying to debug or reading reports.
2. The `actualStatement` is a function which states something about your
system. The result of which will be compared by equality `==` to the
result of the `expectedStatement`.
3. The `expectedStatement` is a function which states something which
the `actualStatement` should conform to or be equivalent to. The result of
which will be compared by equality `==` to the result of the `actualStatement`.
4. The `specifier` is a specifier used to generate random values to be passed
to the `actualStatement` and `expectedStatement` in order to attempt to
disprove the claim. If a counter example is found, the `specifier` will then
shrink the counter example until it yields a minimal counter example which
is then easy to debug.


Example :

    claim_sort_idempotent =
      claim "Sort is idempotent"
        (\list -> List.sort (List.sort (list))
        (\list -> List.sort (list))
        (list int)

-}
claim : String -> (a -> b) -> (a -> b) -> Specifier a -> Claim
claim name claim1 claim2 specifier =
  Claim
    (\n seed ->
      let
          -- originalCounterExample' : Seed -> Int -> Trampoline (Result (a, b, b, Seed, Int) Int)
          originalCounterExample' seed accum =
            if accum >= n
            then
              Done (Ok n)
            else
              let
                  (value, nextSeed) = Random.generate specifier.generator seed
                  actual = claim1 value
                  expected = claim2 value
              in
                  if actual == expected
                  then
                    Continue (\() -> originalCounterExample' nextSeed (accum + 1))
                  else
                    Done (Err (value, actual, expected, nextSeed, accum + 1))

          -- originalCounterExample : Result (a, b, b, Seed, Int) Int
          originalCounterExample =
            trampoline (originalCounterExample' seed 0)

      in case originalCounterExample of
        Ok n -> Unit <|
          Ok
            { name = name
            , seed = seed
            , numberOfChecks = max 0 n
            }
        Err (originalCounterExample, originalActual, originalExpected, seed, n) ->
          let
              -- shrink : a -> Int -> Trampoline (a, Int)
              shrink value numberOfShrinks =
                let
                    -- shrinks : List a
                    shrinks = specifier.shrinker value

                    -- failingShrunks : List a
                    failingShrunks =
                      List.filter (\shrunk ->
                        not (claim1 shrunk == claim2 shrunk)
                      ) shrinks

                in case List.head failingShrunks of
                  Nothing ->
                    Done (value, numberOfShrinks)

                  Just failing ->
                    Continue (\() -> shrink failing (numberOfShrinks + 1))

              -- minimal : a
              -- numberOfShrinks : Int
              (minimal, numberOfShrinks) =
                trampoline (shrink originalCounterExample 0)

              -- actual : b
              actual = claim1 minimal

              -- expected : b
              expected = claim2 minimal

          in Unit <|
            Err
              { name = name
              , seed = seed
              , counterExample = toString minimal
              , expected = toString expected
              , actual = toString actual
              , original =
                { counterExample = toString originalCounterExample
                , actual    = toString originalActual
                , expected  = toString originalExpected
                }
              , numberOfChecks = n
              , numberOfShrinks = numberOfShrinks
              }
    )

{-| Make a claim of truth about a system.

Similar to `claim`, `claimTrue` only considers claims which always yield `True`
to be true. If `claimTrue` manages to find an input which causes the given
predicate to yield `False`, then it will consider that as the counter example.

    claimTrue nameOfClaim predicate specifier


Example:

    claim_length_list_nonnegative =
      claimTrue "The length of a list is strictly non-negative"
        (\list -> List.length list >= 0)
        (list string)
-}
claimTrue : String -> (a -> Bool) -> Specifier a -> Claim
claimTrue name predicate =
  claim name predicate (always True)


{-| Make a claim of falsiness about a system.

Analogous to `claimTrue`, `claimFalse` only considers claims which always yield
`False` to be true. If `claimFalse` manages to find an input which causes the
given predicate to yield `True`, then it will consider that as the counter
example.

    claimFalse nameOfClaim predicate specifier


Example:

    claim_length_list_never_negative =
      claimFalse "The length of a list is never negative"
      (\list -> List.length list < 0)
      (list float)
-}
claimFalse : String -> (a -> Bool) -> Specifier a -> Claim
claimFalse name predicate =
  claim name predicate (always False)


-------------------
-- CHECK A CLAIM --
-------------------

{-| Check a claim.

To check a claim, you need to provide the number of checks which check will
perform as well a random seed. Given a random seed and a number of checks,
`check` will always yield the same result. Thus, `check` is especially useful
when you wish to reproduce checks.

    check claim numberOfChecks seed
-}
check : Claim -> Int -> Seed -> Evidence
check claim n seed = case claim of
  Claim f ->
    f n seed
  Suite name claims ->
    Multiple name (List.map (\c -> check c n seed) claims)


{-| Quick check a claim.

This function is very useful when checking claims locally. `quickCheck` will
perform 100 checks and use `Random.initialSeed 1` as the random seed.

    quickCheck claim =
      check claim 100 (Random.initialSeed 1)
-}
quickCheck : Claim -> Evidence
quickCheck claim =
  check claim 100 (Random.initialSeed 1)


-------------------------------
-- GROUP CLAIMS INTO A SUITE --
-------------------------------

{-| Group a list of claims into a suite. This is very useful in order to
group similar claims together.

    suite nameOfSuite listOfClaims
-}
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
