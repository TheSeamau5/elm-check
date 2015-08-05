module Check
  ( Claim
  , TestTree
  , Evidence
  , SuccessOptions
  , FailureOptions
  , suite
  , claim_generic
  , claim
  , claimTrue
  , claimFalse
  , forAll
  , check
  , quickCheck
  , claim2
  , claim2True
  , claim2False
  , claim3
  , claim3True
  , claim3False
  , claim4
  , claim4True
  , claim4False
  , claim5
  , claim5True
  , claim5False
  , that
  , is
  , for
  , true
  , false
  ) where
{-|

# Make a claim
@docs Claim, claim, forAll, claimTrue, claimFalse

# Check a claim
@docs check, quickCheck

# Group claims into a suite
@docs suite

# Types
@docs TestTree, Evidence, SuccessOptions, FailureOptions

# Make a generic claim (where equality is not defined by `==`)
@docs claim_generic

# Multi-arity claims
@docs claim2, claim2True, claim2False, claim3, claim3True, claim3False, claim4, claim4True, claim4False, claim5, claim5True, claim5False


# DSL
`elm-check` provides a shorthand DSL for authoring claims. The goal of this
DSL is to help improve readability and encode intent in the phrasing of your
test code.
With the DSL, claims read as either:
1. claim - (string) - that - (actual) - is - (expected) - for - (investigator)
2. claim - (string) - true - (predicate) - for - (investigator)
3. claim - (string) - false - (predicate) - for - (investigator)
**Example:**
    claim_multiplication_by_one_noop =
      claim
        "Multiplying by one does not change a number"
      `true`
        (\n -> n * 1 == n)
      `for`
        int
    claim_reverse_append =
      claim
        "Append then reverse is equivalent to reverse then append"
      `that`
        (\(l1, l2) -> List.reverse (l1 ++ l2))
      `is`
        (\(l1, l2) -> List.reverse l1 ++ List.reverse l2)
      `for`
        tuple (list int, list int)
It is important to note that, if you wish to deal with multi-arity functions
using this DSL, you must deal explicitly in tuples.
*Warning: The DSL follows a very strict format. Deviating from this format will
yield potentially unintelligible type errors. While not all of the type errors
are strictly necessary, they are there to ensure that the test is authored in
a uniform way. As a result, the following functions have horrendous type
signatures and you are better off ignoring them.*
@docs that, is, for, true, false
-}

import RoseTree   exposing (RoseTree(..))
import Lazy       exposing (Lazy, lazy, force)
import Lazy.List  exposing (LazyList)
import Random     exposing (Generator, Seed)
import Random.Extra as Random
import Result exposing (Result(..))
import List
import Check.Utils exposing (..)
import Check.Investigator as Investigator exposing (Investigator, tuple, tuple3, tuple4, tuple5)
import Trampoline exposing (Trampoline(..), trampoline)

{-|-}
type alias Claim = Int -> Seed -> TestTree Evidence

{-|-}
type TestTree a
  = Suite String (List (TestTree a))
  | Unit String a

{-|-}
type alias Evidence = Result FailureOptions SuccessOptions

{-|-}
type alias SuccessOptions =
  { seed : Seed
  , numberOfChecks : Int
  }

{-|-}
type alias FailureOptions =
  { counterExample : String
  , actual    : String
  , expected  : String
  , original  :
    { counterExample  : String
    , actual    : String
    , expected  : String
    }
  , seed : Seed
  , numberOfChecks  : Int
  , depth : Int
  , totalNodesVisited : Int
  }

{-|-}
suite : String -> List Claim -> Claim
suite name claims n seed =
  Suite name (List.map (\c -> c n seed) claims)


{-|-}
claim_generic : String -> (a -> b) -> (a -> b) -> (b -> b -> Bool) -> Investigator a -> Claim
claim_generic name actualStatement expectedStatement comparison investigator numberOfChecks seed =
  let
      counterExample =
        getCounterExample actualStatement expectedStatement comparison investigator numberOfChecks seed

      predicate =
        makePredicate actualStatement expectedStatement comparison


  in Unit name <|
      case counterExample of
        Ok _ ->
          Ok
            { seed = seed
            , numberOfChecks = numberOfChecks
            }

        Err {value, actual, expected, seed, numberOfChecks, shrinkTree} ->
          let
              shrinkResult =
                findSmallestShrink predicate shrinkTree

          in
              Err
                { counterExample = toString shrinkResult.value
                , actual = toString (actualStatement shrinkResult.value)
                , expected = toString (expectedStatement shrinkResult.value)
                , seed = seed
                , numberOfChecks = numberOfChecks
                , depth = shrinkResult.depth
                , totalNodesVisited = shrinkResult.totalNodesVisited
                , original =
                    { counterExample = toString value
                    , actual = toString actual
                    , expected = toString expected
                    }
                }


{-|-}
claim : String -> (a -> b) -> (a -> b) -> Investigator a -> Claim
claim name actualStatement expectedStatement investigator =
  claim_generic name actualStatement expectedStatement (==) investigator



{-| Make a claim of truth about a system.
Similar to `claim`, `claimTrue` only considers claims which always yield `True`
to be true. If `claimTrue` manages to find an input which causes the given
predicate to yield `False`, then it will consider that as the counter example.
    claimTrue nameOfClaim predicate investigator
Example:
    claim_length_list_nonnegative =
      claimTrue "The length of a list is strictly non-negative"
        (\list -> List.length list >= 0)
        (list string)
-}
claimTrue : String -> (a -> Bool) -> Investigator a -> Claim
claimTrue name predicate =
  claim name predicate (always True)


{-| Make a claim of falsiness about a system.
Analogous to `claimTrue`, `claimFalse` only considers claims which always yield
`False` to be true. If `claimFalse` manages to find an input which causes the
given predicate to yield `True`, then it will consider that as the counter
example.
    claimFalse nameOfClaim predicate investigator
Example:
    claim_length_list_never_negative =
      claimFalse "The length of a list is never negative"
      (\list -> List.length list < 0)
      (list float)
-}
claimFalse : String -> (a -> Bool) -> Investigator a -> Claim
claimFalse name predicate =
  claim name predicate (always False)


{-| Shorthand for making a claim.
-}
forAll : Investigator a -> (a -> Bool) -> Claim
forAll investigator predicate =
  claimTrue "" predicate investigator


-------------------
-- CHECK A CLAIM --
-------------------

{-| Check a claim.
To check a claim, you need to provide the number of checks which check will
perform as well a random seed. Given a random seed and a number of checks,
`check` will always yield the same result. Thus, `check` is especially useful
when you wish to reproduce checks.
    check claim 100 (Random.initialSeed 1)
-}
check : Claim -> Int -> Seed -> TestTree Evidence
check claim int seed =
  claim int seed


{-| Quick check a claim.
This function is very useful when checking claims locally. `quickCheck` will
perform 100 checks and use `Random.initialSeed 1` as the random seed.
    quickCheck claim =
      check claim 100 (Random.initialSeed 1)
-}
quickCheck : Claim -> TestTree Evidence
quickCheck claim =
  check claim 100 (Random.initialSeed 1)

------------------------
-- MULTI-ARITY CLAIMS --
------------------------

{-|-}
claim2 : String -> (a -> b -> c) -> (a -> b -> c) -> Investigator a -> Investigator b -> Claim
claim2 name actualStatement expectedStatement specA specB =
  claim name (\(a, b) -> actualStatement a b) (\(a, b) -> expectedStatement a b) (tuple (specA, specB))

{-|-}
claim2True : String -> (a -> b -> Bool) -> Investigator a -> Investigator b -> Claim
claim2True name predicate =
  claim2 name predicate (\_ _ -> True)

{-|-}
claim2False : String -> (a -> b -> Bool) -> Investigator a -> Investigator b -> Claim
claim2False name predicate =
  claim2 name predicate (\_ _ -> False)

{-|-}
claim3 : String -> (a -> b -> c -> d) -> (a -> b -> c -> d) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3 name actualStatement expectedStatement specA specB specC =
  claim name (\(a, b, c) -> actualStatement a b c) (\(a, b, c) -> expectedStatement a b c) (tuple3 (specA, specB, specC))

{-|-}
claim3True : String -> (a -> b -> c -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3True name predicate =
  claim3 name predicate (\_ _ _ -> True)

{-|-}
claim3False : String -> (a -> b -> c -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Claim
claim3False name predicate =
  claim3 name predicate (\_ _ _ -> False)

{-|-}
claim4 : String -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4 name actualStatement expectedStatement specA specB specC specD =
  claim name (\(a, b, c, d) -> actualStatement a b c d) (\(a, b, c, d) -> expectedStatement a b c d) (tuple4 (specA, specB, specC, specD))

{-|-}
claim4True : String -> (a -> b -> c -> d -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4True name predicate =
  claim4 name predicate (\_ _ _ _ -> True)

{-|-}
claim4False : String -> (a -> b -> c -> d -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Claim
claim4False name predicate =
  claim4 name predicate (\_ _ _ _ -> False)

{-|-}
claim5 : String -> (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5 name actualStatement expectedStatement specA specB specC specD specE =
  claim name (\(a, b, c, d, e) -> actualStatement a b c d e) (\(a, b, c, d, e) -> expectedStatement a b c d e) (tuple5 (specA, specB, specC, specD, specE))

{-|-}
claim5True : String -> (a -> b -> c -> d -> e -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5True name predicate =
  claim5 name predicate (\_ _ _ _ _ -> True)

{-|-}
claim5False : String -> (a -> b -> c -> d -> e -> Bool) -> Investigator a -> Investigator b -> Investigator c -> Investigator d -> Investigator e -> Claim
claim5False name predicate =
  claim5 name predicate (\_ _ _ _ _ -> False)



---------
-- DSL --
---------

{-|-}
that : ((a -> b) -> (a -> b) -> Investigator a -> Claim) -> (a -> b) -> ((a -> b) -> Investigator a -> Claim)
that f x = f x

{-|-}
is : ((a -> b) -> Investigator a -> Claim) -> (a -> b) -> (Investigator a -> Claim)
is f x = f x

{-|-}
for : (Investigator a -> Claim) -> Investigator a -> Claim
for f x = f x

{-|-}
true : ((a -> Bool) -> (a -> Bool) -> Investigator a -> Claim) -> (a -> Bool) -> (Investigator a -> Claim)
true f pred =
  f pred (always True)

{-|-}
false : ((a -> Bool) -> (a -> Bool) -> Investigator a -> Claim) -> (a -> Bool) -> (Investigator a -> Claim)
false f pred =
  f pred (always False)




-----------------
-- HELPER CODE --
-----------------

type alias CounterExample a b =
  Result
    { value : a
    , actual : b
    , expected : b
    , seed : Seed
    , numberOfChecks : Int
    , shrinkTree : RoseTree a
    }
    ()

getCounterExample : (a -> b) -> (a -> b) -> (b -> b -> Bool) -> Investigator a -> Int -> Seed -> CounterExample a b
getCounterExample actualStatement expectedStatement comparison investigator numberOfChecks seed =
  let
      getCounterExample' seed currentNumberOfChecks =
        if currentNumberOfChecks >= numberOfChecks
        then
          Done (Ok ())
        else
          let
              (tree, nextSeed) =
                Random.generate investigator seed

              value =
                RoseTree.root tree

              actual =
                actualStatement value

              expected =
                expectedStatement value

          in
              if
                comparison actual expected
              then
                Continue (\() -> getCounterExample' nextSeed (currentNumberOfChecks + 1))
              else
                Done
                  (Err
                    { value           = value
                    , actual          = actual
                    , expected        = expected
                    , seed            = nextSeed
                    , numberOfChecks  = currentNumberOfChecks + 1
                    , shrinkTree      = tree
                    }
                  )
  in
      trampoline (getCounterExample' seed 0)


type alias ShrinkResult a =
  { value : a
  , depth : Int
  , totalNodesVisited : Int
  }

findSmallestShrink : (a -> Bool) -> RoseTree a -> ShrinkResult a
findSmallestShrink predicate tree =
  let
      findSmallestShrink' depth totalNodesVisited currentSmallest nodes =
        case split nodes of
          Nothing ->
            Done
              { value = currentSmallest
              , depth = depth
              , totalNodesVisited = totalNodesVisited
              }

          Just (head, tail) ->
            if
              predicate (RoseTree.root head)
            then
              Continue (\() -> findSmallestShrink' depth (totalNodesVisited + 1) currentSmallest nodes)
            else
              let
                  children =
                    RoseTree.children head
              in
                  if
                    Lazy.List.isEmpty children
                  then
                    Continue (\() -> findSmallestShrink' depth (totalNodesVisited + 1) (RoseTree.root head) tail)
                  else
                    Continue (\() -> findSmallestShrink' (depth + 1) (totalNodesVisited + 1) (RoseTree.root head) children)
  in
      trampoline (findSmallestShrink' 0 0 (RoseTree.root tree) (RoseTree.children tree))


makePredicate : (a -> b) -> (a -> b) -> (b -> b -> Bool) -> a -> Bool
makePredicate actualStatement expectedStatement comparison a =
  let
      actual =
        actualStatement a

      expected =
        expectedStatement a
  in
      comparison actual expected
