module Check.Utils where

import Lazy.List exposing (LazyList, (:::), (+++))
import RoseTree exposing (RoseTree(..))
import Lazy exposing (Lazy, lazy, force)
import Random exposing (Generator)
import Random.Extra as Random
import Shrink exposing (Shrinker)
import Array exposing (Array)

-------------
-- HELPERS --
-------------

shrinkLazyList : LazyList (RoseTree a) -> RoseTree (LazyList a)
shrinkLazyList trees =
  let
      roots =
        Lazy.List.map RoseTree.root trees

      children =
        trees
        |> removeRoses
        |> Lazy.List.map shrinkLazyList

  in
      Rose roots children

toN : Int -> LazyList Int
toN n =
  if
    n < 0
  then
    Lazy.List.empty
  else
    natural
    |> Lazy.List.take (n + 1)


natural : LazyList Int
natural =
  Lazy.List.iterate ((+) 1) 0


removeRoses : LazyList (RoseTree a) -> LazyList (LazyList (RoseTree a))
removeRoses trees =
  Lazy.List.map (flip excludeNth trees) (toN (Lazy.List.length trees - 1))
  +++ rosePermutations trees

excludeNth : Int -> LazyList a -> LazyList a
excludeNth n list =
  Lazy.List.take n list +++ Lazy.List.drop (n - 1) list

rosePermutations : LazyList (RoseTree a) -> LazyList (LazyList (RoseTree a))
rosePermutations trees =
  Lazy.List.zip trees natural
    `Lazy.List.andThen` \(rose, index) -> RoseTree.children rose
    `Lazy.List.andThen` \child -> Lazy.List.singleton (update index child trees)





update : Int -> a -> LazyList a -> LazyList a
update n x list =
  let
      (first, rest) =
        splitAt n list

      newRest =
        case force list of
          Lazy.List.Nil ->
            x ::: Lazy.List.empty

          Lazy.List.Cons _ xs ->
            x ::: xs

  in
      first +++ newRest


split : LazyList a -> Maybe (a, LazyList a)
split list =
  case force list of
    Lazy.List.Nil ->
      Nothing

    Lazy.List.Cons x xs ->
      Just (x, xs)


splitAt : Int -> LazyList a -> (LazyList a, LazyList a)
splitAt n list =
  (Lazy.List.take n list, Lazy.List.drop n list)


replicateM : Int -> Generator a -> Generator (LazyList a)
replicateM m generator =
  if m <= 0
  then
    Random.constant Lazy.List.empty
  else
    generator
      `Random.andThen` \a -> replicateM (m - 1) generator
      `Random.andThen` \l -> Random.constant (a ::: l)


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

{- Turn a tree of generators into a generator of trees.
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


flattenGenerator : Generator (Generator a) -> Generator a
flattenGenerator genOfGens =
  Random.customGenerator <|
    \seed ->
        let
            (gen, seed2) = Random.generate genOfGens seed
            (value, seed3) = Random.generate gen seed2
        in
            (value, seed3)

{- Create a shrink tree from a shrinker and a value.
-}
shrinkTree : Shrinker a -> a -> RoseTree a
shrinkTree shrink a =
  Rose a (Lazy.List.map (shrinkTree shrink) (shrink a))


listAndThen : List a -> (a -> List b) -> List b
listAndThen =
  flip List.concatMap
