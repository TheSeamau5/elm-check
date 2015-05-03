module Check.Runner.Browser where

import Check exposing (Evidence (..), UnitEvidence, SuccessOptions, FailureOptions)
import Html  exposing (Html, div, text, ul, li)
import Html.Attributes
import List

display : Evidence -> Html
display evidence = case evidence of
  Unit unitEvidence ->
    displayUnit unitEvidence
  Multiple name evidences ->
    displaySuite name evidences

displaySuite : String -> List Evidence -> Html
displaySuite name evidences =
  li
    []
    [ text ("Suite: " ++ name)
    , ul
        []
        (List.map display evidences)
    ]



displayUnit : UnitEvidence -> Html
displayUnit unitEvidence = case unitEvidence of
  Ok options ->
    li
      []
      [ text (successMessage options) ]

  Err options ->
    li
      []
      [ text (failureMessage options) ]

successMessage : SuccessOptions -> String
successMessage {name, seed, numberOfChecks} =
  name
  ++ " passed after "
  ++ (toString numberOfChecks)
  ++ " checks."


failureMessage : FailureOptions -> String
failureMessage {name, counterExample, actual, expected, original, seed, numberOfChecks, numberOfShrinks} =
  "" ++  name ++ " failed after " ++ (toString numberOfChecks) ++ "!" ++
  "" ++ " Input: " ++ (toString counterExample) ++ " Actual: " ++ actual ++
  "" ++ " Expected: " ++ expected
