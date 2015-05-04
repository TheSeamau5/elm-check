module Check.Runner.Browser
  ( display
  , displayVerbose
  ) where
{-| Browser test runner for elm-check. This module provides functions to
run and visualize tests in the browser.

# Display Test Results
@docs display, displayVerbose

-}

import Check exposing (Evidence (..), UnitEvidence, SuccessOptions, FailureOptions)
import Html exposing (Html, Attribute, div, text, ul, ol, li)
import Html.Attributes exposing (style)
import List


(:::) = (,)

type alias Style = List (String, String)


pomegranate     = "#c0392b"
backgroundBrown = "#DDCCA1"
midnightBlue    = "#2c3e50"
nephritis       = "#27ae60"

toColor : Bool -> String
toColor b =
  if b
  then
    nephritis
  else
    pomegranate

suiteStyle : Bool -> Style
suiteStyle b =
  [ "display"          ::: "flex"
  , "flex-direction"   ::: "column"
  , "color"            ::: toColor b
  ]



displayStyle : Style
displayStyle =
  [ "width"             ::: "100vw"
  , "min-height"        ::: "100vh"
  , "background-color"  ::: backgroundBrown
  ]

{-| Display test results in the browser.
-}
display : Evidence -> Html
display evidence =
  div
    [ style displayStyle ]
    [ display' False evidence ]


{-| Verbose version of `display`. Contains additional information
about the test results.
-}
displayVerbose : Evidence -> Html
displayVerbose evidence =
  div
    [ style displayStyle ]
    [ display' True evidence]

display' : Bool -> Evidence -> Html
display' b evidence = case evidence of
  Unit unitEvidence ->
    displayUnit b unitEvidence
  Multiple name evidences ->
    displaySuite b name evidences

displaySuite : Bool -> String -> List Evidence -> Html
displaySuite b name evidences =
  li
    [ style (suiteStyle (areOk evidences)) ]
    [ text ("Suite: " ++ name)
    , ol
        []
        (List.map (display' b) evidences)
    ]


isOk : Evidence -> Bool
isOk evidence = case evidence of
  Unit unitEvidence -> case unitEvidence of
    Ok _ -> True
    _ -> False
  Multiple _ evidences ->
    areOk evidences


areOk : List Evidence -> Bool
areOk evidences =
  List.all ((==) True) (List.map isOk evidences)


unitStyle : Bool -> Style
unitStyle b =
  [ "color"            ::: toColor b
  , "margin-bottom"    ::: "5px"
  , "margin-top"       ::: "10px"
  ]


unitInnerStyle : Style
unitInnerStyle =
  [ "color" ::: midnightBlue ]

displayUnit : Bool -> UnitEvidence -> Html
displayUnit b unitEvidence = case unitEvidence of
  Ok options ->
    li
      [ style (unitStyle True) ]
      [ text (successMessage options) ]

  Err options ->
    let
        essentialParts =
          [ li
              []
              [ text ("Counter example: " ++ options.counterExample) ]
          , li
              []
              [ text ("Actual: " ++ options.actual) ]
          , li
              []
              [ text ("Expected: " ++ options.expected)]

          ]

        verboseParts =
          if not b then []
          else
            [ li
                []
                [ text ("Seed: " ++ (toString options.seed.state)) ]
            , li
                []
                [ text ("Number of shrinking operations performed: " ++ (toString options.numberOfShrinks)) ]
            , li
                []
                [ text "Before shrinking: "
                , ul
                    []
                    [ li
                        []
                        [ text ("Counter example: " ++ options.original.counterExample) ]
                    , li
                        []
                        [ text ("Actual: " ++ options.original.actual) ]
                    , li
                        []
                        [ text ("Expected: " ++ options.original.expected) ]
                    ]
                ]
            ]
    in
        li
          [ style (unitStyle False) ]
          [ text (options.name ++ " FAILED after " ++ (toString options.numberOfChecks) ++ " check"++ (if options.numberOfChecks == 1 then "" else "s") ++ "!")
          , ul
              [ style unitInnerStyle ]
              (essentialParts ++ verboseParts)
          ]

successMessage : SuccessOptions -> String
successMessage {name, seed, numberOfChecks} =
  name ++ " passed after " ++ (toString numberOfChecks) ++ " checks."
