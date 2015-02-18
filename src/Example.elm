import Text (plainText)
import Random (..)
import Check (..)
import Signal (..)
import List

prop_numberIsIdentity : Property
prop_numberIsIdentity = propertyN 100 "number is identity" (\n -> n == n) (int 0 100)

prop_numberIsOdd : Property
prop_numberIsOdd = property "number is odd" (\n -> n % 2 == 1) (int 0 100)

prop_reverseReverseList : Property
prop_reverseReverseList =
  property "reverse reverse list" (\l -> List.reverse (List.reverse l) == l) (list 100 (int 0 100))

prop_discontinuous : Property
prop_discontinuous =
  property "discontinuous" (\x -> (x - 1) // (x - 1) == 1) (int 0 1000)

test : Signal TestOutput
test =
  continuousCheck
    [ prop_numberIsIdentity
    , prop_discontinuous
    , prop_numberIsOdd
    , prop_reverseReverseList
    ]


main =
  display <~ test
