import Check (..)
import Random (..)

import Text (plainText)


tests =
  check
  [ property "No Identity" (\x -> x /= x) (float 0 2)
  , property2 "Bad Addition Subtraction Inverse" (\a b -> (a - b - 1) == (a + (-b))) (float 0 100) (float 0 100)  
  ]

main = plainText tests
