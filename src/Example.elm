import Check (..)
import Random (..)

import Text (plainText)


tests =
  check [
    property "No Identity" (\x -> x /= x) (float 0 2)
  ]

main = plainText tests
