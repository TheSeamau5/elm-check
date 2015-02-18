# Property Based Testing in Elm with elm-check

Property based testing is a way to generate tests for your code and automate the testing procedure. The problem with testing is that unit testing gets very tedious and unfortunately a large class of bugs manage to pass the unit tests simply because not enough unit tests were written. This is often true because in many cases it may be intractable to test all (or even a large sample) of the input space. As a result, property-based testing depends on the heavy use of random generators.


# How it works

elm-check is very simple. You specify the properties you wish to test and you call `simpleCheck` on all these properties.

So, what is a property?

A property is a condition which your code must satisfy.

For example, a property of square roots is that the square root of a number squared is equal to the original number. In essence, square root is the inverse of square.

In elm-check, you can define this property as follows:

```elm
prop_squareRootInverse =
  property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
```

`property` is a function that takes in 3 arguments, the name of a property (this is a useful name good for seeing the output and for documentation), the actual condition the property describes, and a random generator. We'll come back to the random generator in a bit, but let's focus on the condition.

The condition in the above example is represented by this function:

```elm
\number -> sqrt (number * number) == number
```

As you can see, the function is one that takes in a number and returns true if in fact the square root of the number squared is equal to the number itself.

While this is what you want to test, the question is, how does one supply values to this function? This is where the random generator comes into play.

The random generator used in this example is

```elm
float 0 100
```

This is a random generator that will generate a random float between 0 and 100. You may specify your generators to be as precise or as wide reaching as you need them to be.


Now that we have our property and understand how it works, how do we check it?

Simple, we use the `simpleCheck` function.

`simpleCheck` takes a list of properties and returns a `TestOutput`. This output will tell you either that all tests have passed or it will lists the properties that have failed and point to the input that has made the property fail. You can then either `print` the `TestOutput`, in which case you will get a string or you can
`display` the `TestOutput`, in which case you will get an `Element` (ideal for viewing in the browser).

For example, were we to run the following code:

```elm
import Check (..)
import Random (..)

import Text (plainText)


tests =
  simpleCheck [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = display tests
```


We would get :

```
Square Root Inverse has passed 100 tests!
```

But, if we just modified the function a teeny weeny bit and made it wrong:

```elm
(\number -> sqrt (number * number) + 1 == number)
```

We would get the following output:

```elm
Square Root Inverse has failed with the following input: 35.92545985847839
```

As you can see, this failure has been determined without having to resort to writing any unit tests manually. Basically, elm-check does that for you. It uses the random generator that you pass to it to generate as many test cases as it can. How many test cases does it generate?

Well, by default, `property` generates 100 test cases. If you need more or fewer test cases (which depends of the nature of the input data), you can use `propertyN` which takes an additional parameter specifying the number of test cases to generate.

So, we can rewrite the above property as :

```elm
prop_squareRootInverse =
  propertyN 1000 "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
```

Now, this will generate 1,000 cases instead of 100.

# Continuous checking

While, this strategy can find a number of bugs, certain bugs are quite hard to find.

For example, let's consider the following property:

```elm
prop_discontinuous : Property
prop_discontinuous =
  property "discontinuous" (\x -> (x - 1) // (x - 1) == 1) (int 0 10000)
```

This property tests if a number divided by itself is equal to 1 except that it has a slight twist,
the property has a discontinuity at `x == 1`. This means that this property should fail if `x` were
set to `1`.

Except, if we actually tried it, we get the result:

```
discontinuous has passed 100 tests!
```

This means that it has passed all the tests. In order to find the bug we would need to run it more than
100 times. One way is to change from `property` to `propertyN`, but then again, this is hard coding
the number of attempts elm-check would try.

Another, better, way would be to use `continuousCheck` as follows:

```elm
import Random (..)
import Check (..)
import Signal (..)


prop_discontinuous : Property
prop_discontinuous =
  property "discontinuous" (\x -> (x - 1) // (x - 1) == 1) (int 0 10000)

test : Signal TestOutput
test =
  continuousCheck
    [ prop_discontinuous
    ]


main =
  display <~ test
```

`continuousCheck` re-runs the test every second using the current time as a seed.

So, if we wait enough time, we go from this output:

```
discontinuous has passed 100 tests!
```

to this output:

```
discontinuous has failed with the following input: 1
```

Which is exactly what we wanted. Now, elm-check has found the bug even though
it was a pretty precise and edge case bug. The reason elm-check has found it
is because it ran several times using different seeds. This means that it has
had a change to try a larger portion of the input space. The longer the test
runs, the more input it tries. This means that if you run elm-check long enough
(say, a minute or so), and find no bugs, then you can be quite confident on the
strength and correctness of the properties you are testing.
