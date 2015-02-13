# Property Based Testing in Elm with elm-check

Property based testing is a way to generate tests for your code and automate the testing procedure. The problem with testing is that unit testing gets very tedious and unfortunately a large class of bugs manage to pass the unit tests simply because not enough unit tests were written. This is often true because in many cases it may be intractable to test all (or even a large sample) of the input space. As a result, property-based testing depends on the heavy use of random generators.


# How it works

elm-check is very simple. You specify the properties you wish to test and you call `check` on all these properties.

So, what is a property?

A property is a condition which your code must satisfy.

For example, a property of square roots is that the square root of a number squared is equal to the original number. In essence, square root is the inverse of square.

In elm-check, you can define this property as follows:

```elm
squareRootInverseProperty =
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

Simple, we use the `check` function.

`check` takes a list of properties and returns a string output. This output will tell you either that all tests have passed or it will lists the properties that have failed and point to the input that has made the property fail.

For example, were we to run the following code:

```elm
import Check (..)
import Random (..)

import Text (plainText)


tests =
  check [
    property "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
  ]

main = plainText tests
```


We would get :

```
Ok, passed all tests.
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
squareRootInverseProperty =
  propertyN 10000 "Square Root Inverse" (\number -> sqrt (number * number) == number) (float 0 100)
```

Now, this will generate 10,000 cases instead of 100.
