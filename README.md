# Property Based Testing in Elm with elm-check

Property-based testing is a style of testing that focuses on making claims about your system and attempting to disprove these claims. The goal of `elm-check` is to automate this process.

The most common method of testing is unit-testing. Unit-testing consists in manually stating assertions that certain inputs yield certain outputs. While familiar, this approach has several drawbacks.

1. Writing assertions is a very boring and tedious process.
2. More often than not, the inputs chosen to test are completely arbitrary and may give little insight to the correctness of your system.
3. Even if a unit test did fail, the failed unit may or may not be helpful to diagnose your problem.

The way `elm-check` solves these problems is by

1. Automate the generation of unit tests
2. Use random number generation to explore an arbitrarily large sample of your input space
3. Compute a minimal failing case which is then more representative of the issue encountered in your system by the test.


# How it works

`elm-check` is centered around the idea of `claims` and `investigator`. You make a claim of truth about your system and have an investigator check the claim.

For example, suppose you wanted to test a function to reverse a list of elements.

```elm
reverse : List a -> List a
```

For this to be a correct `reverse` function, there are a number of properties that must hold true, such as:

1. Reversing a list twice yields the original list
2. Reversing does not modify the length of a list

You can make these claims in `elm-check` as follows:

```elm
claim_reverse_twice_yields_original =
  claim
    "Reversing a list twice yields the original list"
  `that`
    (\list -> reverse (reverse list))
  `is`
    (identity)
  `for`
    list int


claim_reverse_does_not_modify_length =
  claim
    "Reversing a list does not modify its length"
  `that`
    (\list -> length (reverse list))
  `is`
    (\list -> length list)
  `for`
    list int
```

As, you can see, `elm-check` defines a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for writing claims. The code reads very simply.

The first claim claims that reversing a list twice yields the original list. The string passed to claim is used for displaying, so make it as descriptive as necessary. Then `that` takes your actual statement about `reverse`, i.e. reversing it twice. `is` takes your expected statement about `reverse`, i.e. that it is the identity function. This is analogous to expected vs actual in unit testing. You expect that reversing a list twice is equivalent to not doing anything to the list. `for` then takes an investigator for the claim. In this case, `list int` will investigate the space of possible lists of integers and will try to disprove the claim.



You can then test each claim individually using the `quickCheck` function:

```elm
result1 = quickCheck claim_reverse_twice_yields_original
result2 = quickCheck claim_reverse_does_not_modify_length
```

Or you can group these claims in a suite and check the suite:

```elm
suite_reverse =
  suite "List Reverse Suite"
    [ claim_reverse_twice_yields_original
    , claim_reverse_does_not_modify_length
    ]

result = quickCheck suite_reverse
```

`quickCheck` will take either a single claim or a suite of claims and will run 100 checks on each claim to attempt to disprove each claim. `quickCheck` will then return a descriptive result of the checks performed.

You can then visualize these results using the `display` function:

```elm
main = display result
```

And, voila, in just a few lines of code, you have made two different claims about the `reverse` function and performed 200 checks and displayed the results.


### Simple example

Let us look at a very simple example to get a feel for how to use `elm-check`.

Suppose we wanted to test that multiplication and division are inverse operations.

You would make this claim as follows:

```elm
claim_multiplication_division_inverse =
  claim
    "Multiplication and division are inverse operations"
  `that`
    (\(x, y) -> x * y / y)
  `is`
    (\(x, y) -> x)
  `for`
    tuple (float, float)
```

Now, if you run `quickCheck` on this claim and displayed it in the browser with `display`, you would get:

```
Multiplication and division are inverse operations FAILED after 1 check!
  - Counter example: (0,0)
  - Actual: NaN
  - Expected: 0
```

This result shows that `elm-check` has found a counter example, namely `(0,0)`
which falsifies the claim. This is obviously true because division by 0 is undefined, hence the `NaN` value.

We can solve this issue by adding this condition to our actual statement and modify it as follows:

```elm
claim_multiplication_division_inverse =
  claim
    "Multiplication and division are inverse operations"
  `that`
    (\(x, y) -> if y == 0 then x else x * y / y)
  `is`
    (\(x, y) -> x)
  `for`
    tuple (float, float)
```

So, we added the condition where if y is 0, we simply return x. Now, let's see
what `elm-check` gives us now if we run `quickCheck`.

```
Multiplication and division are inverse operations FAILED after 1 check!
  - Counter example: (0.0001073802195855836,0.00013967437556471545)
  - Actual: 0.00010738021958558358
  - Expected: 0.0001073802195855836
```

Uh-oh, a new counter example. So, we can see that the actual and the expected values are incredibly close. From their closeness we can easily infer that something went wrong in the rounding. This is exactly what has happened as this is a floating-point error.

An interesting thing to note is that the counter example found was incredibly close to the original one of `(0,0)`. How come? The `float` investigator has the ability to generate any random float. So, what has happened here?

Well, to do this let us look back at the original claim:

```elm
claim_multiplication_division_inverse =
  claim
    "Multiplication and division are inverse operations"
  `that`
    (\(x, y) -> x * y / y)
  `is`
    (\(x, y) -> x)
  `for`
    tuple (float, float)
```

And this time, instead of displaying the results with `display`, let us use the alternative `displayVerbose` function which gives more detail about the test results.

Now, we get this output:

```
Multiplication and division are inverse operations FAILED after 1 check!
  - Counter example: (0,0)
  - Actual: NaN
  - Expected: 0
  - Seed: State 879767458 1052200661
  - Number of shrinking operations performed: 4
  - Before shrinking:
    - Counter example: (-14.074540141521613,-18.307399754018384)
    - Actual: -14.074540141521611
    - Expected: -14.074540141521613
```

From here we can see that there are a "seed", a "number of shrinking operations performed" and a "before shrinking" fields. The "seed" is there in order to reproduce test results. The "shrinking" stuff relates to a feature that `elm-check` provides called "shrinking".

Shrinking is the idea of shrinking a test case to a minimal representation. In this case, the investigator `tuple (float, float)` has found the original counter example `(-14.074540141521613,-18.307399754018384)`. It has then taken this counter example and has searched for another counter example that is more minimal that still disproves the claim. It has then repeated this process until it finds no more minimal counter example that still disproves the claim, in this case, simply `(0,0)`. Intuitively, `(0,0)` is as minimal an input as it gets. Having such minimal inputs is key to diagnosing problem.

As we have seen, this simple claim has enabled us to diagnose two gotchas about division. Get used to this as it is very common for individual claims to encounter multiple problems with a system.
