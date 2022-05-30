# Keelung Tutorial

Keelung is a *domain-specific language* (DSL) embedded in *Haskell* for writing high-level programs that can be compiled into **R1CS**.

R1CS (short for Rank-1 Constraint Systems) allows us to describes the relations and constraints between inputs and outputs of a computation. 

## Input

Keelung programs should have **at least one input**, so that we can have something to describe constraints with. 

Here's an example that does nothing other than returning its only **input** as its **output**.

```haskell
identity = do
    x <- inputVar
    return (Var x)
```
If we picture this program as a circuit of 2 pins, with `$0` as the input pin and `$1` as the output pin, then we expect the relation between `$0` and `$1` to be:

```
$0 = $1
```

And, indeed, this will be the constraint of the above program if we compile it into R1CS.

There can be more than one input:

```haskell
addition = do
    x <- inputVar
    y <- inputVar
    return (Var x + Var y)
```

This would result in the following constraint, with `$0` and `$1` as inputs and `$2` as the output.

```
$0 + $1 = $2 
```

It's also possible to have an array of inputs, but we'll talk about that later. 

## Output

Instead of returning an output like the above examples, a Keelung program can return nothing and have *side-effects* instead.

Just like the following *void function* in C:

```c
void hello()
{
    printf("Hello, World!");
    return;
}
```

In Keelung, we use `return unit` when we don't want a program to return anything.

```haskell
foo = do
    x <- inputVar
    y <- inputVar
    return unit 
```

## Assertions 

To make above example actually useful, we need to introduce some kind of side-effect in Keelung.
Since the purpose of a Keelung program is to *assert* relations and constraints between variables, we can do exactly just that!

```haskell
equal = do
    x <- inputVar
    y <- inputVar
    assert (Var x `Eq` Var y)
    return unit 
```

Instead of relating inputs to the output, we can let inputs relate to each other.

## Types 

If you have made it this far, and you've heard that Haskell is famous for it's *type system*, then you'll probably wonder what are the **types** of all those constructs. 

### Expressions

Let's start with the *expressions* appeared in the above example:

```haskell
Var x :: Expr 'Num n
Var y :: Expr 'Num n
Eq    :: Expr 'Num n -> Expr 'Num n -> Expr 'Bool n
unit  :: Expr 'Unit n
```

All of their types start with an `Expr`, followed by their *kind* and *parameter*.

There are 3 *kinds* of *values* in Keelung, they are:
* `'Num`: *numbers*
* `'Bool`: *booleans*
* `'Unit`: nothing really

What about that *parameter* `n` in `Expr 'Num n`? Programs in Keelung are *polymorphic*, you can specifying the *type for numbers* by instantiating `n` with any type you want. 

For example, `GF181` is a *Galois field* with numbers ranging from `0` to `1552511030102430251236801561344621993261920897571225601`. So an expression of type `Expr 'Num GF181` should result in some number in that range.

### Values & Literals 

Instead of comparing 2 numbers from inputs, we can see if an input number equals to `420`:

```haskell
equalTo420 = do
    x <- inputVar
    assert (Var x `Eq` 420)
    return unit 
```

Here we have constructed an expression of with literals like `420`.
We can also replace ```(Var x `Eq` 420)``` with just `true` or `false`:

```haskell
true  :: Expr 'Bool n
false :: Expr 'Bool n
```

### Computation  

What about the types of the program itself? you may ask.

```haskell
identity :: Comp n (Expr 'Num n)
equal    :: Comp n (Expr 'Unit n)
```

`Comp` is a *monad* for *computation*. The first parameter `n` is the same as in `Expr`, while the second parameter like `(Expr 'Num n)` represents the type of *result* of this computation.

Take `return` for example:

```haskell
return :: a -> Comp n a
```

It converts whatever is given into a computation, it's useful when you want to return some result.

We can also "extract" the result from a computation, by using the monadic syntax we saw as in:

```
   x <- inputVar
```

Here, we are extracting the result from `inputVar` and bind it to variable `x`.
We can do this because we know that `inputVar` is returning something by looking at its type:

```haskell
inputVar :: Comp n (Ref ('V kind))
```

The type of `x` will be `Ref ('V ty)` in this example (we'll come back to this `Ref` later!).

We can also compose other Keelung computations like this: 

```haskell
sumBe420 = do
    result <- addition
    assert (Var result `Eq` 420)
    return unit 
```

It's also possible to extract result from functions like `assert`: 

```
    resultOfAssertion <- assert (Var result `Eq` 420)
```

But you will be disappointed by the result from `assert`:

```haskell
assert :: Expr 'Bool n -> Comp n ()
```

Because `resultOfAssertion` will be the value of type `()`, which has no information at all.
Functions with type like this are only good for making *side-effects*, and Haskell allows us to ignore the result of this kind of functions.

### References

Now that we have understood the typings of computations in Keelung.
Let's revisit and examine the remaining puzzle in `identity`:

```haskell
identity :: Comp n (Expr kind n)
identity = do
    x <- inputVar
    return (Var x)
```

Let's start with `inputVar`, it's a built-in function for requesting an input variable:


```haskell
inputVar :: Comp n (Ref ('V kind))
```

We can extract something of type `Ref ('V kind)` from `inputVar`.
Here, `Ref` stands for *references*:
if it has a `'V` like in this example, then it's a reference to a *variable*;
if it has a `'A` in the type, then it's a reference to an *array*.

In order to treat references like `x :: Ref ('V kind)` as expressions, you will need `Var` to dereference them:
 
```haskell
Var      :: Ref ('V kind) -> Expr kind n
```

### Arrays 

Instead only requesting variables one by one, we can request an array of variables at once.

```haskell
assertAllToBe420 :: Comp GF181 (Expr 'Unit GF181)
assertAllToBe420 = do
  -- request an array of length 8
  xs <- inputArray 8
  -- iterate through the array with indices from 0 to 7
  forM_ [0 .. 7] $ \i -> do
    -- read out the variable in array 'xs' at index 'i' 
    x <- access xs i
    -- dereference variable 'x'
    assert $ Var x `Eq` 420

  return unit
```

<!-- 

Here are the functions of interest in this example:

```haskell
inputArray :: Int -> Comp n (Ref ('A ('V 'Num)))
access :: Ref ('A ('V 'Num)) -> Int -> Comp n (Ref ('V 'Num))
``` -->


# DUMP
<!-- 
Sections for `### Values and literals`: 

This `420` is actually a shorthand for `Val (Number 420)`. 
Assuming we are targeting `GF181`, here's a step-by-step breakdown of what happened:

```haskell
     Number 420  :: Value 'Num GF181
Val (Number 420) :: Expr  'Num GF181
```

You can probably work out the type of `Val`, it's the constructor for making *expressions* from *values*:

```haskell
Val :: Value kind n -> Expr kind n
``` -->