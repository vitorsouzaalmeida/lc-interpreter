[Functions describes the world](https://youtu.be/zHU1xH6Ogs4?si=DARVzaLnBgp5OAyx)!

Goal: build a Lambda Calculus Interpreter:
1. LC Interpreter (beta-reduction + alpha-conversion)
2. LC Interpreter (beta-reduction + De Bruijn index)
3. Simply Typed LC Interpreter (beta-reduction + De Bruijn index)

[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a simple programming language, and a model of computation (akin to Turing machines and recursive functions)

> By now this project covers only the Untyped lambda calculus. Take a look at the [Typed lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#Typed_lambda_calculus) later.

## Abstraction and application

It's composed only of abstractions and applications using variables (e.g. `(λx.x) y`). The whole language is made up of _lambda terms_.

Lambda terms can be one of these three things:

- A variable like `x` is a valid lambda term.

- An abstraction `(λx.y)`, or `(λx.t)`, where `x` is it's variable and `y`/`t` is another lambda term. `(λx.x)` could be written in JS as `const id = x => x`. Everything after the _dot_ works as the body of a function, so you could read `λx` as a function receiving `x` as variable. That's why `(λx.x)` is what we call identity function, becase it returns what it receives.

- An application `(t s)` where both `t` and `s` are lamba terms.

Since lambda calculus has only functions, we use [church encoding](https://en.wikipedia.org/wiki/Church_encoding) to model arithmetic, booleans and data structures by representing data types in the lambda calculus.

Church numerals let you represent natural numbers under Church encoding:

- 0 := `λf.λx. x`
- 1 := `λf.λx. f x`
- 2 := `λf.λx. f (f x)`
- 3 := `λf.λx. f (f (f x))`

Each numeral is a function that takes two arguments, `f` and `x`, and applies `f` _n_ times to `x`. That's how we know what is each number, by couting how many times `f` was applied to `x`.

It is just a [higher-order function](https://en.wikipedia.org/wiki/Higher-order_function), in Javascript it would look like this:
```typescript
const zero = f => x => x
const one = f => x => f(x)
const two = f => x => f(f(x))
const three = f => x => f(f(f(x))) 
```

If you're not familiar with the notation, you can read it as giving a function `f`, and a value `x`, apply `f` to `x`.

A successor function is quite similar. Since two is `x` applied to `f` two times, to know it's successor, you should apply `f` one more time. So, given a numeral `n`, it returns a numeral that applies `f` (n+1) times.

- succ := `λn.λf.λx.f (n f x)`

Let's break it into small pieces, because it's not that easy to understand at the first time.

```rust
1 := λf.λx. f x
succ := λn.λf.λx. f (n f x)

(λn.λf.λx. f (n f x)) (λf.λx. f x)
    (λf.λx. f ((λf.λx. f x) f x)) // now it is a function that expects f and x
    |           (λx. f x) x
    |               f x
    (λf.λx. f (f x)) // it just becomes the definition of two
```

By reducing the full expression, we're now seeing why succ (1 `λf.λx. f x`) returns 2 (`λf.λx. f (f x)`).

It's possible to do the same for [plus, mult, pow, pred, true, false, and, or, not, ...](https://en.wikipedia.org/wiki/Lambda_calculus#Arithmetic_in_lambda_calculus). Church encoding can also be used to create a pair (2-tuple), lists with their common functions like head, tail, cons...

## Evaluation and Reduction

The process of computing the value of a lambda term is what we call evaluation or reduction, and we will take a look at the β-reduction.

```rust
(λx.x + 1) 5
(5 + 1)
(6)
```

So we're applying a function to an argument by replacing the bound variable with that argument.

A more general rule could be defined as `(λx. M) N → M[x := N]`. Where `M` is the body of the abstraction, and `N`, it's argument. `M[x := N]` replaces all free instances of `x`, by `N` (I did a bunch of reductions by hand to understand well this process). But this rule requires alpha-conversion to avoid variable capure. It happens when a free variable in `N` becomes bound after substitution.

```rust
(λx. λy. x) y
     (λy. y)
```

It changed the meaning of our previous expression. the inner `y` in `(λy. x)` referred to the outer argument `y`. But after substitution, it became bound by the inner `λy`. So it no longer refers to the same variable

With alpha-conversion, we rename bound variables.

```rust
(λx. λy. x) y
(λx. λz. x) y
     (λz. y)
```

We're giving each scope a unique variable name.


```
AND TRUE FALSE
// remember TRUE λx.λy.x and FALSE λx.λy.y

(λp. λq. p q p) TRUE FALSE
     (λq. TRUE q TRUE) FALSE
     TRUE FALSE TRUE
     (λx. λy. x) FALSE TRUE
          (λy. FALSE) TRUE
               FALSE
```

[Try to reduce AND, OR, NOT, IF, etc...](https://en.wikipedia.org/wiki/Lambda_calculus#Logic_and_predicates)