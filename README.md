[Functions describes the world](https://youtu.be/zHU1xH6Ogs4?si=DARVzaLnBgp5OAyx)!

Have you ever thought if exists a language composed only of abstractions (aka functions), and a way of applying things to these abstractions, using variables, and nothing more? No numbers, no keywords, but keeping you able to do any kind of computation, like creating any logic operator, playing with booleans and do math? 

This language is called [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus). It's composed only of abstractions and applications using variables (e.g. `(λx.x) y`). The whole language is made up of _lambda terms_. 

Lambda terms can be one of these three things:

- A variable like `x` is a valid lambda term.

- An abstraction `(λx.y)`, or `(λx.t)`, where `x` is it's variable and `y`/`t` is another lambda term. `(λx.x)` could be written in JS as `const id = x => x`. Everything after the _dot_ works as the body of a function, so you could read `λx` as a function receiving `x` as variable. That's why `(λx.x)` is what we call identity function, becase it returns what it receives.

- An application `(t s)` where both `t` and `s` are lamba terms.

Since lambda calculus has only one primivite data type (functions), we must use church encoding to model arithmetic, booleans and data structures.

[Church encoding](https://en.wikipedia.org/wiki/Church_encoding) is a way of representing data types in the lambda calculus. Church numerals let you represent natural numbers under Church encoding:

- 0 := `λf.λx. x`
- 1 := `λf.λx. f x`
- 2 := `λf.λx. f (f x)`
- 3 := `λf.λx. f (f (f x))`

> dot notation (λx. t) is often used to indicate scope λx.x x means (λx.(x x)), which is not (λx.x) x. 
> ```javascript
>// λx.x x
>const f = x => x(x)
>
>// (λx.x) x
>const f = x => x
>```

Each numeral is a function that takes two arguments, `f` and `x`, and applies `f` _n_ times to `x`. That's how we know what is each number, by couting how many times `f` was applied to `x`.

It is just a [higher-order function](https://en.wikipedia.org/wiki/Higher-order_function), in Javascript it would look like this:
```javascript
const zero = f => x => x
const one = f => x => f(x)
const two = f => x => f(f(x))
const three = f => x => f(f(f(x))) 
```

If you're not familiar with the notation, you can read it as giving a function `f`, and a value `x`, apply `f` to `x`.

A successor function is quite similar. Since two is `x` applied to `f` two times, to know it's successor, you should apply `f` one more time. So, given a numeral `n`, it returns a numeral that applies `f` (n+1) times.

- succ := `λn.λf.λx.f (n f x)`

What may be hard to see at the first time, so let's break it into small pieces:

```
1 := λf.λx. f x
succ := λn.λf.λx. f (n f x)

(λn.λf.λx. f (n f x)) (λf.λx. f x)

// apply succ to the numeral
(λn.λf.λx. f (n f x)) (λf.λx. f x) // n becomes (λf.λx. f x)
    (λf.λx. f ((λf.λx. f x) f x)) // now it is a function that expects f and x
    |           (λx. f x) x
    |               f x
    (λf.λx. f (f x)) // it just becomes the definition of two
```

By reducing the full expression, we're now seeing why succ (1 `λf.λx. f x`) returns 2 (`λf.λx. f (f x)`).

It's possible to do the same for [plus, mult, pow, pred, true, false, and, or, not, ...](https://en.wikipedia.org/wiki/Lambda_calculus#Arithmetic_in_lambda_calculus)