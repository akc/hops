% The HOPS language
% Anders Claesson
% 12 December 2015

# The HOPS language

Each line of a HOPS script is an independent program and each
line/program consists of a semicolon separated list of functional
equations and generating functions. We shall now describe operations,
functions and transformations that can be used when building such
programs.

## Binary operations

Operation | Meaning
----------|-------------------------------------------------
`f + g`   | sum of *f* and *g*
`f - g`   | difference of *f* and *g*
`f ^ g`   | *f* to the power *g*
`f @ g`   | *f* composed with *g* (can also be written *f(g)* when *f* is a name)
`f ? g`   | coefficients of *f* selected by nonnegative integer coefficients of *g*
`f * g`   | product of *f* and *g*
`f / g`   | quotient of *f* and *g*
`f .* g`  | coefficient-wise/Hadamard product of *f* and *g*
`f ./ g`  | coefficient-wise quotient of *f* and *g*

## Derivative and integral

Operation   | Meaning
------------|--------------------------------------------
D(f)        | derivative of *f*
integral(f) | integral of *f*

## Functions

Function       | Meaning
---------------|----------------------------------------
`sqrt(f)`      | `f^(1/2)`
`abs(f)`       | `f` multiplied by the sign of its leading coefficient
`log(f)`       | logarithmic function
`exp(f)`       | exponential function
`sin(f)`       | sine function
`cos(f)`       | cosine function
`tan(f)`       | tangent function
`sec(f)`       | `1/cos(f)`
`arcsin(f)`    | arcsine function
`arccos(f)`    | arccosine function
`arctan(f)`    | arctangent function
`sinh(f)`      | hyperbolic sine function
`cosh(f)`      | hyperbolic cosine function
`tanh(f)`      | hyperbolic tangent function
`arsinh(f)`    | area hyperbolic sine function
`arcosh(f)`    | area hyperbolic cosine function
`artanh(f)`    | area hyperbolic tangent function
`laplace(f)`   | `f .* {n!}`
`laplacei(f)`  | `f ./ {n!}`
`revert(f)`    | the compositional inverse of *f*

## Transforms

Transform      | Meaning
---------------|----------------------------------------
`ABS(f)`       | coefficient-wise absolute value
`AERATE1(f)`   | `f(x^2)`
`AERATE2(f)`   | `f(x^3)`
`BARRY1(f)`    | `1/(1-x-x^2*f)`
`BARRY2(f)`    | `1/(1+x+x^2*f)`
`BINOMIAL(f)`  | `g=exp(x)*laplacei(f);laplace(g)`
`BINOMIALi(f)` | `g=exp(-x)*laplacei(f);laplace(g)`
`BIN1(f)`      | `g={(-1)^n/n!}*((laplacei(x*f))@(-x));LEFT(laplace(-g))`
`BISECT0(f)`   | if `f={a0,a1,a2,a3,a4,...}` then `BISECT0(f)={a0,a2,a4,...}`
`BISECT1(f)`   | if `f={a0,a1,a2,a3,a4,...}` then `BISECT1(f)={a1,a3,a5,...}`
`BOUS(f)`      | The [Boustrophedon transform](https://en.wikipedia.org/wiki/Boustrophedon_transform)
`BOUSi(f)`     | The inverse [Boustrophedon transform](https://en.wikipedia.org/wiki/Boustrophedon_transform)
`CATALAN(f)`   | `C=1+x*C^2;f@(x*C)`
`CATALANi(f)`  | `f@(x*(1-x))`
`CYC(f)`       | `sum[(phi(k)/k)*log(1/(1-f(x^k)))], k>0`; phi = totient function
`DIFF(f)`      | `LEFT(f)-f`
`EULER(f)`     | [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`EULERi(f)`    | inverse [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`EXP(f)`       | `g={1/n!}@(laplacei(x*f));laplace(g-1)/x`
`HANKEL(f)`    | [Hankel transform](https://cs.uwaterloo.ca/journals/JIS/VOL4/LAYMAN/hankel.html)
`lHANKEL(f)`   | `g=f.*f-LEFT(f).*RIGHT(f);LEFT(g)`
`I(f)`         | if `f={a_k}` then `I(f)={b_i}` where `b_(a_k)=1`; else `b_i=0`
`IC(f)`        | "Complement" of `I(f)`: substituting 0 for 1 and 1 for 0.
`LAH(f)`       | `g=(laplacei(f))@(x/(1-x));laplace(g)`
`LAHi(f)`      | `g=(laplacei(f))@(x/(1+x));laplace(g)`
`LEFT(f)`      | if `f={a0,a1,a2,a3,a4,...}` then `LEFT(f)={a1,a2,a3,...}`
`LOG(f)`       | `g=log(1+laplacei(x*f));LEFT(laplace(g))`
`MOBIUS(f)`    | The [Möbius transform](http://mathworld.wolfram.com/MoebiusTransform.html)
`MOBIUSi(f)`   | The inverse [Möbius transform](http://mathworld.wolfram.com/MoebiusTransform.html)
`MSET(f)`      | if `f={a_k}` then `MSET(f)=product[(1-x^k)^(-a_k)], k>0`
`PARTITION(f)` | See [[1](https://oeis.org/transforms.txt)] (Includes constant term)
`POINT(f)`     | `laplace(x*D(laplacei(f)))`
`PRODS(f)`     | if `f = {a0,a1,a2,...}` then `PRODS(f)={a0,a0*a1,a0*a1*a2,...}`
`PSET(f)`      | if `f={a_k}` then `PSET(f)=product[(1+x^k)^a_k], k>0`
`RIGHT(f)`     | `1+x*f`
`STIRLING(f)`  | `g=laplacei(x*f);laplace(g@({0,1/n!}))/x`
`STIRLINGi(f)` | `g=laplacei(x*f);laplace(g@({0,(-1)^(n+1)/n!}))/x`
`T019(f)`      | if `f={a[n]}` then `{a[n+2]-2*a[n+1]+a[n]}`
`TRISECT0(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a0,a3,a6,...}`
`TRISECT1(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a1,a4,a7,...}`
`TRISECT2(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a2,a5,a8,...}`
`WEIGHT(f)`    | if `f={a0,a1,a2,...}` then `WEIGHT(f)=(1+x^n)^a0*(1+x^n)^a1*...`

[1] <https://oeis.org/transforms.txt>

## A grammar for HOPS scripts

A HOPS script is a list of independent programs (`prg`) - one program
per line:

```
hops = prg { "\n" prg }
```

A program is a list of semicolon separated commands (`cmd`):

```
prg = cmd { ";" cmd }
```

A command is a generating function expression (`expr0`) or an assignment:

```
cmd = expr0 | name "=" expr0
```

We use the precedence climbing method to define generating function
expressions:

```
expr0 = expr0 ("+" | "-") expr0 | expr1

expr1 = expr1 ("*" | "/" | ".*" | "./") expr1 | expr2

expr2 = ("-" | "+") expr2 | expr3 "!" | expr3 "^" expr3 | expr3 "@" expr3 | expr3 "?" expr3 |expr3

expr3 = "x" | anum | tag | name | lit | "{" { terms } "}" | "[" { terms } "]" | name "(" expr3 ")" | expr0

lit = int

int = digit { digit }

digit = "0" | "1" | ... | "9"

alpha = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"

alphanum = alpha | digit

name = alphanum { alphanum | "_" }

terms = cexpr0 { "," expr0 } ("..." | cexpr0 | fun)

fun = the same as cexpr0 except lit = linear

linear = int | int "*n"

cexpr0 = cexpr0 ("+" | "-") cexpr0 | cexpr1

cexpr1 = cexpr1 ("*" | "/") cexpr1 | cexpr2

cexpr2 = ("+" | "-") cexpr2 | cexpr3 "!" | cexpr3 "^" cexpr3 | cexpr3

cexpr3 = lit | cexpr0
```
