% The HOPS language
% Anders Claesson
% 3 April 2017

# The HOPS language

Each line of a HOPS script is an independent program and each
line/program consists of a semicolon separated list of functional
equations and generating functions. We shall now describe operations,
functions and transformations that can be used when building such
programs.

## Binary operations

Operation | Meaning
----------|-------------------------------------------------
`f + g`   | sum of `f` and `g`
`f - g`   | difference of `f` and `g`
`f ^ g`   | `f` to the power `g`
`f @ g`   | `f` composed with `g` (can also be written `f(g)` when `f` is a name)
`f ? g`   | coefficients of `f` selected by nonnegative integer coefficients of `g`
`f * g`   | product of `f` and `g`
`f / g`   | quotient of `f` and `g`
`f .* g`  | coefficient-wise/Hadamard product of `f` and `g`
`f ./ g`  | coefficient-wise quotient of `f` and `g`

## Functions

Function         | Meaning
-----------------|----------------------------------------
`diff(f)`        | derivative of `f`
`int(f)`         | integral of `f`
`sqrt(f)`        | `f^(1/2)`
`abs(f)`         | `f` multiplied by the sign of its leading coefficient
`log(f)`         | logarithmic function
`exp(f)`         | exponential function
`sin(f)`         | sine function
`cos(f)`         | cosine function
`tan(f)`         | tangent function
`sec(f)`         | `1/cos(f)`
`arcsin(f)`      | arcsine function
`arccos(f)`      | arccosine function
`arctan(f)`      | arctangent function
`sinh(f)`        | hyperbolic sine function
`cosh(f)`        | hyperbolic cosine function
`tanh(f)`        | hyperbolic tangent function
`arsinh(f)`      | area hyperbolic sine function
`arcosh(f)`      | area hyperbolic cosine function
`artanh(f)`      | area hyperbolic tangent function
`laplace(f)`     | `f .* {n!}`
`laplacei(f)`    | `f ./ {n!}`
`revert(f)`      | the compositional inverse of `f`
`absolute(f)`    | coefficient-wise absolute value
`bisect0(f)`     | if `f={a0,a1,a2,a3,a4,...}` then `bisect0(f)={a0,a2,a4,...}`
`bisect1(f)`     | if `f={a0,a1,a2,a3,a4,...}` then `bisect1(f)={a1,a3,a5,...}`
`bous(f)`        | [Boustrophedon transform](https://en.wikipedia.org/wiki/Boustrophedon_transform)
`bousi(f)`       | inverse [Boustrophedon transform](https://en.wikipedia.org/wiki/Boustrophedon_transform)
`cyc(f)`         | `sum[(phi(k)/k)*log(1/(1-f(x^k)))], k>0`; phi = totient function
`delta(f)`       | `shift(f)-f`
`euler(f)`       | [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`euleri(f)`      | inverse [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`hankel(f)`      | [Hankel transform](https://cs.uwaterloo.ca/journals/JIS/VOL4/LAYMAN/hankel.html)
`indicator(f)`   | if `f={a_k}` then `indicator(f)={b_i}` where `b_(a_k)=1`; else `b_i=0`
`indicatorc(f)`  | "complement" of `indicator(f)`: substituting 0 for 1 and 1 for 0
`shift(f)`       | if `f={a0,a1,a2,a3,a4,...}` then `shift(f)={a1,a2,a3,...}`
`mobius(f)`      | [Möbius transform](http://mathworld.wolfram.com/MoebiusTransform.html)
`mobiusi(f)`     | inverse [Möbius transform](http://mathworld.wolfram.com/MoebiusTransform.html)
`mset(f)`        | if `f={a_k}` then `mset(f)=product[(1-x^k)^(-a_k)], k>0`
`partition(f)`   | see <https://oeis.org/transforms.txt> (Includes constant term)
`point(f)`       | `laplace(x*diff(laplacei(f)))`
`prods(f)`       | if `f = {a0,a1,a2,...}` then `prods(f)={a0,a0*a1,a0*a1*a2,...}`
`pset(f)`        | if `f={a_k}` then `pset(f)=product[(1+x^k)^a_k], k>0`
`seq(f)`         | `1/(1-f)`
`T019(f)`        | if `f={a[n]}` then `{a[n+2]-2*a[n+1]+a[n]}`
`trisect0(f)`    | if `f={a0,a1,a2,a3,a4,...}` then `trisect0(f)={a0,a3,a6,...}`
`trisect1(f)`    | if `f={a0,a1,a2,a3,a4,...}` then `trisect0(f)={a1,a4,a7,...}`
`trisect2(f)`    | if `f={a0,a1,a2,a3,a4,...}` then `trisect0(f)={a2,a5,a8,...}`
`weight(f)`      | if `f={a0,a1,a2,...}` then `weight(f)=(1+x^n)^a0*(1+x^n)^a1*...`
`dirichlet(f,g)` | [Dirichlet convolution](https://en.wikipedia.org/wiki/Dirichlet_convolution)

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

expr3 = "x" | anum | tag | name | lit | "{" { terms } "}" | "[" { terms } "]" | name "(" expr0 { "," expr0 }  ")" | name expr3 | "(" expr0 ")"

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

<!--

Deleted transforms:

BARRY1(f)    == 1/(1-x-x^2*f)
BARRY2(f)    == 1/(1+x+x^2*f)
BIN1(f)      == shift((-{(-1)^n/n!} * (((x*f) ./ {n!})@(-x))) .* {n!})
BINOMIAL(f)  == (f ./ {n!}) * {1/n!} .* {n!}
BINOMIALi(f) == (f ./ {n!}) * {(-1)^n/n!} .* {n!}
CATALAN(f)   == C=1+x*C^2;f@(x*C)
CATALANi(f)  == f@(x*(1-x))
EXP(f)       == (({1/n!}@(x*f./{n!}) - 1) .* {n!})/x
lHANKEL(f)   == g=f.*f-shift(f).*(1+x*f);shift(g)
LOG(f)       == ({0,(-1)^(n+1)/n}@(x*f./{n!}) .* {n!})/x
LAH(f)       == (f./{n!})@(x/(1-x)) .* {n!}
LAHi(f)      == (f./{n!})@(x/(1+x)) .* {n!}
STIRLING(f)  == ((x*f ./ {n!})@({0,1/n!}) .* {n!})/x
STIRLINGi(f) == ((x*f ./ {n!})@({0,(-1)^(n+1)/n}) .* {n!})/x
RIGHT(f)     == 1+x*f

-->
