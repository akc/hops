# ![HOPS](https://github.com/akc/akc.github.io/raw/master/hops/images/hops.png) HOPS [![Build Status](https://travis-ci.org/akc/hops.svg)](https://travis-ci.org/akc/hops)

Hackable Operations on Power Series.

## Install

The easiest way to get started is to download a prebuilt binary. Such
binaries can be found on the
[releases page](https://github.com/akc/hops/releases).
The binaries are statically linked and should work on any Linux system.

Alternative ways of installing `hops` include
using the [nix](https://nixos.org/nix/) package manager:

```
$ nix-env -f "<nixpkgs>" -iA haskellPackages.hops
```

Or using [cabal](https://www.haskell.org/cabal/):

```
$ cabal update && cabal install hops
```

## Introduction

To get a feeling for the HOPS language and using its interpreter (hops)
let us look at a few examples.

### Fibonacci numbers

The generating function, *f*, for the Fibonacci numbers satisfies
*f=1+(x+x<sup>2</sup>)f*, and we can get the coefficient of *f* directly
from that equation:

```
$ hops 'f=1+(x+x^2)*f'
f=1+(x+x^2)*f => {1,1,2,3,5,8,13,21,34,55,89,144,233,377,610}
```

Alternatively, we could first solve for *f* in *f=1+(x+x<sup>2</sup>)f*
and let `hops` expand that expression:

```
$ hops 'f=1/(1-x-x^2)'
f=1/(1-x-x^2) => {1,1,2,3,5,8,13,21,34,55,89,144,233,377,610}
```

### Catalan numbers

It could hardly be easier:

```
$ hops C=1+x*C^2
C=1+x*C^2 => {1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440}
```

### Bell numbers

The exponential generating function for the Bell numbers is
*e<sup>e<sup>x</sup>-1</sup>* and we can give that expression to
`hops`:

```
$ hops --prec=10 'exp(exp(x)-1)'
exp(exp(x)-1) => {1,1,1,5/6,5/8,13/30,203/720,877/5040,23/224,1007/17280}
```

To get the Bell numbers we, however, also need to multiply the
coefficient of *x<sup>n</sup>* in that series by *n!*; this is what
the laplace transform does:

```
$ hops --prec=10 'f=exp(exp(x)-1);laplace(f)'
f=exp(exp(x)-1);laplace(f) => {1,1,2,5,15,52,203,877,4140,21147}
```

### Euler numbers

Power series defined by trigonometric functions are fine too:

```
$ hops --prec=12 'f=sec(x)+tan(x);laplace(f)'
f=sec(x)+tan(x);laplace(f) => {1,1,1,2,5,16,61,272,1385,7936,50521,353792}
```

### Number of ballots (ordered set partitions)

This sequence most simply defined by its exponential generating function
*y=1/(2-e<sup>x</sup>)*:

```
$ hops --prec 10 'y=1/(2-exp(x)); laplace(y)'
y=1/(2-exp(x));laplace(y) => {1,1,3,13,75,541,4683,47293,545835,7087261}
```

Alternatively, one can exploit that *y'=2y<sup>2</sup>-y*:

```
$ hops --prec 10 'y = 1 + integral(2*y^2 - y); laplace(y)'
y=1+integral(2*y^2-y);laplace(y) => {1,1,3,13,75,541,4683,47293,545835,7087261}
```

### Simple sequence notation

We have seen how to define a few different sequences using generating
functions and functional equations. HOPS also supports a more naive way
of specifying sequences. Here's a simple finite sequence:

```
$ hops '{1,2,3}'
{1,2,3} => {1,2,3}
```

We can also use ellipses to build infinite sequences:

```
$ hops '{1,2,...}'
{1,2,...} => {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
```

What happened in the background here is that `hops` fitted the first
degree polynomial *p(n)=1+n* to the values *p(0)=1* and *p(1)=2*. We
could alternatively have given this formula explicitly:

```
$ hops '{1+n}'
{1+n} => {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
```

We are not limited to first degree polynomials either:

```
$ hops '{0,1,8,27,...}'
{0,1,8,27,...} => {0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744}

$ hops '{n^3}'
{n^3} => {0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744}
```

The number of integer compositions of *n* is 1 if *n=0* and
*2<sup>n-1</sup>* if *n>0*; see [A011782](https://oeis.org/A011782).
Here's how we might specify that formula:

```
$ hops '{1,2^(n-1)}'
{1,2^(n-1)} => {1,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192}
```

Factorials are fine too. Here's the order of the alternating group
([A001710](https://oeis.org/A001710)):

```
$ hops --prec=12 '{1,1,n!/2}'
{1,1,n!/2} => {1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400}
```

### Composing programs

Using the special variable `stdin` we can compose programs:

```
$ hops 'f=1+(x+x^2)*f' | hops 'stdin/(1-x)'
f=1+(x+x^2)*f;f/(1-x) => {1,2,4,7,12,20,33,54,88,143,232,376,609,986,1596}
```

As a side note, one can show that our programs form a monoid under this
type of composition.

Be aware that `hops` may have to rename variables when composing programs:

```
$ hops --prec=10 'f=1+(x+x^2)*f' | hops 'f=1/(1-2*x);f/(1-x*stdin)'
f=1+(x+x^2)*f;g=1+2*x*g;g/(1-x*f) => {1,3,8,21,54,137,344,857,2122,5229,12836}
```

### Misc transformations

HOPS knows about many of the transformations used by OEIS
<https://oeis.org/transforms.html>.

As an example, the sequences `A067145` claims to shift left under
reversion:

```
S A067145 1,1,-1,3,-13,69,-419,2809,-20353,157199,-1281993,10963825,-97828031,
N A067145 Shifts left under reversion.
```

Let's test that claim:

```
$ hops 'REVERT(A067145)-LEFT(A067145)'
REVERT(A067145)-LEFT(A067145) => {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
```

### HOPS scripts

Sometimes it is useful be able to apply many transformations to the same
input. One way to achieve that is to write a script with the
transformations we are interested in. E.g. if we create a file
`transforms.hops` containing

```
BINOMIAL(stdin)
EULER(stdin)
REVEGF(stdin)
STIRLING(stdin)
```
then we can apply all of these transforms to `1/(1-x)` as follows:

```
$ hops '1/(1-x)' | hops --prec=9 -f transforms.hops
f=1/(1-x);BINOMIAL(f) => {1,2,4,8,16,32,64,128,256}
f=1/(1-x);EULER(f) => {1,2,3,5,7,11,15,22,30}
f=1/(1-x);REVEGF(f) => {1,-2,9,-64,625,-7776,117649,-2097152,43046721}
f=1/(1-x);STIRLING(f) => {1,2,5,15,52,203,877,4140,21147}
```

N.B: As in this example, the preferred file extension for HOPS
program files is `.hops`.

## The HOPS language

Each line of a HOPS script is an independent program and each
line/program consists of a semicolon separated list of functional
equations and generating functions. We shall now describe operations,
functions and transformations that can be used when building such
programs.

### Binary operations

Operation | Meaning
----------|-------------------------------------------------
`f + g`   | sum of *f* and *g*
`f - g`   | difference of *f* and *g*
`f ^ g`   | *f* to the power *g*
`f @ g`   | *f* composed with *g* (can also be written *f(g)* when *f* is a name)
`f * g`   | product of *f* and *g*
`f / g`   | quotient of *f* and *g*
`f .* g`  | coefficient-wise/Hadamard product of *f* and *g*
`f ./ g`  | coefficient-wise quotient of *f* and *g*

### Derivative and integral

Operation   | Meaning
------------|--------------------------------------------
D(f)        | derivative of *f*
integral(f) | integral of *f*

### Functions

Function       | Meaning
---------------|----------------------------------------
`sqrt(f)`      | `f^(1/2)`
`abs(f)`       | coefficient-wise absolute value
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

### Transforms

Transform      | Meaning
---------------|----------------------------------------
`AERATE1(f)`   | `f(x^2)`
`AERATE2(f)`   | `f(x^3)`
`BARRY1(f)`    | `1/(1-x-x^2*f)`
`BARRY2(f)`    | `1/(1+x+x^2*f)`
`BINOMIAL(f)`  | `g=exp(x)*laplacei(f);laplace(g)`
`BINOMIALi(f)` | `g=exp(-x)*laplacei(f);laplace(g)`
`BIN1(f)`      | `g={(-1)^n/n!}*((laplacei(x*f))@(-x));LEFT(laplace(-g))`
`BISECT0(f)`   | if `f={a0,a1,a2,a3,a4,...}` then `BISECT0(f)={a0,a2,a4,...}`
`BISECT1(f)`   | if `f={a0,a1,a2,a3,a4,...}` then `BISECT1(f)={a1,a3,a5,...}`
`BOUS2(f)`     | See [[1](https://oeis.org/transforms.txt)]
`BOUS2i(f)`    | See [[1](https://oeis.org/transforms.txt)]
`BOUS(f)`      | See [[1](https://oeis.org/transforms.txt)]
`CONV(f)`      | `f^2`
`CONVi(f)`     | `sqrt(f)`
`DIFF(f)`      | `LEFT(f)-f`
`EULER(f)`     | [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`EULERi(f)`    | inverse [Euler transform](http://mathworld.wolfram.com/EulerTransform.html)
`EXPCONV(f)`   | `g=laplacei(f);laplace(g*g)`
`EXP(f)`       | `g={1/n!}@(laplacei(x*f));laplace(g-1)/x`
`HANKEL(f)`    | [Hankel transform](https://cs.uwaterloo.ca/journals/JIS/VOL4/LAYMAN/hankel.html)
`lHANKEL(f)`   | `g=f.*f-LEFT(f).*RIGHT(f);LEFT(g)`
`INVERT(f)`    | `LEFT(1/(1-x*f))`
`INVERTi(f)`   | `LEFT(-1/(1+x*f))`
`LAH(f)`       | `g=(laplacei(f))@(x/(1-x));laplace(g)`
`LAHi(f)`      | `g=(laplacei(f))@(x/(1+x));laplace(g)`
`LEFT(f)`      | if `f={a0,a1,a2,a3,a4,...}` then `LEFT(f)={a1,a2,a3,...}`
`LOG(f)`       | `g=log(1+laplacei(x*f));LEFT(laplace(g))`
`M2(f)`        | `2*f-f(0)`
`M2i(f)`       | `(f + f(0))/2`
`MOBIUS(f)`    | See [[1](https://oeis.org/transforms.txt)]
`MOBIUSi(f)`   | See [[1](https://oeis.org/transforms.txt)]
`NEGATE(f)`    | `(1-x/(1-x)).*f`
`PARTITION(f)` | See [[1](https://oeis.org/transforms.txt)]
`POINT(f)`     | `laplace(x*D(laplacei(f)))`
`PRODS(f)`     | if `f = {a0,a1,a2,...}` then `PRODS(f)={a0,a0*a1,a0*a1*a2,...}`
`PSUM(f)`      | `f/(1-x)`
`PSUMSIGN(f)`  | `f/(1+x)`
`REVERT(f)`    | `LEFT(revert(x*f))`
`REVEGF(f)`    | `LEFT(laplace(revert((x*f)./(1+x*laplace(1/(1-x))))))`
`RIGHT(f)`     | `1+x*f`
`STIRLING(f)`  | `g=laplacei(x*f);laplace(g@({0,1/n!}))/x`
`STIRLINGi(f)` | `g=laplacei(x*f);laplace(g@({0,(-1)^(n+1)/n!}))/x`
`T019(f)`      | if `f={a[n]}` then `{a[n+2]-2*a[n+1]+a[n]}`
`TRISECT0(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a0,a3,a6,...}`
`TRISECT1(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a1,a4,a7,...}`
`TRISECT2(f)`  | if `f={a0,a1,a2,a3,a4,...}` then `TRISECT0(f)={a2,a5,a8,...}`
`WEIGHT(f)`    | if `f={a0,a1,a2,...}` then `WEIGHT(f)=(1+x^n)^a0*(1+x^n)^a1*...`

[1] <https://oeis.org/transforms.txt>

### A grammar for HOPS scripts

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

expr2 = ("-" | "+") expr2 | expr3 "!" | expr3 "^" expr3 | expr3 "@" expr3 | expr3

expr3 = "x" | anum | tag | name | lit | "{" { terms } "}" | name "(" expr3 ")" | expr0

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

## The man page

The `hops` command has additional functionality such as the ability to
assign tags to sequences:

```
$ printf "1,1,2,5,17,33\n1,1,2,5,19,34\n" | hops --tag 1
TAG000001 => {1,1,2,5,17,33}
TAG000002 => {1,1,2,5,19,34}
```

For further information regarding command line options to `hops` see the
[man page](https://github.com/akc/hops/blob/master/hops.md).

## Issues

Have you found a bug? Want to contribute to `hops`? Please open an issue
at <https://github.com/akc/hops/issues>.

## How to cite

```
@misc{hops,
  author = "Anders Claesson",
  title  = "HOPS: Hackable Operations on Power Series",
  year   =  2015,
  howpublished = "\url{http://akc.is/hops}"
}
```

## License

BSD-3: see the
[LICENSE](https://github.com/akc/hops/blob/master/LICENSE) file.
