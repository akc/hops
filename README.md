# ![HOPS](https://github.com/akc/akc.github.io/raw/master/src/hops/images/hops.png) HOPS [![Build Status](https://travis-ci.org/akc/hops.svg)](https://travis-ci.org/akc/hops)

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

Or [cabal](https://www.haskell.org/cabal/):

```
$ cabal install hops
```

## Usage examples

### Fibonacci numbers

The generating function, *f*, for the Fibonacci numbers satisfies
*f=1+(x+x<sup>2</sup>)f*, and using `hops` we can get the
coefficient of *f* directly from this equation:

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

### HOPS program files

Sometimes it is useful be able to apply many transformations to the same
input. One way to achieve that is to write a little program with the
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

### Tagging sequences

```
$ printf "1,1,2,5,17,33\n1,1,2,5,19,34\n" | hops --tag 1
TAG000001 => {1,1,2,5,17,33}
TAG000002 => {1,1,2,5,19,34}
```

## The man page

For further information on usage see the
[man page](https://github.com/akc/hops/blob/master/hops.md).

## A grammar for HOPS programs

```
hops ::= prg { "\n" prg }

prg ::= cmd { ";" cmd }

cmd ::= expr0 | name "=" expr0

expr0 ::= expr0 ("+" | "-") expr0 | expr1

expr1 ::= expr1 ("*" | "/" | ".*" | "./") expr1 | expr2

expr2 ::= expr3 "^" expr2 | expr3

expr3 ::= ("-" | "+") expr3 | expr4 "!" | name "(" expr4 ")"
          | expr4 "@" expr4
          | expr4

expr4 ::= "x" | anum | tag | name | literal | "{" { terms } "}"
          | expr0

literal ::= int

int ::= digit { digit }

digit ::= "0" | "1" | ... | "9"

alpha ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"

alphanum ::= alpha | digit

name ::= alphanum { alphanum | "_" }

terms ::= cexpr0 { "," expr0 } ("..." | cexpr0 | fun)

fun ::= the same as cexpr0 except literal = linear

linear ::= int | int "*n"

cexpr0 ::= cexpr0 ("+" | "-") cexpr0 | cexpr1

cexpr1 ::= cexpr1 ("*" | "/") cexpr1 | cexpr2

cexpr2 ::= cexpr3 "^" cexpr2 | cexpr3

cexpr3 ::= ("+" | "-") cexpr3 | cexpr4 "!" | cexpr4

cexpr4 ::= literal | cexpr0
```

## Issues

Have you found a bug? Want to contribute to `hops`? Please open an issue
at <https://github.com/akc/hops/issues>.

## How to cite

```
@misc{hops,
  author = "Anders Claesson",
  title  = "HOPS: Hackable Operations on Power Series",
  year   =  2015,
  howpublished = "\url{http://akc.is/src/hops}"
}
```

## License

BSD-3: see the
[LICENSE](https://github.com/akc/hops/blob/master/LICENSE) file.
