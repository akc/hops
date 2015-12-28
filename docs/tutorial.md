% HOPS Tutorial
% Anders Claesson
% 28 December 2015

# HOPS Tutorial

To get a feeling for the HOPS language and using its interpreter
let us look at a number of examples.

## Generating functions

The generating function, *f*, for the
[Fibonacci numbers](https://oeis.org/A000045) satisfies
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

The exponential generating function for the
[Bell numbers](https://oeis.org/A000110) is *exp(e<sup>x</sup>-1)* and we
can give that expression to `hops`:

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

Power series defined by trigonometric functions are fine too. Here's
how we might generate the Euler numbers:

```
$ hops --prec=11 'f=sec(x)+tan(x);laplace(f)'
f=sec(x)+tan(x);laplace(f) => {1,1,1,2,5,16,61,272,1385,7936,50521}
```

The [number of ballots](https://oeis.org/A000670) (ordered set partitions)
is most simply defined by its exponential generating function
*y=1/(2-e<sup>x</sup>)*:

```
$ hops --prec 10 'y=1/(2-exp(x));laplace(y)'
y=1/(2-exp(x));laplace(y) => {1,1,3,13,75,541,4683,47293,545835,7087261}
```

Alternatively, one can exploit that *y'=2y<sup>2</sup>-y*:

```
$ hops --prec 10 'y=1+integral(2*y^2-y);laplace(y)'
y=1+integral(2*y^2-y);laplace(y) => {1,1,3,13,75,541,4683,47293,545835,7087261}
```

Let *A* be the exponential generating function for the
[number of labeled interval orders](https://oeis.org/A079144).
Zagier [showed](
http://people.mpim-bonn.mpg.de/zagier/files/doi/10.1016/S0040-9383(00)00005-7/fulltext.pdf)
that *A(24x)=exp(x)T(x)* where *T* is the exponential
generating function for the seqence of
[Glaisher's *T* numbers](https://oeis.org/A002439). Moreover, the exponential
generating function for the aerated seqence of Glaisher's *T* numbers
is *sin(2x)/(2cos(3x))*. Putting this together we have an
HOPS-expression for the number of labeled interval orders:

```
$ hops 'g=sin(2*x)/(2*cos(3*x));T=laplacei(BISECT1(laplace(g)));laplace((exp(x)*T)@(x/24))'
g=sin(2*x)/(2*cos(3*x));T=laplacei(BISECT1(laplace(g)));laplace((exp(x)*T)@(x/24)) => {1,1,3,19,207,3451,81663}
```

## Simple sequence notation

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

$ hops '{0,1,8,27,...}' | sloane

S A000578 0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744,3375,
N A000578 The cubes: a(n) = n^3.

$ hops '{n^3}'
{n^3} => {0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744}
```

Note that the example above also illustrates that we can pipe the output
of `hops` through `sloane`; see <http://akc.is/sloane>.

The [number of integer compositions](https://oeis.org/A011782) of *n* is
1 if *n=0* and *2<sup>n-1</sup>* if *n>0*.  Here's how we might specify
that formula:

```
$ hops '{1,2^(n-1)}'
{1,2^(n-1)} => {1,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192}
```

Factorials are fine too. Here's the
[order of the alternating group](https://oeis.org/A001710):

```
$ hops --prec=12 '{1,1,n!/2}'
{1,1,n!/2} => {1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400}
```

## Composing programs

Using the special variable `stdin` we can compose programs:

```
$ hops 'f=1+(x+x^2)*f' | hops 'stdin/(1-x)'
f=1+(x+x^2)*f;f/(1-x) => {1,2,4,7,12,20,33,54,88,143,232,376,609,986,1596}
```

As a side note, one can show that HOPS programs form a monoid under this
type of composition.

Be aware that `hops` may have to rename variables when composing programs:

```
$ hops --prec=10 'f=1+(x+x^2)*f' | hops 'f=1/(1-2*x);f/(1-x*stdin)'
f=1+(x+x^2)*f;g=1+2*x*g;g/(1-x*f) => {1,3,8,21,54,137,344,857,2122,5229,12836}
```

## A more involved example

We shall use `hops` and [jq](https://stedolan.github.io/jq/) to
reimplement an idea due to [Thomas
Baruchel](https://github.com/baruchel/oeis-deconvolution). Assume that
we save the following bash script in a file named `deconv`.

```bash
norm="map (.*.) | add"
d=`jq -n "[$1] | length"`
N=`jq -n "[$1] | $norm | sqrt"`
hops --dump --prec=$d \
  | grep -E "^A[[:digit:]]{6} => {1(,-?[[:digit:]]+){$(($d-1))}}" \
  | hops --prec=$d "{$1}/stdin" \
  | hops --to-json \
  | jq -c "if (.seq | $norm) < $N then . else empty end" \
  | hops --from-json
```

Then `./deconv <sequence>` would find power series $G$ and $H$ such
that $F=G\cdot H$, where $F$ is the generating function for
`<sequence>`, $G$ is the generating function for some sequence in the
OEIS, and $H$ has "small" norm. Here, the norm of a finite sequence (or
polynomial, or truncated power series) is the sum of the squares of its
elements, and $H$ is said to have small norm if its norm is smaller than
the square root of the norm of $F$. For simplicity we further assume
that the constant term of $G$ is $1$. It should also be noted that using
`jq` this way comes with a caveat: In contrast with `hops`, `jq` only
supports IEEE 754 64-bit numbers, so rounding could occur for numbers with
more that 15 decimal digits.

As an alternative to restricting the norm of $H$ we could require that
$H$ be in the OEIS, and one way to accomplished that would be to replace the
last three lines in the above script with `sloane --filter`
(see [sloane](http://akc.is/sloane/)).

## OEIS A-numbers

OEIS A-numbers can be used directly in HOPS programs and they are
interpreted as ordinary generating functions. E.g. this is the
difference between the Catalan numbers
([A000108](https://oeis.org/A000108)) and the Motzkin numbers
([A001006](https://oeis.org/A001006)):

```
$ hops 'A000108-A001006'
A000108-A001006 => {0,0,0,1,5,21,81,302,1107,4027,14608,52988,192501,701065,2560806}
```

The first time you use A-numbers with `hops` you will be asked to run
`hops --update`. This will download `https://oeis.org/stripped.gz` and
unpack it into `.oeis-data/stripped` in your home
directory. Alternatively, you can do this by hand using `wget` and
`gunzip`, say, if you prefer.

## Misc transformations

HOPS knows about many of the
[transformations used by the OEIS](https://oeis.org/transforms.html).
As an example, the sequence [A067145](https://oeis.org/A067145)
claims to shift left under reversion:

```
S A067145 1,1,-1,3,-13,69,-419,2809,-20353,157199,-1281993,10963825,-97828031,
N A067145 Shifts left under reversion.
```

Let's test that claim:

```
$ hops 'REVERT(A067145)-LEFT(A067145)'
REVERT(A067145)-LEFT(A067145) => {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
```

## HOPS scripts

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

## Tagging

The `hops` command can assign tags to sequences:

```
$ printf "1,1,2,5,17,33\n1,1,2,5,19,34\n" | hops --tag 1
TAG000001 => {1,1,2,5,17,33}
TAG000002 => {1,1,2,5,19,34}
```

## Bash completion

A bash completion script can be obtained by running

~~~
hops --bash-completion-script `which hops`
~~~

This functionality is due to the excellent
[optparse-applicative](https://github.com/pcapriotti/optparse-applicative) library.
For more information see
<https://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion>.
