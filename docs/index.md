% HOPS
% Anders Claesson
% 1 May 2017

<h1 style="font-weight:normal; font-size:15px;">
![HOPS](hops.png)HOPS - Handy Operations on Power Series
</h1>
<hr/>

**HOPS** is a small domain specific language for working with
power series and integer sequences. It comes with an interpreter and
command line program called `hops`.

For instance, we can generate the sequence of
[Catalan numbers](https://oeis.org/A000108) from its generating function,
a functional equation, or an explicit formula for the *n*-th term:

```
$ hops --prec=11 '2/(1+sqrt(1-4*x))'
{
  "hops":"2/(1+sqrt(1-4*x))",
  "seq":[1,1,2,5,14,42,132,429,1430,4862,16796]
}
```
```
$ hops --prec=11 'C=1+x*C^2'
{
  "hops":"C=1+x*C^2",
  "seq":[1,1,2,5,14,42,132,429,1430,4862,16796]
}
```
```
$ hops --prec=11 '{(2*n)!/(n!*(n+1)!)}'
{
  "hops":"{(2*n)!/(n!*(n+1)!)}",
  "seq":[1,1,2,5,14,42,132,429,1430,4862,16796]
}
```

See the [documentation](#documentation) section below for more.

## Installation

Using the [nix](https://nixos.org/nix/) package manager:

```
$ nix-env -f "<nixpkgs>" -iA haskellPackages.hops
```

Or using [cabal](https://www.haskell.org/cabal/):

```
$ cabal update && cabal install hops
```

## Documentation

- [Tutorial](tutorial/)
- [Description of the HOPS language](language/)
- [Man page](man/)
- [Hackage](http://hackage.haskell.org/package/hops)

## Contribute

- [Issue tracker](https://github.com/akc/hops/issues) on GitHub
- [Source code](https://github.com/akc/hops) on GitHub
- You can also [contact me](http://akc.is/contact/) directly

## How to cite

```
@misc{hops,
  author = "Anders Claesson",
  title  = "HOPS: Handy Operations on Power Series",
  howpublished = "\url{http://akc.is/hops}"
}
```

## License

This project is licensed under a
[BSD license](https://github.com/akc/hops/blob/master/LICENSE).
