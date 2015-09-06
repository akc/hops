---
title: HOPS(1) User Manual | Version 0.0.2
date: 6 Sep 2015
---

# NAME

hops - hackable operations on power series.

# SYNOPSIS

`hops [--prec N] PROGRAMS...`  
`hops [--prec N] (-f|--script) FILENAME`  
`hops --tag N`  
`hops (--to-json|--from-json)`  
`hops (--list-transforms|--dump|--update|--version|--help)`

# DESCRIPTION

With `hops` one can generate integer (or rational) sequences from
programs containing power series and functional equations. See
<http://akc.is/hops> for more information about the HOPS
language and how to program in it.

# EXAMPLES

```
$ hops '1/(1-2*x)'
1/(1-2*x) => {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384}

$ hops 'f=1+(x+x^2)*f'
f=1+(x+x^2)*f => {1,1,2,3,5,8,13,21,34,55,89,144,233,377,610}

$ hops --prec=12 'f=sec(x)+tan(x);laplace(f)'
f=sec(x)+tan(x);laplace(f) => {1,1,1,2,5,16,61,272,1385,7936,50521,353792}

$ hops '1/(1-x)' | hops 'stdin^2'
f=1/(1-x);f^2 => {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
```

# OPTIONS

--prec N
:   Generating function precision [default: 15].

-f, --script FILENAME
:   Filename of script to run.

--tag N
:   Read sequences from stdin and tag them, starting at N.

--to-json
:   Convert to JSON.

--from-json
:   Convert from JSON.

--dump
:   Output all the sequences of the local data base.

--list-transforms
:   List the names of all transforms.

--update
:   Update the local database.

--version
:   Print version information.

--help
:   Briefly describe the available options.

# SEE ALSO
<http://akc.is/hops>

# AUTHOR

Anders Claesson, <http://akc.is>
