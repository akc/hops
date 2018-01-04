---
title: HOPS(1) User Manual | Version 0.8.3
date: 28 May 2017
---

# NAME

hops - handy operations on power series.

# SYNOPSIS

`hops [--prec N] PROGRAMS...`  
`hops [--prec N] (-f|--script) FILENAME`  
`hops --tag N`  
`hops (--dump|--update|--version|--help)`

# DESCRIPTION

With `hops` one can generate integer (or rational) sequences from
programs containing power series and functional equations. See
<http://akc.is/hops> for more information about the HOPS
language and how to program in it.

# OPTIONS

--prec N
:   Generating function precision [default: 15].

-f, --script FILENAME
:   Filename of script to run.

--tag N
:   Read sequences from stdin and tag them, starting at N.

--dump
:   Output all the sequences of the local data base.

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
