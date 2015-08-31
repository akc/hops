#!/bin/sh

n=0

check () {
  n=$((n + 1))
  out=`eval $1`
  if [ "$out" = "$2" ]; then
      printf "Test #%d passed\n" $n
  else
      printf "Test #%d FAILED:\n\n" $n
      printf "COMMAND:\n%s\n\n" "$1"
      printf "EXPECTED OUTPUT:\n%s\n\n" "$2"
      printf "ACTUAL OUTPUT:\n%s\n\n" "$out"
      exit 1
  fi
}

check "hops --prec=12 'f=tanh(log(1+x)); laplace(f)'" "`cat <<EOF
f=tanh(log(1+x));laplace(f) => {0,1,-1,0,6,-30,90,0,-2520,22680,-113400,0}
EOF`"

check 'printf "1,2,3,4,5\n0,0,0,0,0,0\n" | hops --tag 1' "`cat <<EOF
TAG000001 => {1,2,3,4,5}
TAG000002 => {0,0,0,0,0,0}
EOF`"

check "hops --prec 10 {n!}" "{n!} => {1,1,2,6,24,120,720,5040,40320,362880}"

check "hops --prec 15 '1/(1-x)'" "1/(1-x) => {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}"

check "hops --prec 29 'laplace(tan(x)+sec(x))'" "laplace(tan(x)+sec(x)) => \
{1,1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,22368256,199360981,\
1903757312,19391512145,209865342976,2404879675441,29088885112832,\
370371188237525,4951498053124096,69348874393137901,1015423886506852352,\
15514534163557086905,246921480190207983616,4087072509293123892361,\
70251601603943959887872,1252259641403629865468285}"

check "hops --prec 10 A014307" \
"A014307 => {1,1,2,7,35,226,1787,16717,180560,2211181}"

check "hops --prec 10 'A=sqrt(exp(x)/(2-exp(x)));laplace(A)'" \
"A=sqrt(exp(x)/(2-exp(x)));laplace(A) => {1,1,2,7,35,226,1787,16717,180560,2211181}"

check "hops --prec 10 'A=1+integral(A^3*exp(-x));laplace(A)'" \
"A=1+integral(A^3*exp(-x));laplace(A) => {1,1,2,7,35,226,1787,16717,180560,2211181}"
