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

check "hops --prec 12 'f=1+x*f(f-1);(f-1)/x'" \
"f=1+x*f(f-1);(f-1)/x => {1,1,2,6,23,104,531,2982,18109,117545,808764,5862253}"

# The order of the alternating group
check "hops --prec=12 '{1,1,n!/2}'" \
"{1,1,n!/2} => {1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400}"

# Indecomposable perms: http://oeis.org/A003319
check "hops --prec=13 '1-1/{n!}'" \
"1-1/{n!} => {0,1,1,3,13,71,461,3447,29093,273343,2829325,31998903,392743957}"

# Doubly indecomposable perms
check "hops --prec=11 'f={n!};3-2/f-f'" \
"f={n!};3-2/f-f => {0,1,0,0,2,22,202,1854,17866,183806,2029850}"

# https://oeis.org/A059373
check "hops --prec 13 'revert({0,n!})^2'" \
"revert({0,n!})^2 => {0,0,1,-4,8,-16,12,-96,-480,-4672,-45520,-493120,-5798912}"

# https://oeis.org/A046912
check "hops 'A=2-1/laplacei(A000798);laplace(A)'" \
"A=2-1/laplacei(A000798);laplace(A) => {1,1,2,11,147,3412,121553,6353629,476850636,\
50811255045,7636459252135,1610584897516674,474333338553730879,194055026319667963777,\
109692570582311591696890}"

check "hops --dump --prec=20 | head -5" \
"A000001 => {0,1,1,1,2,1,2,1,5,2,2,1,5,1,2,1,14,1,5,1}
A000002 => {1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1}
A000003 => {1,1,1,1,2,2,1,2,2,2,3,2,2,4,2,2,4,2,3,4}
A000004 => {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
A000005 => {1,2,2,3,2,4,2,4,3,4,2,6,2,4,4,5,2,6,2,6}"

check "hops --dump | hops --to-json | head -5" \
'{"seq":"{0,1,1,1,2,1,2,1,5,2,2,1,5,1,2}","prg":"A000001"}
{"seq":"{1,2,2,1,1,2,1,2,2,1,2,2,1,1,2}","prg":"A000002"}
{"seq":"{1,1,1,1,2,2,1,2,2,2,3,2,2,4,2}","prg":"A000003"}
{"seq":"{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}","prg":"A000004"}
{"seq":"{1,2,2,3,2,4,2,4,3,4,2,6,2,4,4}","prg":"A000005"}'

check "hops --dump | hops --to-json | hops --from-json | head -5" \
"A000001 => {0,1,1,1,2,1,2,1,5,2,2,1,5,1,2}
A000002 => {1,2,2,1,1,2,1,2,2,1,2,2,1,1,2}
A000003 => {1,1,1,1,2,2,1,2,2,2,3,2,2,4,2}
A000004 => {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
A000005 => {1,2,2,3,2,4,2,4,3,4,2,6,2,4,4}"
