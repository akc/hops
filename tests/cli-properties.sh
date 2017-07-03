#!/bin/sh

n=0

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}dist/build"
hops=dist/build/hops/hops

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

check "$hops --prec=12 'f=tanh(log(1+x)); laplace(f)'" \
'{"hops":"f=tanh(log(1+x));laplace(f)","seq":[0,1,-1,0,6,-30,90,0,-2520,22680,-113400,0]}'

check 'printf "1,2,3,4,5\n0,0,0,0,0,0\n" | $hops --tag 1' \
'{"hops":"TAG000001","seq":[1,2,3,4,5]}
{"hops":"TAG000002","seq":[0,0,0,0,0,0]}'

check "$hops --prec 10 {n!}" \
'{"hops":"{n!}","seq":[1,1,2,6,24,120,720,5040,40320,362880]}'

check "$hops --prec 15 '1/(1-x)'" \
'{"hops":"1/(1-x)","seq":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]}'

check "$hops --prec 28 'laplace(tan(x)+sec(x))'" \
'{"hops":"laplace(tan(x)+sec(x))","seq":[1,1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,22368256,199360981,1903757312,19391512145,209865342976,2404879675441,29088885112832,370371188237525,4951498053124096,69348874393137901,1015423886506852352,15514534163557086905,246921480190207983616,4087072509293123892361,70251601603943959887872]}'

check "$hops --prec 10 A014307" \
'{"hops":"A014307","seq":[1,1,2,7,35,226,1787,16717,180560,2211181]}'

check "$hops --prec 10 'A=sqrt(exp(x)/(2-exp(x)));laplace(A)'" \
'{"hops":"A=sqrt(exp(x)/(2-exp(x)));laplace(A)","seq":[1,1,2,7,35,226,1787,16717,180560,2211181]}'

check "$hops --prec 10 'A=1+int(A^3*exp(-x));laplace(A)'" \
'{"hops":"A=1+int(A^3*exp(-x));laplace(A)","seq":[1,1,2,7,35,226,1787,16717,180560,2211181]}'

check "$hops --prec 12 'f=1+x*f(f-1);(f-1)/x'" \
'{"hops":"f=1+x*f(f-1);(f-1)/x","seq":[1,1,2,6,23,104,531,2982,18109,117545,808764]}'

# The order of the alternating group
check "$hops --prec=12 '{1,1,n!/2}'" \
'{"hops":"{1,1,n!/2}","seq":[1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400]}'

# Indecomposable perms: http://oeis.org/A003319
check "$hops --prec=13 '1-1/{n!}'" \
'{"hops":"1-1/{n!}","seq":[0,1,1,3,13,71,461,3447,29093,273343,2829325,31998903,392743957]}'

# Doubly indecomposable perms
check "$hops --prec=11 'f={n!};3-2/f-f'" \
'{"hops":"f={n!};3-2/f-f","seq":[0,1,0,0,2,22,202,1854,17866,183806,2029850]}'

# https://oeis.org/A059373
check "$hops --prec 13 'revert({0,n!})^2'" \
'{"hops":"revert({0,n!})^2","seq":[0,0,1,-4,8,-16,12,-96,-480,-4672,-45520,-493120,-5798912]}'

# https://oeis.org/A046912
check "$hops 'A=2-1/laplacei(A000798);laplace(A)'" \
'{"hops":"A=2-1/laplacei(A000798);laplace(A)","seq":[1,1,2,11,147,3412,121553,6353629,476850636,50811255045,7636459252135,1610584897516674,474333338553730879,194055026319667963777,109692570582311591696890]}'

# https://oeis.org/A051296
check "$hops --prec 12 '1/(1-{0,n!})'" \
'{"hops":"1/(1-{0,n!})","seq":[1,1,3,11,47,231,1303,8431,62391,524495,4960775,52223775]}'

# https://oeis.org/A002538
check "$hops --prec 15 'f=(x+2*log(1-x))/(x-1)^3;laplace(f)'" \
'{"hops":"f=(x+2*log(1-x))/(x-1)^3;laplace(f)","seq":[0,1,8,58,444,3708,33984,341136,3733920,44339040,568356480,7827719040,115336085760,1810992556800,30196376985600]}'

# https://oeis.org/A233389
check "$hops --prec=16 'T=1+x*T^3;(T-2)*T^3/(T^2-3*T+1)'" \
'{"hops":"T=1+x*T^3;(T-2)*T^3/(T^2-3*T+1)","seq":[1,1,3,11,46,209,1006,5053,26227,139726,760398,4211959,23681987,134869448,776657383,4516117107]}'

# https://oeis.org/A006789
check "$hops --prec=20 'B=1/(1-x-x^2*B(x/(1-x))/(1-x))'" \
'{"hops":"B=1/(1-x-x^2*B(x/(1-x))/(1-x))","seq":[1,1,2,5,14,43,143,509,1922,7651,31965,139685,636712,3020203,14878176,75982829,401654560,2194564531,12377765239,71980880885]}'

# https://oeis.org/A036987
check "$hops --prec 32 'f=1+x*f(x^2)'" \
'{"hops":"f=1+x*f(x^2)","seq":[1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]}'

# Catalan transform of 2^n gives central binomials
check "$hops --prec=20 'C=1+x*C^2;{2^n}@(x*C)-{(2*n)!/(n!)^2}'" \
'{"hops":"C=1+x*C^2;{2^n}@(x*C)-{(2*n)!/(n!)^2}","seq":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}'

# Inverse Catalan transform of central binomials gives 2^n
check "$hops --prec=20 'f={(2*n)!/(n!)^2};{2^n}-f@(x*(1-x))'" \
'{"hops":"f={(2*n)!/(n!)^2};{2^n}-f@(x*(1-x))","seq":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}'

# Square root of Goldbach's conjecture
check "$hops --prec=30 'sqrt(A002372)'" \
'{"hops":"sqrt(A002372)","seq":[0,1,1,1,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0]}'

# Central binomial coefficients as the square root of powers of 4
check "$hops --prec=30 'sqrt(A000302)-A000984'" \
'{"hops":"sqrt(A000302)-A000984","seq":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}'

# https://oeis.org/A079144 -- Number of labeled interval orders
check "$hops --prec=32 \
'T=laplacei(bisect1(laplace(sin(2*x)/(2*cos(3*x)))));laplace((exp(x)*T)@(x/24))'" \
'{"hops":"T=laplacei(bisect1(laplace(sin(2*x)/(2*cos(3*x)))));laplace((exp(x)*T)@(x/24))","seq":[1,1,3,19,207,3451,81663,2602699,107477247,5581680571,356046745023,27365431508779,2494237642655487,266005087863259291,32815976815540917183,4636895313201764853259]}'

check "$hops --forall --prec=20 'stdin' | head -5" \
'{"hops":"f=A000001;f","seq":[0,1,1,1,2,1,2,1,5,2,2,1,5,1,2,1,14,1,5,1]}
{"hops":"f=A000002;f","seq":[1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1]}
{"hops":"f=A000003;f","seq":[1,1,1,1,2,2,1,2,2,2,3,2,2,4,2,2,4,2,3,4]}
{"hops":"f=A000004;f","seq":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}
{"hops":"f=A000005;f","seq":[1,2,2,3,2,4,2,4,3,4,2,6,2,4,4,5,2,6,2,6]}'

# Polynomial notation
check "$hops --prec=10 '1/(1-[0,1,1])'" \
'{"hops":"1/(1-[0,1,1])","seq":[1,1,2,3,5,8,13,21,34,55]}'

# Extracting coeffs
check "$hops --prec=10 '{n+1}?0'" \
'{"hops":"{n+1}?0","seq":[1]}'

check "$hops --prec=10 '{n+1}?[1,5]'" \
'{"hops":"{n+1}?[1,5]","seq":[2,6]}'

check "$hops --prec=10 '{n+1}?17'" \
'{"hops":"{n+1}?17","seq":[]}'

check "$hops --prec=10 '{n+1}?(1/(1-2*x))'" \
'{"hops":"{n+1}?(1/(1-2*x))","seq":[2,3,5,9]}'

check "$hops --prec=10 '{n+1}?(1/x)'" \
'{"hops":"{n+1}?(1/x)","seq":[]}'

# https://oeis.org/A034695
check "$hops --prec=15 'dirichlet(A000005,A007426)' " \
'{"hops":"dirichlet(A000005,A007426)","seq":[1,6,6,21,6,36,6,56,21,36,6,126,6,36,36]}'
