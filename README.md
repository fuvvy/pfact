# putil : primality testing and prime factorization utility

`putil` is a small utility for testing primality and finding the prime decomposition of a given integer.

It has three major modes:
* `fac` find prime factorizations
* `mrt` probabilistic test for primality using ⌈log₄(n)⌉ rounds of the Miller-Rabin algorithm
* `llt` deterministic primality test for Mersenne numbers using the Lucas-Lehmer algorithm

These three modes accept positive integers only.

Mersenne primes are prime numbers of the form `2ᵖ-1`. In Lucas-Lehmer test mode it assumes you are passing only the Mersenne exponent `p`. Do not pass the actual value of the Mersenne number or you will be waiting for eons.

## Compiling

The `cmdargs`, `sscript` and `integer-logarithms` libraries are required.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cabal install cmdargs sscript integer-logarithms`

Compile with `-O2` and multi-core optimizations:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`ghc -o putil -O2 -threaded Main.hs`

## Running

If you compiled with multi-core support, run the `putil` utility with `+RTS -N ` appended to the command and the Haskell runtime will automatically detect the number of cores your system has and use them when possible.

### Examples

```
> putil fac 3895462145894328
2^3 x 3^3 x 18034546971733

> putil mrt 18848997157
Composite

> putil llt 110503
Prime
```

### Help message

```
putil v1.0
Primality testing and prime factorization utility

putil [COMMAND] ... [OPTIONS]
  Examples:
   putil fac 3895462145894328
   putil mrt 18848997157
   putil llt 110503

Common flags:
  -s --seed=INTEGER+    Provide your own seed
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

putil fac [OPTIONS] COMPOSITE+
  Find prime factors

putil mrt [OPTIONS] INTEGER+
  Test for primality using Miller-Rabin

putil llt [OPTIONS] EXPONENT+
  Test mersenne number for primality using Lucas-Lehmer
```
