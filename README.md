# putil : primality testing, search and factorization utility

`putil` is a small tool for testing primality and finding the prime decomposition of a given integer as well as searching for primes of any given bit-length.

It has 4 major modes:
* `fac` find prime factorizations
* `mrt` probabilistic test for primality using ⌈log₄(n)⌉ rounds of the Miller-Rabin algorithm
* `llt` deterministic primality test for Mersenne numbers using the Lucas-Lehmer algorithm
* `fpp` pseudo-random search for probable primes of a given bit-length using Miller-Rabin

These 4 modes accept positive integers only.

Mersenne primes are prime numbers of the form `2ᵖ-1`. In Lucas-Lehmer test mode it assumes you are passing only the Mersenne exponent `p`. Do not pass the actual value of the Mersenne number or you will be waiting for eons.

In prime search mode, locating large primes can take a long time on desktop computers depending on the bit-length you query. Anything larger than `8192` bits will require a coffee break.

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

> putil fpp 64 3
[15223910619379001233,17858347140586802539,12182162899075669723]
```

### Help message

```
putil v1.0, (A) github.com/fuvvy
Primality testing and prime factorization utility

putil [COMMAND] ... [OPTIONS]
  Examples:
   putil fac 3895462145894328
   putil mrt 18848997157
   putil llt 110503
   putil fpp 1024 3

Common flags:
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

putil fac [OPTIONS] COMPOSITE-INTEGER
  Find prime factors

putil mrt [OPTIONS] TEST-INTEGER
  Test for primality using ⌈log₄(n)⌉ rounds of Miller-Rabin

putil llt [OPTIONS] MERSENNE-EXPONENT
  Test mersenne number for primality using Lucas-Lehmer

putil fpp [OPTIONS] BIT-LENGTH NUM-PRIMES
  Pseudo-random search for NUM-PRIMES probable primes of size BIT-LENGTH using
  Miller-Rabin
```
