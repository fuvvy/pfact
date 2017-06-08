# pfact : prime factorization calculator

`pfact` is a tiny utility for finding the prime decomposition of a number using the very interesting Pollard's rho algorithm.

Simply pass it an arbitrary positive integer and the prime factorization will be output in a simplified format.

Take the composite integer `3895462145894328`, for example:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`2^3 x 3^3 x 18034546971733`

## Compiling

`pfact` uses the CmdArgs package so you will need to install that first.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cabal install cmdargs`

Compile with `-O2` and multi-core optimizations:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`ghc -O2 -threaded --make "pfact.hs"`

## Running

If you compiled with multi-core support, run the `pfact` utility with:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;` +RTS -N `

The Haskell runtime will automatically detect the number of cores your system has and use them when possible.

### Example

```
> pfact 3895462145894328 +RTS -N
2^3 x 3^3 x 18034546971733
```

#### Help message

```
pfact [OPTIONS] NUMBER

Common flags:
  -s --seed=SEED        Provide your own seed
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```