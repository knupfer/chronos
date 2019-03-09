# Chronos

`chronos` performs lazy benchmarking of shell commands with continuous feedback and improving precision.

Please look at the documentation of the module for an overview of the api:
[Html](https://hackage.haskell.org/package/chronos-bench/docs/Chronos.html)

## How does it work?

`chronos` will benchmark the specified commands until you abort it
with ctrl-c or it reaches some termination criterion specified by the
command line.  It updates every iteration the measurements of the
benchmarks so you can terminate it when you're satisfied with the
presented precision.  It will intersperse all benchmarks for fastest
possible overview and to distribute any external load over all
benchmarks to improve precision.

![chronos](https://user-images.githubusercontent.com/5609565/54072918-5ec9ab00-4281-11e9-851a-7a4bde1295b2.png)

## Options

`chronos` presents rich command line options.

![options](https://user-images.githubusercontent.com/5609565/54072912-55d8d980-4281-11e9-909e-ea7c06a17d6a.png)

You can as well use autocomplete of your shell with following command:
```bash
source <(chronos --bash-completion-script `which chronos`)
```

Normally, the output of `--bash-completion-script` should be coppied in the appropriate directory.

## Library

You can use `chronos` as a haskell library to benchmark pure or impure functions or shell commands.

```haskell
module Main where

import Chronos

main :: IO ()
main = defaultMain
  [ bench "fib 1" fib 1
  , bench "fib 2" fib 2
  , bench "fib 4" fib 4
  , bench "fib 8" fib 8
  ]

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
```

```haskell
module Main where

import Chronos

import Control.Concurrent
import Data.IORef

main :: IO ()
main = defaultMain
  [ benchIO "ioref" (newIORef True)
  , benchIO "mvar"  (newMVar True)
  , benchIO "qsem"  (newQSem 5)
  , benchIO "chan"  (newChan :: IO (Chan Int))
  ]
```

```haskell
module Main where

import Chronos

main :: IO ()
main = defaultMain
  [ benchShell "sleep is slow"  "sleep 0"
  , benchShell "echo is fast"   "echo"
  , benchShell "true is faster" "true"
  ]
```

## Comparision

Comparing `chronos` to `bench` and `hyperfine`:

`chronos`
- intersperses all benchmarks, therefore allowing you fast comparisions
- uses scientific notation
- is more robust to external loads because ouf the interspersing (it will affect all benchmarks and not only some)
- uses a number of significant digits according to the current standard error (so with time more digits are presented and not always 4 which is often the wrong thing to do)
- uses bars with confidence intervals on the command line for easy comparision

Comparing `chronos` to `criterion`:

`chronos`
- has got a much simpler api
- can be used in testsuites
- has got much simpler internals
- has got less dependencies
- doesn't measure allocations
- doesn't measure cpu time
