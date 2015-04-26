Instructions
============

- to start local server, run `snapproject`
    - this is an executable put in your .cabal/bin or your sandbox after you build successfully
- the whole building process
    - `cabal configure --enable-tests --enable-benchmarks`
    - `cabal install --only-dependencies --enable-tests --enable-benchmarks --jobs=5`
    - `cabal build && cabal test --test-options="--color" --show-details=always`
    - `cabal build && cabal bench --benchmark-options="-o report.html"`
- notes:
    - if you don't run build, you are just testing old code...
    - whenever you need to do `:m +module` in ghci, you may need to add to the cabal build-dependency
        - unless that module is included in the base package
        - if it is in a different package, you may already have it, (thus can +m without errors from ghci) but still need to add to cabal package dependency list
        - if you completely don't have it in your sandbox, then need to add to dependency list and then cabal configure + install dependencies
    - `cabal repl` or `cabal repl my-tests` and then :m +your-modules to debug, and :r after changes
    - just using quickcheck with tests (without hspec) means that you need to manually trigger a system exit/failure
        - otherwise the test counts as "passed" even tough quickcheck prints failures out
        - To see passes, check log file or run with option `--show-details=always`
    - alternatively, you can try to do `runhaskell Spec.hs`, but
        - you need to run it from within src-test
        - it won't be able to import src-lib stuff
- parallel stuff
    - `ghc -O2 ParallelExercise.hs -threaded -rtsopts -eventlog && time ./ParallelExercise +RTS -N1 -s -l`
        - rarely, maybe add `-qg1` at the end to turn off garbage collection?

Setup Notes
===========
- get updated cabal with `cabal update && cabal install cabal-install`
    - note where new cabal is installed to (.cabal/bin or local/bin etc)- prob need to do a bash alias/symlink/chg path to use new cabal
- optionally, use cabal sandbox (only for updated cabal) to create a virtualenv
    - install the dependencies in parallel `cabal sandbox init; cabal install -j10 --only-dependencies; cabal install`
