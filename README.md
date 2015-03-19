Instructions
============

- to start local server, run `snapproject`
    - this is an executable put in your .cabal/bin or your sandbox after you build successfully
- the whole building process
    - `cabal configure --enable-tests`
    - `cabal install --only-dependencies --enable-tests --jobs=5`
    - `cabal build && cabal test --test-options="--color --show-details=always"`
- notes:
    - if you don't run build, you are just testing old code...
    - just using quickcheck with tests means that you need to manually trigger a system exit/failure
        - otherwise the test counts as "passed" even tough quickcheck prints failures out
        - To see passes, check log file or run with option `--show-details=always`
    - alternatively, you can try to do `runhaskell Spec.hs`, but
        - you need to run it from within src-test
        - it won't be able to import src-lib stuff

Setup Notes
===========
- get updated cabal with `cabal update && cabal install cabal-install`
    - note where new cabal is installed to (.cabal/bin or local/bin etc)- prob need to do a bash alias/symlink/chg path to use new cabal
- optionally, use cabal sandbox (only for updated cabal) to create a virtualenv
    - install the dependencies in parallel `cabal sandbox init; cabal install -j10 --only-dependencies; cabal install`
