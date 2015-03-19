Instructions
============

- to start local server, run `snapproject`
    - this is an executable put in your .cabal/bin or your sandbox after `cabal install`
- for tests, run `cabal test`
    - test only "fails" if there is a system exit failure. To see other stuff, run with option `cabal test --show-details=always` or `=failures` to print the results

Setup Notes
===========
- get updated cabal with `cabal update && cabal install cabal-install`
    - note where new cabal is installed to (.cabal/bin or local/bin etc)- prob need to do a bash alias/symlink/chg path to use new cabal
- optionally, use cabal sandbox (only for updated cabal) to create a virtualenv
    - install the dependencies in parallel `cabal sandbox init; cabal install -j10 --only-dependencies; cabal install`
