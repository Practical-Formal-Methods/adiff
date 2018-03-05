[![Build Status](https://travis-ci.com/chkl/vdiff.svg?token=LABbvVHJ7ARjnEncQ2vN&branch=master)](https://travis-ci.com/chkl/vdiff)

# vdiff
A simple tool to compare program verifiers.

## Build and Installation 
 * Make sure you have [stack](https://haskellstack.org) installed.
(For the impatient: `curl -sSL https://get.haskellstack.org/ | sh`). It is *not* necessary to have ghc, cabal or any other Haskell-related tool installed; `stack` will take care of everything.
 * When you are using `stack` for the first time run `stack setup` from inside the project
 * To build run `stack build`, if this is your first build it might take a while
 * To install run `stack install` which will copy the binary `vdiff` into `$HOME/.local/bin`, so make sure that this folder is in your $PATH variable.
 
 
## Usage
Run `vdiff --help` to learn more about how to use `vdiff`.

### Supported Verifiers
Currently we only support one (and a half) verifiers. 
 * cbmc (parses the last line of its stdout output)
 * vim (not a real verifier but useful for debugging. To accept a file close vim
   with `:q`, to reject a file use `:cq` to close with a non-zero exit code)
