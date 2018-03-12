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
Currently we support two (and a half) verifiers. 
 * cbmc (we use `--error-label ERROR`)
 * klee
 * cpaChecker
 * Ultimate Automizer
 * Ultimate Taipan
 * vim (not a real verifier but useful for debugging. To accept a file close vim
   with `:q`, to reject a file use `:cq` to close with a non-zero exit code)

### Docker
In some cases (or should we rather say in most), it will not  be possible to execute all verifiers on your system. Klee, for example, needs a specific LLVM version to work.
To alleviate this problem and for your convenience we provide a Docker image with all the tools installed.

 * Make sure you have an up-to-date generated Dockerfile `make Dockerfile`
 * Build the docker image `docker build .`. The last line of the output should give you a hash.
 * You can execute the image with `docker run -it <hash> /bin/bash`. A few test-cases are located in `~/samples` so you can try it out right away.
