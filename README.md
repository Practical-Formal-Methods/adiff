[![Build Status](https://travis-ci.com/chkl/vdiff.svg?token=LABbvVHJ7ARjnEncQ2vN&branch=master)](https://travis-ci.com/chkl/vdiff)

# vdiff
A simple tool to compare program verifiers.

## Build and Installation 
 * Make sure you have [stack](https://haskellstack.org) installed.
(For the impatient: `curl -sSL https://get.haskellstack.org/ | sh`). It is *not* necessary to have ghc, cabal or any other Haskell-related tool installed; `stack` will take care of everything.
 * When you are using `stack` for the first time run `stack setup` from inside the project
 * To build run `stack build`, if this is your first build it might take a while
 * To install run `stack install` which will copy the binaries `vdiff` and `vdiff-viewer` into `$HOME/.local/bin`, so make sure that this folder is in your $PATH variable.
 * In most use cases, `vdiff` will require certain tools to be installed on your system. We recommend to use the docker image, see below.
 
 
## Usage
Run the tools with `--help` to learn more.

### Supported Verifiers
Currently we support the following verifiers:

 * cbmc
 * klee
 * cpaChecker
 * Ultimate Automizer
 * Ultimate Taipan
 * Seahorn
 * Seacrab


### Docker
In some cases (or should we rather say in most), it will not be possible to
execute all verifiers on your system. Klee, for example, needs a specific LLVM
version to work. To alleviate this problem and for your convenience we provide a
Docker image with all the tools installed.

Just run `make` to build a docker image. The docker image will be tagged
`vdiff:latest`.

You can execute the image with `docker run -it <hash> /bin/bash`. A few
test cases are located in `~/samples` so you can try it out right away.

### How to get results
`vdiff` can be called with the parameter `--database <filename.db>` (or `-d`).
In this case, `vdiff` will store all runs and instrumented files into the given
sqlite3 database. Currently there are two tables: `runs` and `programs`. Note
that sqlite allows the database file to be opened by multiple programs
(concurrent writes though are only safe on file systems with proper file
locking).

For a quick overview you can use `vdiff-viewer`.

Example: `vdiff-viewer -d my.db --list --unsound`

# Frequently Asked Questions

### I don't understand how vdiff reaches its conclusions.
To understand what's going on you can always switch on additional information.
Use `--log-level=debug` for the highest verbosity. If `vdiff-viewer` shows a
incompletion or unsoudness you can use `vdiff-viewer -d database.db --hash
<hash> > tmp.c` to extract a specific test case with the given hash from the
database and write it into a file `tmp.c`. Invoke now the verifiers manually on
that file or use `vdiff --run --verifiers=<verifier> tmp.c` to let vdiff invoke
it for you.

### Are there more verifiers? 
`vdiff` has some built-in "verifiers" (quotation marks because they don't actually verify anything):

 * `vim`: To accept a file close vim
   with `:q`, to reject a file use `:cq` to close with a non-zero exit code)

  * `always-Sat`, `always-Unsat` : return the verdicts Sat (resp. Unsat) constantly.
