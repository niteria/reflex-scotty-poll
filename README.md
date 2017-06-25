reflex-scotty-poll
==================

Simple poll app built with reflex-platform and scotty.
Stack, cabal, nix integration based on 
https://github.com/anderspapitto/nix-stack-ghcjs-demo.

Features
--------

- Runtime configured polls
- Questions form a directed graph, with paths dependent on the answer
- No back button (Google polls doesn't offer that)
- Custom background per question
- Optional image per question
  
Building
--------

1. Clone the repository
2. `git submodule update --init`
3. `./nix-front-build`
4. Open the last printed `file://` in the browser

Deployment
----------

Development build is about `5mb`, but is quick.
Release build is about `1mb`, but it takes a bit longer. 
https://www.npmjs.com/package/closurecompiler is required.

1. Build with `./nix-front-release`

Front end repl
--------------

1. `./nix-front-repl`

Known problems
--------------

- Poor diagnostics with badly formatted questions
