reflex-scotty-poll
==================

Simple poll app built with reflex-platform and scotty.
Stack, cabal, nix integration based on 
https://github.com/anderspapitto/nix-stack-ghcjs-demo.

Demo
----

https://reflex-scotty-poll-demo.herokuapp.com/

Admin panel (in Polish currently, sorry): 
https://reflex-scotty-poll-demo.herokuapp.com/admin/

User: admin

Password: correct horse battery staple

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

Deployment to Heroku
--------------------

Done with the awesome https://github.com/mfine/heroku-buildpack-stack

Prerequisites:

0. Install heroku cli: https://devcenter.heroku.com/articles/heroku-cli#download-and-install
1. `heroku create --buildpack https://github.com/mfine/heroku-buildpack-stack.git`
or `heroku git:remote --app reflex-scotty-poll-demo` if you already have an app
2. (optional) `heroku apps:rename reflex-scotty-poll-demo`
3. `./heroku-build-deploy`

Front end repl
--------------

1. `./nix-front-repl`

Known problems
--------------

- Poor diagnostics with badly formatted questions
- Allows multiple submissions, makes a feeble attempt at keeping the identity
- No real protection from the client modifying the question structure, no proof-of-work
