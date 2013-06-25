# hs-tic-tac-toe 

## An AI Tic Tac Toe to be played locally or over a server

### Installation:

Install [leiningen](http://leiningen.org), then clone this project:

    git clone git://github.com/richardharrington/hs-tic-tac-toe.git

### Run locally:

    cd <path-to>/hs-tic-tac-toe
    lein repl
    (play-game-local pick-square-minimax pick-square-minimax)

You can also use `pick-square-heuristic` or `pick-square-random` in place of either or both of those two arguments, to test out the various algorithms.

### Run over the server:

    cd <path-to>/hs-tic-tac-toe
    lein repl
    (play-request pick-square-minimax)

You can also use `pick-square-heuristic` or `pick-square-random` for the argument, which is the algorithm the local player will use. To find another player, have them connect to the server (currently http://thomasballinger.com:8001, with API instructions at https://github.com/eriktaubeneck/tictax), or simply start another repl yourself and have your two repls play against each other.

And, of course, if it's no longer deployed to the address above, or you want to see the output from the server, you can run it locally. You have to install Python and Flask, then clone the directory above and run it.