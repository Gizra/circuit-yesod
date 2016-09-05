[![Build Status](https://travis-ci.org/Gizra/circuit-yesod.svg?branch=master)](https://travis-ci.org/Gizra/circuit-yesod)

# Circuit - Yesod

> Backend site for online auctions

## Installation

1. Download and install local copies of all the libraries needed by using [the Stack tool](https://github.com/commercialhaskell/stack/):
    ```
    stack install yesod-bin cabal-install --install-ghc && stack build
    ```    
1. create new postgresql database (one for the site, and one for the tests):

    ```
    sudo su - postgres
    psql template1
    CREATE USER root WITH PASSWORD 'root';
    CREATE DATABASE circuit;
    CREATE DATABASE circuit_test;
    GRANT ALL PRIVILEGES ON DATABASE circuit TO root;
    GRANT ALL PRIVILEGES ON DATABASE circuit_test TO root;
    \q
    ```

## Testing

Execute tests using `stack test`.
