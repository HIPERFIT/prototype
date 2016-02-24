Prototype is haskell library for integration contract language and parallel pricing engine.

Requirements
------------
GHC >= 7.8.3
In addition to libraries listed in .cabal file some system packages are required for hmatrix lib. Here is information on hmatrix requirements: https://github.com/albertoruiz/hmatrix/blob/master/INSTALL.md

Running
-------

Build pricing engine before running tests:
```
make compile_opencl
```

After that tests can be run using make:
```
make run_test
```
or using main function in Tests.hs.

Use `make run_web` to run web interface. Open `localhost:3000` in browser.

Use `make init_data` to initialize database with quotes for last 90 days for Apple and Google stocks from Yahoo finance API.

Alternatively, it's possible to run application using `cabal run web` or directly from `./dist/build/web/web` directory.

The following command-line options are accepted:

```
  -i       --initdata   Fetch quotes for AAPL and GOGL from public sources
  -p PORT  --port=PORT  Run server on specified port (3000 by default)
```

Emacs Haskell-mode users
------------------

Use `haskell-session-change-target` command to set proper target: `tests` when running tests/Tests.hs and `web` for web interface.
    
