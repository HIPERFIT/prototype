The HIPERFIT Prototype is a web-based system that integrates the
HIPERFIT contract language and the HIPERFIT parallel pricing engine.

Requirements
------------
GHC >= 7.8.3

In addition to libraries listed in the .cabal file, some system
packages are required for the `hmatrix` library. For information on
`hmatrix` requirements, see
https://github.com/albertoruiz/hmatrix/blob/master/INSTALL.md

How to Run the Prototype
------------------------

Build the pricing engine before running the tests:
```
make compile_opencl
```

After building, test that it runs using make:
```
make run_test
```

As an alternative, use the main function in `Tests.hs`.

Use `make run_web` to run the web interface. Open `localhost:3000` in your browser (login: hiperfit, password: 123).

Use `make init_data` to initialize the database with quotes for the
last 90 days for Apple and Google stocks from the Yahoo finance API.

Alternatively, it is possible to run the application using `cabal run web` or directly from `./dist/build/web/web` directory.

The following command-line options are accepted:

```
  -i       --initdata   Fetch quotes for AAPL and GOGL from public sources
  -p PORT  --port=PORT  Run server on specified port (3000 by default)
```

Emacs Haskell-mode users
------------------------

Use `haskell-session-change-target` command to set properly the target
`tests`, when running tests/Tests.hs, and `web` for the web interface.
    
