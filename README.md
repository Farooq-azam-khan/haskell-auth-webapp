# hauth Application 

# Setup 
1. Install `stack`
    * cli that scaffolds projects, downloads dependecies (including the compiler), run tests, build applications, etc. 
```bash 
$ brew install haskell-stack # for homebrew users
$ curl -sSL https://get.haskellstack.org/ | sh
$ wget -qO- https://get.haskellstack.org/ | sh
```
2. create a new project with `stack new [app-name]` 
3. `cd [app-name]`
4. `stack setup`
    * downloads the necessary depenencies including `GHC`
5. `stack build` 

# Run Application 
```bash 
stack exec [app-name]-exe 
```

# Run Tests 
```bash 
stack test 
```

# `app/` vs `src/` folder 
The `src` folder contains reusable library code while the `app` folder contains executable code. 

The code in `app` depends on the code in `src`

# `app/` vs `src/` folder 
The `src` folder contains reusable library code while the `app` folder contains executable code. 

The code in `app` depends on the code in `src`. 

# `stack.yaml` file
The `stack.yaml` file deines the configuration for `stack`. The `extra-deps` contains a list of Haskell packages that are not available in Stackage. 

# `hpack`
```bash
stack install hpack-convert
```
* Finds the `cabal` file in the current directory and converts that to `package.yaml` 
* The `package.yaml` file will build the `[app-name].cabal` file (cabal file added `.gitignore` file)
