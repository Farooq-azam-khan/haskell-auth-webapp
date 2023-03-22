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
