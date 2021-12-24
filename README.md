monad-transformers
---

A quick tutorial on Monad Transfomers based on Chris Alen's [monad-transformers-step-by-step](https://github.com/bitemyapp/monad-transformers-step-by-step) 

## Prerequisit

- [nixpkg](https://github.com/NixOS/nixpkgs) 

[cookiecutter](https://cookiecutter.readthedocs.io/en/1.7.2/) template from [hls-nix-template](https://github.com/utdemir/hs-nix-template) was used to create the project.

## To run the project

```sh
git clone 
cd /monad-transformers
nix-shell
hpack
cabal new-build
cabal new-run monad-transformers-exe

```

### Next step

- unit tests
- replace mtl with polysemy(https://hackage.haskell.org/package/polysemy)
