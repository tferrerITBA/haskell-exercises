# haskell-exercises

## IDE Configuration
It is recommended to use VS Code with the `Haskero` extension.

## Project Setup
Install `stack` and required dependencies:
```bash
wget -qO- https://get.haskellstack.org/ | sh # Linux and macOS
sudo apt install libtinfo-dev # Debian/Ubuntu
stack build intero --copy-compiler-tool
```

To run GHCI and compile the project run `stack ghci` in the project's root folder.

## Useful GHCI commands
`:t <function>` retrieves a type
`:a` recompiles files that changed since they were last loaded
`:q` exits GHCI
