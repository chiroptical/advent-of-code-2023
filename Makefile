build: hpack
	cabal --ghc-options='${GHC_OPTIONS}' build

hpack:
	hpack .

test: hpack
	cabal --ghc-options='${GHC_OPTIONS}' test

run: hpack
	cabal --ghc-options='${GHC_OPTIONS}' run

format-haskell:
	find app/ src/ test/ -name "*.hs" -exec fourmolu -i {} +

format-nix:
	alejandra --quiet .

format: format-nix format-haskell

ghcid: hpack
	ghcid -c "cabal --ghc-options='${GHC_OPTIONS}' repl"

ghcid-test: hpack
	ghcid -c "cabal --ghc-options='${GHC_OPTIONS}' repl adventOfCode2023-test"

hlint: hpack
	hlint .

clean: hpack
	cabal clean

repl: hpack
	cabal repl

.PHONY: build hpack test run format-haskell format-nix format ghcid ghcid-test hlint clean repl
