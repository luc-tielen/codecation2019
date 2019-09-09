
build: configure
		@cabal new-build

output: build
		@./scripts/build.sh

configure:
		@hpack --force && cabal new-configure --enable-tests

repl:
		@cabal new-repl exe:codecation

lint:
		@hlint .

continuous:
		@ghcid --command="cabal new-repl exe:codecation" --test=":!cabal new-run codecation"

hoogle:
		@hoogle server --local -p 8080

.PHONY: hoogle lint configure build output repl continuous
