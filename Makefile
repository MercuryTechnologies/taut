library = lib:taut
executable = exe:taut
config = config.yml
match =

configure:
	cabal configure

build:
	cabal build

clean:
	cabal clean

ghci:
	cabal repl $(library)

ghcid:
ifeq ($(match),)
	ghcid -c "cabal repl $(library)"
else
	ghcid -c "cabal repl $(library)" --allow-eval --warnings --test $(match)
endif

.PHONY : configure build clean run ghci ghcid
