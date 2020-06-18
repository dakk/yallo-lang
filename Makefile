all: grammar
	#dune build @install @runtest --profile release
	dune build

clean:
	rm -rf _build
	
run-test:
	./_build/default/test/test.exe 

grammar:
	@echo Pretty-printing grammar... docs/grammar.ebnf
	@obelisk lib/parser.mly > docs/grammar.ebnf

local-install:
	sudo cp ./_build/default/src/yallo.exe /usr/local/bin

# list-errors:
#  menhir --list-errors lib/parser.mly > lib/parser.messages

# update-errors:
# 	menhir --update-errors lib/parser.messages lib/parser.mly > lib/parser.messages

#pin: 
#	opam pin add yallo . -n --working-dir && opam remove yallo && opam install yallo --working-dir
