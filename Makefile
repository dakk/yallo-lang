all: grammar
	#dune build @install @runtest --profile release
	dune build

clean:
	rm -rf _build
	
run-test:
	./_build/default/test/test.exe 

grammar:
	@echo Pretty-printing grammar... docs/grammar.ebnf
	@obelisk lib/passes/1_parsing/parser.mly > docs/grammar.ebnf

local-install:
	sudo cp ./_build/default/src/yallo.exe /usr/local/bin

vscode-install:
	rm -rf ~/.vscode-oss/extensions/dakk.yallo-lang*
	cp -r ./vsext/ ~/.vscode-oss/extensions/dakk.yallo-lang-0.1

# list-errors:
#  menhir --list-errors lib/parser.mly > lib/parser.messages

# update-errors:
# 	menhir --update-errors lib/parser.messages lib/parser.mly > lib/parser.messages

#pin: 
#	opam pin add yallo . -n --working-dir && opam remove yallo && opam install yallo --working-dir
