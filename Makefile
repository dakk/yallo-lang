all: grammar
	dune build @install @runtest --profile release
	dune build

clean:
	rm -rf _build
	
run-test:
	./_build/default/test/test.exe 

grammar:
	@echo Pretty-printing grammar... docs/grammar.ebnf
	@obelisk lib/parser.mly > docs/grammar.ebnf

#list-errors:
# menhir --list-errors src/parser.mly > src/parser.messages

#pin: 
#	opam pin add nmea . -n --working-dir && opam remove nmea && opam install nmea --working-dir
