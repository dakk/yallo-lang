all: grammar
	dune build @install @runtest --profile release
	dune build

clean:
	rm -rf _build
	
run-test:
	./_build/default/src/yallo.exe compile test/test_literal.yallo
	./_build/default/src/yallo.exe compile test/test_types.yallo
	./_build/default/src/yallo.exe compile test/test_expr.yallo
	./_build/default/src/yallo.exe compile test/test_statements.yallo
	./_build/default/src/yallo.exe compile test/test_declarations.yallo
	./_build/default/src/yallo.exe compile test/test_record.yallo 
	./_build/default/src/yallo.exe compile test/test_typemod.yallo -print-pt

grammar:
	@echo Pretty-printing grammar... docs/grammar.ebnf
	@obelisk src/parser.mly > docs/grammar.ebnf

#pin: 
#	opam pin add nmea . -n --working-dir && opam remove nmea && opam install nmea --working-dir
