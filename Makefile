all:
	dune build @install @runtest --profile release
	dune build
clean:
	rm -rf _build
runtest:
	./_build/default/test/test.exe
run-test:
	./_build/default/src/yallo.exe compile test/test_literal.yallo tzToken
	./_build/default/src/yallo.exe compile test/test_types.yallo tzToken
	./_build/default/src/yallo.exe compile test/test3.yallo tzToken
	./_build/default/src/yallo.exe compile test/test_expr.yallo tzToken
#pin: 
#	opam pin add nmea . -n --working-dir && opam remove nmea && opam install nmea --working-dir
