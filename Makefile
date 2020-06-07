all:
	dune build @install @runtest --profile release
	dune build
clean:
	rm -rf _build
runtest:
	./_build/default/test/test.exe
run:
	./_build/default/src/yallo.exe compile test/test_interface.yallo TestContract
#pin: 
#	opam pin add nmea . -n --working-dir && opam remove nmea && opam install nmea --working-dir
