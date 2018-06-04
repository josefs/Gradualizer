.PHONY: run

run:	absform.beam constraints.beam gradualizer_db.beam typechecker.beam typelib.beam
	erl *.beam

%.beam:	%.erl
	erlc $<
