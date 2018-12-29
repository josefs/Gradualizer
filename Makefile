#
# compile the project
# > make app
#
# start a shell
# > make shell
#
# build cli
# > make escript
#
# run cli
# > ./gradualizer
#
# run unit tests
# > make tests
#
# generate coverage
# > make cover
#
# show coverage (Linux)
# > make showcover
# show coverage (OS X)
# > make osxshowcover
#
# run dialyzer
# > make dialyze
#
# run gradualizer on itself
# > make gradualize
#
# run all checks (tests, dialyze and gradualize)
# > make check
#

.PHONY: app
app:
	rebar3 compile

.PHONY: shell
shell:
	rebar3 shell

.PHONY: escript
escript:
	rebar3 escriptize
	cp _build/default/bin/gradualizer .

.PHONY: gradualize
gradualize: escript
	./gradualizer -pa src/ src/*.erl

.PHONY: clean
clean:
	rebar3 clean
	rm -rf _build
	rm -f gradualizer

.PHONY: tests
tests:
	rebar3 eunit

.PHONY: cover
cover:
	rebar3 do eunit -c, cover

.PHONY: showcover
showcover: cover
	xdg-open _build/test/cover/index.html

.PHONY: osxshowcover
osxshowcover: cover
	open _build/test/cover/index.html

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: check
check: tests dialyze gradualize

.PHONY: travischeck
travischeck: cover dialyze
