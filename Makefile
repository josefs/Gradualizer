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
	./gradualizer -pa src/ -I include -- src/*.erl

.PHONY: nocrashongradualize
nocrashongradualize: escript
	./gradualizer -pa src/ -I include -- src/*.erl; \
    EXIT=$$?; \
    if [ $$EXIT -eq 0 ] || [ $$EXIT -eq 1 ]; then \
        exit 0; \
    else \
        exit $$EXIT; \
    fi

.PHONY: clean
clean:
	rebar3 clean
	rm -rf _build
	rm -f gradualizer

.PHONY: tests eunit cli-tests
tests: eunit cli-tests

eunit:
	rebar3 eunit

cli-tests: escript
	# CLI test cases
	# 1. When checking a dir; printing filename is the default
	./gradualizer test/dir \
	|perl -ne 'm%^test/dir/test_in_dir.erl:% or die "CLI 1 ($$_)"'
	# 2. --no-print-file with directory
	./gradualizer --no-print-file test/dir \
	|perl -ne '/^The/ or die "CLI 2 ($$_)"'
	# 3. --print-module with directory
	./gradualizer --print-module test/dir \
	|perl -ne '/^test_in_dir:/ or die "CLI 3 ($$_)"'
	# 4. --print-basename with directory
	./gradualizer --print-basename test/dir \
	|perl -ne '/^test_in_dir.erl:/ or die "CLI 4 ($$_)"'
	# 5. Checking a single file; not printing filename is the default
	./gradualizer test/dir/test_in_dir.erl \
	|perl -ne '/^The/ or die "CLI 5 ($$_)"'
	# 6. Brief formatting
	./gradualizer --fmt-location brief --print-basename test/dir \
	|perl -ne '/^test_in_dir.erl:6:12: The variable N is ex/ or die "CLI 6 ($$_)"'
	# 7. Verbose formatting, without filename
	./gradualizer --fmt-location verbose --no-print-file test/dir \
	|perl -ne '/^The variable N on line 6 at column 12/ or die "CLI 7 ($$_)"'
	# 8. No location, no filename
	./gradualizer --fmt-location none --no-print-file test/dir/test_in_dir.erl \
	|perl -ne '/^The variable N is expected/ or die "CLI 8 ($$_)"'

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
travischeck: cover dialyze nocrashongradualize
