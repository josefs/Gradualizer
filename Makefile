# This Makefile has the following dependencies:
# GNU Make, OTP, Curl, Perl and basic Posix applications.

# Rebar3 can also be used to build the project, but note that
# gradualizer_prelude.beam must be rebuilt if any file under
# priv/prelude/ is changed, added or deleted.

# Overview of make targets:
#
#   all         Compile app and escript (the default target)
#   app         Compile the OTP application (beams and app file)
#   escript     Generate the gradualizer CLI as a self-contained escript
#               located at bin/gradualizer
#   shell       Start an erlang shell with gradualizer in the code path
#   eunit       Run eunit tests
#   tests       Run all tests (eunit and CLI tests)
#   cover       Run tests and generate coverage reports (XML and plain)
#   dialyze     Run dialyzer
#   gradualize  Run gradualizer on itself
#   check       Run all checks (tests, dialyze, gradualize)
#   clean       Delete all generated files
#
# Capitalized variables can be overriden on the command line.
# Example:
#
#   make tests EUNIT_OPTS=      Run tests without the eunit verbose option
#
# The directory structure is following what is described on
# http://erlang.org/doc/design_principles/applications.html

.PHONY: all
all: app escript

# Compilation

erls = $(wildcard src/*.erl)
beams = $(erls:src/%.erl=ebin/%.beam)

ERLC_OPTS = -I include -pa ebin +debug_info

app: $(beams) ebin/gradualizer.app

ebin:
	mkdir ebin

ebin/%.beam: src/%.erl ebin
	erlc $(ERLC_OPTS) -o ebin $<

# Compile-time dependencies between modules and other files
ebin/gradualizer_prelude.beam: priv/prelude \
                               ebin/gradualizer_prelude_parse_trans.beam

# .app file
# TODO When we start using git tags, insert version from
# `git describe --tags`
ebin/gradualizer.app: src/gradualizer.app.src ebin
	cp $< $@

.PHONY: shell
shell:
	erl -pa ebin

.PHONY: escript
escript: bin/gradualizer

# legacy CLI location
gradualizer: bin/gradualizer
	cp $< $@

define erl_build_escript
FileList = [{filename:join("gradualizer", Name), Bin} \
            || Name <- filelib:wildcard(filename:join("ebin", "*.{beam,app}")), \
               {ok, Bin} <- [file:read_file(Name)]], \
{ok, {_Name, ZipBin}} = zip:zip("dummy-name", FileList, [memory]), \
EscriptBin = <<"#!/usr/bin/env escript\n" \
               "%%\n" \
               "%%! -escript main gradualizer_cli\n", \
               ZipBin/binary>>, \
ok = file:write_file("bin/gradualizer", EscriptBin), halt().
endef

bin/gradualizer: $(beams) ebin/gradualizer.app
	mkdir -p bin
	erl -noinput -eval '$(erl_build_escript)'
	chmod +x $@

.PHONY: gradualize
gradualize: escript
	bin/gradualizer -pa ebin -- ebin

.PHONY: nocrashongradualize
nocrashongradualize: escript
	bin/gradualizer -pa ebin -- ebin; \
    EXIT=$$?; \
    if [ $$EXIT -eq 0 ] || [ $$EXIT -eq 1 ]; then \
        exit 0; \
    else \
        exit $$EXIT; \
    fi

.PHONY: clean
clean:
	rm -rf _build        # legacy rebar3 build directory
	rm -f gradualizer    # legacy CLI location
	rm -rf bin/gradualizer ebin cover test/*.beam

.PHONY: tests eunit compile-tests cli-tests
tests: eunit cli-tests

test_erls=$(wildcard test/*.erl)
test_beams=$(test_erls:test/%.erl=test/%.beam)

compile-tests: app $(test_beams) test/any.beam test/records.beam

test/%.beam: test/%.erl
	erlc $(ERLC_OPTS) -o test $<

# Extra beams used by some test cases
test/any.beam: test/should_pass/any.erl
	erlc $(ERLC_OPTS) -o test $<

test/records.beam: test/should_pass/records.erl
	erlc $(ERLC_OPTS) -o test $<

EUNIT_OPTS = verbose

define erl_run_eunit
case eunit:test("test", [$(EUNIT_OPTS)]) of \
    ok -> ok; \
    error -> halt(2) \
end
endef

eunit: compile-tests
	erl -noinput -pa ebin -pa test -eval \
	 '$(erl_run_eunit), halt().'

cli-tests: bin/gradualizer
	# CLI test cases
	# 1. When checking a dir; printing filename is the default
	bin/gradualizer test/dir \
	|perl -ne 'm%^test/dir/test_in_dir.erl:% or die "CLI 1 ($$_)"'
	# 2. --no-print-file with directory
	bin/gradualizer --no-print-file test/dir \
	|perl -ne '/^The/ or die "CLI 2 ($$_)"'
	# 3. --print-module with directory
	bin/gradualizer --print-module test/dir \
	|perl -ne '/^test_in_dir:/ or die "CLI 3 ($$_)"'
	# 4. --print-basename with directory
	bin/gradualizer --print-basename test/dir \
	|perl -ne '/^test_in_dir.erl:/ or die "CLI 4 ($$_)"'
	# 5. Checking a single file; not printing filename is the default
	bin/gradualizer test/dir/test_in_dir.erl \
	|perl -ne '/^The/ or die "CLI 5 ($$_)"'
	# 6. Brief formatting
	bin/gradualizer --fmt-location brief --print-basename test/dir \
	|perl -ne '/^test_in_dir.erl:6:12: The variable N is ex/ or die "CLI 6 ($$_)"'
	# 7. Verbose formatting, without filename
	bin/gradualizer --fmt-location verbose --no-print-file test/dir \
	|perl -ne '/^The variable N on line 6 at column 12/ or die "CLI 7 ($$_)"'
	# 8. No location, no filename
	bin/gradualizer --fmt-location none --no-print-file test/dir/test_in_dir.erl \
	|perl -ne '/^The variable N is expected/ or die "CLI 8 ($$_)"'

.PHONY: cover
cover: EUNIT_OPTS =
cover: compile-tests test/covertool.beam
	mkdir -p cover
	erl -noinput -pa ebin -pa test -eval \
	 '%% Cover compile, run eunit, export and generate reports \
	  case cover:compile_beam_directory("ebin") of % \
	      {error, _} -> halt(2); % \
	      _List      -> ok % \
	  end, % \
	  $(erl_run_eunit), % \
	  cover:export("cover/eunit.coverdata"), % \
	  cover:analyse_to_file([{outdir, "cover"}]), % \
	  cover:reset(), % because covertool imports it again \
	  covertool:main(["-cover", "cover/eunit.coverdata", % \
	                  "-output", "cover/coverage.xml", % \
	                  "-appname", "gradualizer"]), % \
	  halt().'

test/covertool.beam: test/covertool.erl test/covertool.hrl
	erlc $(ERLC_OPTS) -I test -o test $<

# Download the deps for generating XML cover report
test/covertool.erl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.1/src/covertool.erl \
	     -o $@

test/covertool.hrl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.1/include/covertool.hrl \
	     -o $@

DIALYZER_PLT = .dialyzer_plt
export DIALYZER_PLT
PLT_APPS = erts kernel stdlib syntax_tools
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions
               # -Wunmatched_returns -Wunknown
               # -Wunderspecs -Woverspec -Wspecdiffs

.PHONY: dialyze
dialyze: app $(DIALYZER_PLT)
	dialyzer $(DIALYZER_OPTS) ebin

# DIALYZER_PLT is a variable understood directly by Dialyzer.
# Exit status 2 = warnings were emitted
$(DIALYZER_PLT):
	dialyzer --build_plt --apps $(PLT_APPS) || test $$? -eq 2

.PHONY: check
check: tests dialyze gradualize

.PHONY: travischeck
travischeck: cover dialyze nocrashongradualize
