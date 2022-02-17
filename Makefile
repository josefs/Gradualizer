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
# Capitalized variables can be overridden on the command line.
# Example:
#
#   make tests EUNIT_OPTS=verbose
#               Run tests with the eunit verbose option
#
# The directory structure is following what is described on
# http://erlang.org/doc/design_principles/applications.html

.PHONY: all
all: app escript

# Compilation

erls = $(wildcard src/*.erl)
beams = $(erls:src/%.erl=ebin/%.beam)

ERLC_OPTS = -I include -I src -pa ebin +debug_info
TEST_ERLC_OPTS = +debug_info

app: $(beams) ebin/gradualizer.app

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl | ebin
	erlc $(ERLC_OPTS) -o ebin $<

# Compile-time dependencies between modules and other files
ebin/gradualizer_prelude.beam: priv/prelude \
                               ebin/gradualizer_prelude_parse_trans.beam
ebin/typechecker.beam: src/typelib.hrl
ebin/gradualizer_fmt.beam: src/typelib.hrl

# .app file
ebin/gradualizer.app: src/gradualizer.app.src | ebin
	sed -e "s/{vsn, *\"git\"}/{vsn, \"`git describe --tags --always`\"}/" $< > $@

.PHONY: shell
shell: app
	erl -pz ebin

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
	bin/gradualizer -pa ebin --color ebin

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
tests: build_test_data eunit cli-tests

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

test/arg.beam: test/should_fail/arg.erl
	erlc $(ERLC_OPTS) -o test $<

.PHONY: build_test_data
test_data_erls = $(wildcard test/known_problems/**/*.erl test/should_fail/*.erl test/should_pass/*.erl)
build_test_data:
	mkdir -p "test_data"
	erlc $(TEST_ERLC_OPTS) -o test_data $(test_data_erls)

EUNIT_OPTS =

define erl_run_eunit
case eunit:test("test", [$(EUNIT_OPTS)]) of \
    ok -> ok; \
    error -> halt(2) \
end
endef

eunit: compile-tests
	erl -noinput -pa ebin -pa test -eval \
	 '$(erl_run_eunit), halt().'

cli-tests: bin/gradualizer test/arg.beam
	# CLI test cases
	# 1. When checking a dir with erl files, erl file names are printed
	bin/gradualizer test/dir \
	2>&1|perl -0777 -ne 'm%^test/dir/test_in_dir.erl:% or die "CLI 1 ($$_)"'
	# 2. When checking a beam file; beam file name is printed
	bin/gradualizer test/arg.beam \
	2>&1|perl -0777 -ne 'm%^test/arg.beam:% or die "CLI 1 ($$_)"'
	# 3. Brief formatting
	bin/gradualizer --fmt_location brief test/dir \
	2>&1|perl -0777 -ne '/^test\/dir\/test_in_dir.erl:6:12: The variable/ or die "CLI 6 ($$_)"'
	# 4. Verbose formatting
	bin/gradualizer --fmt_location verbose --no_fancy test/dir \
	2>&1|perl -ne '/^test\/dir\/test_in_dir.erl: The variable N on line 6 at column 12/ or die "CLI 7 ($$_)"'
	# 5. Possible to exclude prelude (-0777 from https://stackoverflow.com/a/30594643/497116)
	bin/gradualizer --no_prelude test/should_pass/cyclic_otp_specs.erl \
	2>&1|perl -0777 -ne '/^test\/should_pass\/cyclic_otp_specs.erl: The type spec/g or die "CLI 9 ($$_)"'
	# 6. Excluding prelude and then including it is a no-op
	bin/gradualizer --no_prelude --specs_override_dir priv/prelude \
	  test/should_pass/cyclic_otp_specs.erl || (echo "CLI 10"; exit 1)

.PHONY: cover
cover: compile-tests
	mkdir -p cover
	erl -noinput -pa ebin -pa test -eval \
	 '%% Cover compile, run eunit, export and generate reports \
	  case cover:compile_beam_directory("ebin") of % \
	      {error, _} -> halt(2); % \
	      _List      -> ok % \
	  end, % \
	  $(erl_run_eunit), % \
	  cover:export("cover/eunit.coverdata"), % \
	  cover:analyse_to_file([{outdir, "cover"}]), % plain text \
	  cover:analyse_to_file([{outdir, "cover"}, html]), % \
	  halt().'

cover/coverage.xml: cover test/covertool.beam
	erl -noinput -pa test -eval \
	 'covertool:main(["-cover", "cover/eunit.coverdata", % \
	                  "-output", "cover/coverage.xml", % \
	                  "-appname", "gradualizer"]), % \
	  halt().'

test/covertool.beam: test/covertool.erl test/covertool.hrl
	erlc $(ERLC_OPTS) -I test -o test $<

# Download the deps for generating XML cover report
test/covertool.erl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.2/src/covertool.erl \
	     -o $@

test/covertool.hrl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.2/include/covertool.hrl \
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
travischeck: cover/coverage.xml dialyze nocrashongradualize
