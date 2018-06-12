#
# compile the project
# > make app
#
# start a shell
# > make shell
#
# run unit tests
# > make eunit
#
# run tests with coverage
# > make tests COVER=1 && open cover/index.html
#
# run dialyzer
# > make dialyze
#
# run all checks (including tests and dialyzer)
# > make check
#
PROJECT = gradualizer

PLT_APPS = kernel stdlib compiler crypto
DIALYZER_OPTS = -Werror_handling

include erlang.mk

# We want warnings to be warnings, not errors.
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

EUNIT_OPTS = verbose

# Unit test files to run
EUNIT_TEST_SOURCES = $(shell ls $(TEST_DIR)/*.erl)

# Override erlang.mk variable to only run unit test modules
# (results in a bit less noisy output)
# (Other it tried to compile and run all source files
#  recursively in all subdirs of test)
EUNIT_TEST_MODS = $(notdir $(basename $(EUNIT_TEST_SOURCES)))
