# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: all app apps deps search rel relup docs install-docs check tests clean distclean help erlang-mk

ERLANG_MK_FILENAME := $(realpath $(lastword $(MAKEFILE_LIST)))
export ERLANG_MK_FILENAME

ERLANG_MK_VERSION = 208a116
ERLANG_MK_WITHOUT = index

# Make 3.81 and 3.82 are deprecated.

ifeq ($(MAKE_VERSION),3.81)
$(warning Please upgrade to GNU Make 4 or later: https://erlang.mk/guide/installation.html)
endif

ifeq ($(MAKE_VERSION),3.82)
$(warning Please upgrade to GNU Make 4 or later: https://erlang.mk/guide/installation.html)
endif

# Core configuration.

PROJECT ?= $(notdir $(CURDIR))
PROJECT := $(strip $(PROJECT))

PROJECT_VERSION ?= rolling
PROJECT_MOD ?= $(PROJECT)_app
PROJECT_ENV ?= []

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose_2 = set -x;
gen_verbose = $(gen_verbose_$(V))

# Temporary files directory.

ERLANG_MK_TMP ?= $(CURDIR)/.erlang.mk
export ERLANG_MK_TMP

# "erl" command.

ERL = erl +A0 -noinput -boot start_clean

# Platform detection.

ifeq ($(PLATFORM),)
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
PLATFORM = linux
else ifeq ($(UNAME_S),Darwin)
PLATFORM = darwin
else ifeq ($(UNAME_S),SunOS)
PLATFORM = solaris
else ifeq ($(UNAME_S),GNU)
PLATFORM = gnu
else ifeq ($(UNAME_S),FreeBSD)
PLATFORM = freebsd
else ifeq ($(UNAME_S),NetBSD)
PLATFORM = netbsd
else ifeq ($(UNAME_S),OpenBSD)
PLATFORM = openbsd
else ifeq ($(UNAME_S),DragonFly)
PLATFORM = dragonfly
else ifeq ($(shell uname -o),Msys)
PLATFORM = msys2
else
$(error Unable to detect platform. Please open a ticket with the output of uname -a.)
endif

export PLATFORM
endif

# Core targets.

all:: deps app rel

# Noop to avoid a Make warning when there's nothing to do.
rel::
	$(verbose) :

relup:: deps app

check:: tests

clean:: clean-crashdump

clean-crashdump:
ifneq ($(wildcard erl_crash.dump),)
	$(gen_verbose) rm -f erl_crash.dump
endif

distclean:: clean distclean-tmp

distclean-tmp:
	$(gen_verbose) rm -rf $(ERLANG_MK_TMP)

help::
	$(verbose) printf "%s\n" \
		"erlang.mk (version $(ERLANG_MK_VERSION)) is distributed under the terms of the ISC License." \
		"Copyright (c) 2013-2016 Loïc Hoguin <essen@ninenines.eu>" \
		"" \
		"Usage: [V=1] $(MAKE) [target]..." \
		"" \
		"Core targets:" \
		"  all           Run deps, app and rel targets in that order" \
		"  app           Compile the project" \
		"  deps          Fetch dependencies (if needed) and compile them" \
		"  fetch-deps    Fetch dependencies recursively (if needed) without compiling them" \
		"  list-deps     List dependencies recursively on stdout" \
		"  search q=...  Search for a package in the built-in index" \
		"  rel           Build a release for this project, if applicable" \
		"  docs          Build the documentation for this project" \
		"  install-docs  Install the man pages for this project" \
		"  check         Compile and run all tests and analysis for this project" \
		"  tests         Run the tests for this project" \
		"  clean         Delete temporary and output files from most targets" \
		"  distclean     Delete all temporary and output files" \
		"  help          Display this help and exit" \
		"  erlang-mk     Update erlang.mk to the latest version"

# Core functions.

empty :=
space := $(empty) $(empty)
tab := $(empty)	$(empty)
comma := ,

define newline


endef

define comma_list
$(subst $(space),$(comma),$(strip $(1)))
endef

define escape_dquotes
$(subst ",\",$1)
endef

# Adding erlang.mk to make Erlang scripts who call init:get_plain_arguments() happy.
define erlang
$(ERL) $2 -pz $(ERLANG_MK_TMP)/rebar/ebin -eval "$(subst $(newline),,$(call escape_dquotes,$1))" -- erlang.mk
endef

ifeq ($(PLATFORM),msys2)
core_native_path = $(subst \,\\\\,$(shell cygpath -w $1))
else
core_native_path = $1
endif

core_http_get = curl -Lf$(if $(filter-out 0,$(V)),,s)o $(call core_native_path,$1) $2

core_eq = $(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))

core_find = $(if $(wildcard $1),$(shell find $(1:%/=%) -type f -name $(subst *,\*,$2)))

core_lc = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$(1)))))))))))))))))))))))))))

core_ls = $(filter-out $(1),$(shell echo $(1)))

# @todo Use a solution that does not require using perl.
core_relpath = $(shell perl -e 'use File::Spec; print File::Spec->abs2rel(@ARGV) . "\n"' $1 $2)

# Automated update.

ERLANG_MK_REPO ?= https://github.com/ninenines/erlang.mk
ERLANG_MK_COMMIT ?=
ERLANG_MK_BUILD_CONFIG ?= build.config
ERLANG_MK_BUILD_DIR ?= .erlang.mk.build

erlang-mk: WITHOUT ?= $(ERLANG_MK_WITHOUT)
erlang-mk:
ifdef ERLANG_MK_COMMIT
	git clone $(ERLANG_MK_REPO) $(ERLANG_MK_BUILD_DIR)
	cd $(ERLANG_MK_BUILD_DIR) && git checkout $(ERLANG_MK_COMMIT)
else
	git clone --depth 1 $(ERLANG_MK_REPO) $(ERLANG_MK_BUILD_DIR)
endif
	if [ -f $(ERLANG_MK_BUILD_CONFIG) ]; then cp $(ERLANG_MK_BUILD_CONFIG) $(ERLANG_MK_BUILD_DIR)/build.config; fi
	$(MAKE) -C $(ERLANG_MK_BUILD_DIR) WITHOUT='$(strip $(WITHOUT))'
	cp $(ERLANG_MK_BUILD_DIR)/erlang.mk ./erlang.mk
	rm -rf $(ERLANG_MK_BUILD_DIR)

# The erlang.mk package index is bundled in the default erlang.mk build.
# Search for the string "copyright" to skip to the rest of the code.

# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-kerl

KERL_INSTALL_DIR ?= $(HOME)/erlang

ifeq ($(strip $(KERL)),)
KERL := $(ERLANG_MK_TMP)/kerl/kerl
endif

export KERL

KERL_GIT ?= https://github.com/kerl/kerl
KERL_COMMIT ?= master

KERL_MAKEFLAGS ?=

OTP_GIT ?= https://github.com/erlang/otp

define kerl_otp_target
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(1)),)
$(KERL_INSTALL_DIR)/$(1): $(KERL)
	MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $(1) $(1)
	$(KERL) install $(1) $(KERL_INSTALL_DIR)/$(1)
endif
endef

define kerl_hipe_target
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$1-native),)
$(KERL_INSTALL_DIR)/$1-native: $(KERL)
	KERL_CONFIGURE_OPTIONS=--enable-native-libs \
		MAKEFLAGS="$(KERL_MAKEFLAGS)" $(KERL) build git $(OTP_GIT) $1 $1-native
	$(KERL) install $1-native $(KERL_INSTALL_DIR)/$1-native
endif
endef

$(KERL):
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) git clone --depth 1 $(KERL_GIT) $(ERLANG_MK_TMP)/kerl
	$(verbose) cd $(ERLANG_MK_TMP)/kerl && git checkout $(KERL_COMMIT)
	$(verbose) chmod +x $(KERL)

distclean:: distclean-kerl

distclean-kerl:
	$(gen_verbose) rm -rf $(KERL)

# Allow users to select which version of Erlang/OTP to use for a project.

ifneq ($(strip $(LATEST_ERLANG_OTP)),)
ERLANG_OTP := $(notdir $(lastword $(sort $(filter-out %-rc1 %-rc2 %-rc3,\
	$(wildcard $(KERL_INSTALL_DIR)/*[^-native])))))
endif

ERLANG_OTP ?=
ERLANG_HIPE ?=

# Use kerl to enforce a specific Erlang/OTP version for a project.
ifneq ($(strip $(ERLANG_OTP)),)
export PATH := $(KERL_INSTALL_DIR)/$(ERLANG_OTP)/bin:$(PATH)
SHELL := env PATH=$(PATH) $(SHELL)
$(eval $(call kerl_otp_target,$(ERLANG_OTP)))

# Build Erlang/OTP only if it doesn't already exist.
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(ERLANG_OTP))$(BUILD_ERLANG_OTP),)
$(info Building Erlang/OTP $(ERLANG_OTP)... Please wait...)
$(shell $(MAKE) $(KERL_INSTALL_DIR)/$(ERLANG_OTP) ERLANG_OTP=$(ERLANG_OTP) BUILD_ERLANG_OTP=1 >&2)
endif

else
# Same for a HiPE enabled VM.
ifneq ($(strip $(ERLANG_HIPE)),)
export PATH := $(KERL_INSTALL_DIR)/$(ERLANG_HIPE)-native/bin:$(PATH)
SHELL := env PATH=$(PATH) $(SHELL)
$(eval $(call kerl_hipe_target,$(ERLANG_HIPE)))

# Build Erlang/OTP only if it doesn't already exist.
ifeq ($(wildcard $(KERL_INSTALL_DIR)/$(ERLANG_HIPE))$(BUILD_ERLANG_OTP),)
$(info Building HiPE-enabled Erlang/OTP $(ERLANG_OTP)... Please wait...)
$(shell $(MAKE) $(KERL_INSTALL_DIR)/$(ERLANG_HIPE) ERLANG_HIPE=$(ERLANG_HIPE) BUILD_ERLANG_OTP=1 >&2)
endif

endif
endif

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: search

define pkg_print
	$(verbose) printf "%s\n" \
		$(if $(call core_eq,$(1),$(pkg_$(1)_name)),,"Pkg name:    $(1)") \
		"App name:    $(pkg_$(1)_name)" \
		"Description: $(pkg_$(1)_description)" \
		"Home page:   $(pkg_$(1)_homepage)" \
		"Fetch with:  $(pkg_$(1)_fetch)" \
		"Repository:  $(pkg_$(1)_repo)" \
		"Commit:      $(pkg_$(1)_commit)" \
		""

endef

search:
ifdef q
	$(foreach p,$(PACKAGES), \
		$(if $(findstring $(call core_lc,$(q)),$(call core_lc,$(pkg_$(p)_name) $(pkg_$(p)_description))), \
			$(call pkg_print,$(p))))
else
	$(foreach p,$(PACKAGES),$(call pkg_print,$(p)))
endif

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps clean-tmp-deps.log

# Configuration.

ifdef OTP_DEPS
$(warning The variable OTP_DEPS is deprecated in favor of LOCAL_DEPS.)
endif

IGNORE_DEPS ?=
export IGNORE_DEPS

APPS_DIR ?= $(CURDIR)/apps
export APPS_DIR

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

# External "early" plugins (see core/plugins.mk for regular plugins).
# They both use the core_dep_plugin macro.

define core_dep_plugin
ifeq ($(2),$(PROJECT))
-include $$(patsubst $(PROJECT)/%,%,$(1))
else
-include $(DEPS_DIR)/$(1)

$(DEPS_DIR)/$(1): $(DEPS_DIR)/$(2) ;
endif
endef

DEP_EARLY_PLUGINS ?=

$(foreach p,$(DEP_EARLY_PLUGINS),\
	$(eval $(if $(findstring /,$p),\
		$(call core_dep_plugin,$p,$(firstword $(subst /, ,$p))),\
		$(call core_dep_plugin,$p/early-plugins.mk,$p))))

dep_name = $(if $(dep_$(1)),$(1),$(if $(pkg_$(1)_name),$(pkg_$(1)_name),$(1)))
dep_repo = $(patsubst git://github.com/%,https://github.com/%, \
	$(if $(dep_$(1)),$(word 2,$(dep_$(1))),$(pkg_$(1)_repo)))
dep_commit = $(if $(dep_$(1)_commit),$(dep_$(1)_commit),$(if $(dep_$(1)),$(word 3,$(dep_$(1))),$(pkg_$(1)_commit)))

LOCAL_DEPS_DIRS = $(foreach a,$(LOCAL_DEPS),$(if $(wildcard $(APPS_DIR)/$(a)),$(APPS_DIR)/$(a)))
ALL_APPS_DIRS = $(if $(wildcard $(APPS_DIR)/),$(filter-out $(APPS_DIR),$(shell find $(APPS_DIR) -maxdepth 1 -type d)))
ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(foreach dep,$(filter-out $(IGNORE_DEPS),$(BUILD_DEPS) $(DEPS)),$(call dep_name,$(dep))))

ifeq ($(filter $(APPS_DIR) $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(APPS_DIR):$(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(APPS_DIR):$(DEPS_DIR)
endif
endif
export ERL_LIBS

export NO_AUTOPATCH

# Verbosity.

dep_verbose_0 = @echo " DEP   $1 ($(call dep_commit,$1))";
dep_verbose_2 = set -x;
dep_verbose = $(dep_verbose_$(V))

# Core targets.

apps:: $(ALL_APPS_DIRS) clean-tmp-deps.log
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/apps.log
endif
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
# Create ebin directory for all apps to make sure Erlang recognizes them
# as proper OTP applications when using -include_lib. This is a temporary
# fix, a proper fix would be to compile apps/* in the right order.
ifndef IS_APP
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		mkdir -p $$dep/ebin; \
	done
endif
# at the toplevel: if LOCAL_DEPS is defined with at least one local app, only
# compile that list of apps. otherwise, compile everything.
# within an app: compile all LOCAL_DEPS that are (uncompiled) local apps
	$(verbose) set -e; for dep in $(if $(LOCAL_DEPS_DIRS)$(IS_APP),$(LOCAL_DEPS_DIRS),$(ALL_APPS_DIRS)) ; do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/apps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/apps.log; \
			$(MAKE) -C $$dep IS_APP=1; \
		fi \
	done

clean-tmp-deps.log:
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) rm -f $(ERLANG_MK_TMP)/deps.log
endif

ifneq ($(SKIP_DEPS),)
deps::
else
deps:: $(ALL_DEPS_DIRS) apps clean-tmp-deps.log
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) set -e; for dep in $(ALL_DEPS_DIRS) ; do \
		if grep -qs ^$$dep$$ $(ERLANG_MK_TMP)/deps.log; then \
			:; \
		else \
			echo $$dep >> $(ERLANG_MK_TMP)/deps.log; \
			if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
				$(MAKE) -C $$dep IS_DEP=1; \
			else \
				echo "Error: No Makefile to build dependency $$dep." >&2; \
				exit 2; \
			fi \
		fi \
	done
endif

# Deps related targets.

# @todo rename GNUmakefile and makefile into Makefile first, if they exist
# While Makefile file could be GNUmakefile or makefile,
# in practice only Makefile is needed so far.
define dep_autopatch
	if [ -f $(DEPS_DIR)/$(1)/erlang.mk ]; then \
		rm -rf $(DEPS_DIR)/$1/ebin/; \
		$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
		$(call dep_autopatch_erlang_mk,$(1)); \
	elif [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		if [ -f $(DEPS_DIR)/$1/rebar.lock ]; then \
			$(call dep_autopatch2,$1); \
		elif [ 0 != `grep -c "include ../\w*\.mk" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ 0 != `grep -ci "^[^#].*rebar" $(DEPS_DIR)/$(1)/Makefile` ]; then \
			$(call dep_autopatch2,$(1)); \
		elif [ -n "`find $(DEPS_DIR)/$(1)/ -type f -name \*.mk -not -name erlang.mk -exec grep -i "^[^#].*rebar" '{}' \;`" ]; then \
			$(call dep_autopatch2,$(1)); \
		fi \
	else \
		if [ ! -d $(DEPS_DIR)/$(1)/src/ ]; then \
			$(call dep_autopatch_noop,$(1)); \
		else \
			$(call dep_autopatch2,$(1)); \
		fi \
	fi
endef

define dep_autopatch2
	! test -f $(DEPS_DIR)/$1/ebin/$1.app || \
	mv -n $(DEPS_DIR)/$1/ebin/$1.app $(DEPS_DIR)/$1/src/$1.app.src; \
	rm -f $(DEPS_DIR)/$1/ebin/$1.app; \
	if [ -f $(DEPS_DIR)/$1/src/$1.app.src.script ]; then \
		$(call erlang,$(call dep_autopatch_appsrc_script.erl,$(1))); \
	fi; \
	$(call erlang,$(call dep_autopatch_appsrc.erl,$(1))); \
	if [ -f $(DEPS_DIR)/$(1)/rebar -o -f $(DEPS_DIR)/$(1)/rebar.config -o -f $(DEPS_DIR)/$(1)/rebar.config.script -o -f $(DEPS_DIR)/$1/rebar.lock ]; then \
		$(call dep_autopatch_fetch_rebar); \
		$(call dep_autopatch_rebar,$(1)); \
	else \
		$(call dep_autopatch_gen,$(1)); \
	fi
endef

define dep_autopatch_noop
	printf "noop:\n" > $(DEPS_DIR)/$(1)/Makefile
endef

# Replace "include erlang.mk" with a line that will load the parent Erlang.mk
# if given. Do it for all 3 possible Makefile file names.
ifeq ($(NO_AUTOPATCH_ERLANG_MK),)
define dep_autopatch_erlang_mk
	for f in Makefile makefile GNUmakefile; do \
		if [ -f $(DEPS_DIR)/$1/$$f ]; then \
			sed -i.bak s/'include *erlang.mk'/'include $$(if $$(ERLANG_MK_FILENAME),$$(ERLANG_MK_FILENAME),erlang.mk)'/ $(DEPS_DIR)/$1/$$f; \
		fi \
	done
endef
else
define dep_autopatch_erlang_mk
	:
endef
endif

define dep_autopatch_gen
	printf "%s\n" \
		"ERLC_OPTS = +debug_info" \
		"include ../../erlang.mk" > $(DEPS_DIR)/$(1)/Makefile
endef

define dep_autopatch_fetch_rebar
	mkdir -p $(ERLANG_MK_TMP); \
	if [ ! -d $(ERLANG_MK_TMP)/rebar ]; then \
		git clone -q -n -- https://github.com/rebar/rebar $(ERLANG_MK_TMP)/rebar; \
		cd $(ERLANG_MK_TMP)/rebar; \
		git checkout -q 576e12171ab8d69b048b827b92aa65d067deea01; \
		$(MAKE); \
		cd -; \
	fi
endef

define dep_autopatch_rebar
	if [ -f $(DEPS_DIR)/$(1)/Makefile ]; then \
		mv $(DEPS_DIR)/$(1)/Makefile $(DEPS_DIR)/$(1)/Makefile.orig.mk; \
	fi; \
	$(call erlang,$(call dep_autopatch_rebar.erl,$(1))); \
	rm -f $(DEPS_DIR)/$(1)/ebin/$(1).app
endef

define dep_autopatch_rebar.erl
	application:load(rebar),
	application:set_env(rebar, log_level, debug),
	rmemo:start(),
	Conf1 = case file:consult("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config)") of
		{ok, Conf0} -> Conf0;
		_ -> []
	end,
	{Conf, OsEnv} = fun() ->
		case filelib:is_file("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)") of
			false -> {Conf1, []};
			true ->
				Bindings0 = erl_eval:new_bindings(),
				Bindings1 = erl_eval:add_binding('CONFIG', Conf1, Bindings0),
				Bindings = erl_eval:add_binding('SCRIPT', "$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)", Bindings1),
				Before = os:getenv(),
				{ok, Conf2} = file:script("$(call core_native_path,$(DEPS_DIR)/$1/rebar.config.script)", Bindings),
				{Conf2, lists:foldl(fun(E, Acc) -> lists:delete(E, Acc) end, os:getenv(), Before)}
		end
	end(),
	Write = fun (Text) ->
		file:write_file("$(call core_native_path,$(DEPS_DIR)/$1/Makefile)", Text, [append])
	end,
	Escape = fun (Text) ->
		re:replace(Text, "\\\\$$", "\$$$$", [global, {return, list}])
	end,
	Write("IGNORE_DEPS += edown eper eunit_formatters meck node_package "
		"rebar_lock_deps_plugin rebar_vsn_plugin reltool_util\n"),
	Write("C_SRC_DIR = /path/do/not/exist\n"),
	Write("C_SRC_TYPE = rebar\n"),
	Write("DRV_CFLAGS = -fPIC\nexport DRV_CFLAGS\n"),
	Write(["ERLANG_ARCH = ", rebar_utils:wordsize(), "\nexport ERLANG_ARCH\n"]),
	ToList = fun
		(V) when is_atom(V) -> atom_to_list(V);
		(V) when is_list(V) -> "'\\"" ++ V ++ "\\"'"
	end,
	fun() ->
		Write("ERLC_OPTS = +debug_info\nexport ERLC_OPTS\n"),
		case lists:keyfind(erl_opts, 1, Conf) of
			false -> ok;
			{_, ErlOpts} ->
				lists:foreach(fun
					({d, D}) ->
						Write("ERLC_OPTS += -D" ++ ToList(D) ++ "=1\n");
					({d, DKey, DVal}) ->
						Write("ERLC_OPTS += -D" ++ ToList(DKey) ++ "=" ++ ToList(DVal) ++ "\n");
					({i, I}) ->
						Write(["ERLC_OPTS += -I ", I, "\n"]);
					({platform_define, Regex, D}) ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("ERLC_OPTS += -D" ++ ToList(D) ++ "=1\n");
							false -> ok
						end;
					({parse_transform, PT}) ->
						Write("ERLC_OPTS += +'{parse_transform, " ++ ToList(PT) ++ "}'\n");
					(_) -> ok
				end, ErlOpts)
		end,
		Write("\n")
	end(),
	GetHexVsn = fun(N) ->
		case file:consult("$(call core_native_path,$(DEPS_DIR)/$1/rebar.lock)") of
			{ok, Lock} ->
				io:format("~p~n", [Lock]),
				case lists:keyfind("1.1.0", 1, Lock) of
					{_, LockPkgs} ->
						io:format("~p~n", [LockPkgs]),
						case lists:keyfind(atom_to_binary(N, latin1), 1, LockPkgs) of
							{_, {pkg, _, Vsn}, _} ->
								io:format("~p~n", [Vsn]),
								{N, {hex, binary_to_list(Vsn)}};
							_ ->
								false
						end;
					_ ->
						false
				end;
			_ ->
				false
		end
	end,
	fun() ->
		File = case lists:keyfind(deps, 1, Conf) of
			false -> [];
			{_, Deps} ->
				[begin case case Dep of
							N when is_atom(N) -> GetHexVsn(N);
							{N, S} when is_atom(N), is_list(S) -> {N, {hex, S}};
							{N, S} when is_tuple(S) -> {N, S};
							{N, _, S} -> {N, S};
							{N, _, S, _} -> {N, S};
							_ -> false
						end of
					false -> ok;
					{Name, Source} ->
						{Method, Repo, Commit} = case Source of
							{hex, V} -> {hex, V, undefined};
							{git, R} -> {git, R, master};
							{M, R, {branch, C}} -> {M, R, C};
							{M, R, {ref, C}} -> {M, R, C};
							{M, R, {tag, C}} -> {M, R, C};
							{M, R, C} -> {M, R, C}
						end,
						Write(io_lib:format("DEPS += ~s\ndep_~s = ~s ~s ~s~n", [Name, Name, Method, Repo, Commit]))
				end end || Dep <- Deps]
		end
	end(),
	fun() ->
		case lists:keyfind(erl_first_files, 1, Conf) of
			false -> ok;
			{_, Files} ->
				Names = [[" ", case lists:reverse(F) of
					"lre." ++ Elif -> lists:reverse(Elif);
					Elif -> lists:reverse(Elif)
				end] || "src/" ++ F <- Files],
				Write(io_lib:format("COMPILE_FIRST +=~s\n", [Names]))
		end
	end(),
	Write("\n\nrebar_dep: preprocess pre-deps deps pre-app app\n"),
	Write("\npreprocess::\n"),
	Write("\npre-deps::\n"),
	Write("\npre-app::\n"),
	PatchHook = fun(Cmd) ->
		Cmd2 = re:replace(Cmd, "^([g]?make)(.*)( -C.*)", "\\\\1\\\\3\\\\2", [{return, list}]),
		case Cmd2 of
			"make -C" ++ Cmd1 -> "$$\(MAKE) -C" ++ Escape(Cmd1);
			"gmake -C" ++ Cmd1 -> "$$\(MAKE) -C" ++ Escape(Cmd1);
			"make " ++ Cmd1 -> "$$\(MAKE) -f Makefile.orig.mk " ++ Escape(Cmd1);
			"gmake " ++ Cmd1 -> "$$\(MAKE) -f Makefile.orig.mk " ++ Escape(Cmd1);
			_ -> Escape(Cmd)
		end
	end,
	fun() ->
		case lists:keyfind(pre_hooks, 1, Conf) of
			false -> ok;
			{_, Hooks} ->
				[case H of
					{'get-deps', Cmd} ->
						Write("\npre-deps::\n\t" ++ PatchHook(Cmd) ++ "\n");
					{compile, Cmd} ->
						Write("\npre-app::\n\tCC=$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
					{Regex, compile, Cmd} ->
						case rebar_utils:is_arch(Regex) of
							true -> Write("\npre-app::\n\tCC=$$\(CC) " ++ PatchHook(Cmd) ++ "\n");
							false -> ok
						end;
					_ -> ok
				end || H <- Hooks]
		end
	end(),
	ShellToMk = fun(V) ->
		re:replace(re:replace(V, "(\\\\$$)(\\\\w*)", "\\\\1(\\\\2)", [global]),
			"-Werror\\\\b", "", [{return, list}, global])
	end,
	PortSpecs = fun() ->
		case lists:keyfind(port_specs, 1, Conf) of
			false ->
				case filelib:is_dir("$(call core_native_path,$(DEPS_DIR)/$1/c_src)") of
					false -> [];
					true ->
						[{"priv/" ++ proplists:get_value(so_name, Conf, "$(1)_drv.so"),
							proplists:get_value(port_sources, Conf, ["c_src/*.c"]), []}]
				end;
			{_, Specs} ->
				lists:flatten([case S of
					{Output, Input} -> {ShellToMk(Output), Input, []};
					{Regex, Output, Input} ->
						case rebar_utils:is_arch(Regex) of
							true -> {ShellToMk(Output), Input, []};
							false -> []
						end;
					{Regex, Output, Input, [{env, Env}]} ->
						case rebar_utils:is_arch(Regex) of
							true -> {ShellToMk(Output), Input, Env};
							false -> []
						end
				end || S <- Specs])
		end
	end(),
	PortSpecWrite = fun (Text) ->
		file:write_file("$(call core_native_path,$(DEPS_DIR)/$1/c_src/Makefile.erlang.mk)", Text, [append])
	end,
	case PortSpecs of
		[] -> ok;
		_ ->
			Write("\npre-app::\n\t$$\(MAKE) -f c_src/Makefile.erlang.mk\n"),
			PortSpecWrite(io_lib:format("ERL_CFLAGS ?= -finline-functions -Wall -fPIC -I \\"~s/erts-~s/include\\" -I \\"~s\\"\n",
				[code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)])),
			PortSpecWrite(io_lib:format("ERL_LDFLAGS ?= -L \\"~s\\" -lerl_interface -lei\n",
				[code:lib_dir(erl_interface, lib)])),
			[PortSpecWrite(["\n", E, "\n"]) || E <- OsEnv],
			FilterEnv = fun(Env) ->
				lists:flatten([case E of
					{_, _} -> E;
					{Regex, K, V} ->
						case rebar_utils:is_arch(Regex) of
							true -> {K, V};
							false -> []
						end
				end || E <- Env])
			end,
			MergeEnv = fun(Env) ->
				lists:foldl(fun ({K, V}, Acc) ->
					case lists:keyfind(K, 1, Acc) of
						false -> [{K, rebar_utils:expand_env_variable(V, K, "")}|Acc];
						{_, V0} -> [{K, rebar_utils:expand_env_variable(V, K, V0)}|Acc]
					end
				end, [], Env)
			end,
			PortEnv = case lists:keyfind(port_env, 1, Conf) of
				false -> [];
				{_, PortEnv0} -> FilterEnv(PortEnv0)
			end,
			PortSpec = fun ({Output, Input0, Env}) ->
				filelib:ensure_dir("$(call core_native_path,$(DEPS_DIR)/$1/)" ++ Output),
				Input = [[" ", I] || I <- Input0],
				PortSpecWrite([
					[["\n", K, " = ", ShellToMk(V)] || {K, V} <- lists:reverse(MergeEnv(PortEnv))],
					case $(PLATFORM) of
						darwin -> "\n\nLDFLAGS += -flat_namespace -undefined suppress";
						_ -> ""
					end,
					"\n\nall:: ", Output, "\n\n",
					"%.o: %.c\n\t$$\(CC) -c -o $$\@ $$\< $$\(CFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.C\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cc\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					"%.o: %.cpp\n\t$$\(CXX) -c -o $$\@ $$\< $$\(CXXFLAGS) $$\(ERL_CFLAGS) $$\(DRV_CFLAGS) $$\(EXE_CFLAGS)\n\n",
					[[Output, ": ", K, " += ", ShellToMk(V), "\n"] || {K, V} <- lists:reverse(MergeEnv(FilterEnv(Env)))],
					Output, ": $$\(foreach ext,.c .C .cc .cpp,",
						"$$\(patsubst %$$\(ext),%.o,$$\(filter %$$\(ext),$$\(wildcard", Input, "))))\n",
					"\t$$\(CC) -o $$\@ $$\? $$\(LDFLAGS) $$\(ERL_LDFLAGS) $$\(DRV_LDFLAGS) $$\(EXE_LDFLAGS)",
					case {filename:extension(Output), $(PLATFORM)} of
					    {[], _} -> "\n";
					    {_, darwin} -> "\n";
					    _ -> " -shared\n"
					end])
			end,
			[PortSpec(S) || S <- PortSpecs]
	end,
	Write("\ninclude $$\(if $$\(ERLANG_MK_FILENAME),$$\(ERLANG_MK_FILENAME),erlang.mk)"),
	RunPlugin = fun(Plugin, Step) ->
		case erlang:function_exported(Plugin, Step, 2) of
			false -> ok;
			true ->
				c:cd("$(call core_native_path,$(DEPS_DIR)/$1/)"),
				Ret = Plugin:Step({config, "", Conf, dict:new(), dict:new(), dict:new(),
					dict:store(base_dir, "", dict:new())}, undefined),
				io:format("rebar plugin ~p step ~p ret ~p~n", [Plugin, Step, Ret])
		end
	end,
	fun() ->
		case lists:keyfind(plugins, 1, Conf) of
			false -> ok;
			{_, Plugins} ->
				[begin
					case lists:keyfind(deps, 1, Conf) of
						false -> ok;
						{_, Deps} ->
							case lists:keyfind(P, 1, Deps) of
								false -> ok;
								_ ->
									Path = "$(call core_native_path,$(DEPS_DIR)/)" ++ atom_to_list(P),
									io:format("~s", [os:cmd("$(MAKE) -C $(call core_native_path,$(DEPS_DIR)/$1) " ++ Path)]),
									io:format("~s", [os:cmd("$(MAKE) -C " ++ Path ++ " IS_DEP=1")]),
									code:add_patha(Path ++ "/ebin")
							end
					end
				end || P <- Plugins],
				[case code:load_file(P) of
					{module, P} -> ok;
					_ ->
						case lists:keyfind(plugin_dir, 1, Conf) of
							false -> ok;
							{_, PluginsDir} ->
								ErlFile = "$(call core_native_path,$(DEPS_DIR)/$1/)" ++ PluginsDir ++ "/" ++ atom_to_list(P) ++ ".erl",
								{ok, P, Bin} = compile:file(ErlFile, [binary]),
								{module, P} = code:load_binary(P, ErlFile, Bin)
						end
				end || P <- Plugins],
				[RunPlugin(P, preprocess) || P <- Plugins],
				[RunPlugin(P, pre_compile) || P <- Plugins],
				[RunPlugin(P, compile) || P <- Plugins]
		end
	end(),
	halt()
endef

define dep_autopatch_appsrc_script.erl
	AppSrc = "$(call core_native_path,$(DEPS_DIR)/$1/src/$1.app.src)",
	AppSrcScript = AppSrc ++ ".script",
	{ok, Conf0} = file:consult(AppSrc),
	Bindings0 = erl_eval:new_bindings(),
	Bindings1 = erl_eval:add_binding('CONFIG', Conf0, Bindings0),
	Bindings = erl_eval:add_binding('SCRIPT', AppSrcScript, Bindings1),
	Conf = case file:script(AppSrcScript, Bindings) of
		{ok, [C]} -> C;
		{ok, C} -> C
	end,
	ok = file:write_file(AppSrc, io_lib:format("~p.~n", [Conf])),
	halt()
endef

define dep_autopatch_appsrc.erl
	AppSrcOut = "$(call core_native_path,$(DEPS_DIR)/$1/src/$1.app.src)",
	AppSrcIn = case filelib:is_regular(AppSrcOut) of false -> "$(call core_native_path,$(DEPS_DIR)/$1/ebin/$1.app)"; true -> AppSrcOut end,
	case filelib:is_regular(AppSrcIn) of
		false -> ok;
		true ->
			{ok, [{application, $(1), L0}]} = file:consult(AppSrcIn),
			L1 = lists:keystore(modules, 1, L0, {modules, []}),
			L2 = case lists:keyfind(vsn, 1, L1) of
				{_, git} -> lists:keyreplace(vsn, 1, L1, {vsn, "git"});
				{_, {cmd, _}} -> lists:keyreplace(vsn, 1, L1, {vsn, "cmd"});
				_ -> L1
			end,
			L3 = case lists:keyfind(registered, 1, L2) of false -> [{registered, []}|L2]; _ -> L2 end,
			ok = file:write_file(AppSrcOut, io_lib:format("~p.~n", [{application, $(1), L3}])),
			case AppSrcOut of AppSrcIn -> ok; _ -> ok = file:delete(AppSrcIn) end
	end,
	halt()
endef

define dep_fetch_git
	git clone -q -n -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)); \
	cd $(DEPS_DIR)/$(call dep_name,$(1)) && git checkout -q $(call dep_commit,$(1));
endef

define dep_fetch_git-submodule
	git submodule update --init -- $(DEPS_DIR)/$1;
endef

define dep_fetch_hg
	hg clone -q -U $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)); \
	cd $(DEPS_DIR)/$(call dep_name,$(1)) && hg update -q $(call dep_commit,$(1));
endef

define dep_fetch_svn
	svn checkout -q $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

define dep_fetch_cp
	cp -R $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

define dep_fetch_ln
	ln -s $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1));
endef

# Hex only has a package version. No need to look in the Erlang.mk packages.
define dep_fetch_hex
	mkdir -p $(ERLANG_MK_TMP)/hex $(DEPS_DIR)/$1; \
	$(call core_http_get,$(ERLANG_MK_TMP)/hex/$1.tar,\
		https://repo.hex.pm/tarballs/$1-$(strip $(word 2,$(dep_$1))).tar); \
	tar -xOf $(ERLANG_MK_TMP)/hex/$1.tar contents.tar.gz | tar -C $(DEPS_DIR)/$1 -xzf -;
endef

define dep_fetch_fail
	echo "Error: Unknown or invalid dependency: $(1)." >&2; \
	exit 78;
endef

# Kept for compatibility purposes with older Erlang.mk configuration.
define dep_fetch_legacy
	$(warning WARNING: '$(1)' dependency configuration uses deprecated format.) \
	git clone -q -n -- $(word 1,$(dep_$(1))) $(DEPS_DIR)/$(1); \
	cd $(DEPS_DIR)/$(1) && git checkout -q $(if $(word 2,$(dep_$(1))),$(word 2,$(dep_$(1))),master);
endef

define dep_fetch
	$(if $(dep_$(1)), \
		$(if $(dep_fetch_$(word 1,$(dep_$(1)))), \
			$(word 1,$(dep_$(1))), \
			$(if $(IS_DEP),legacy,fail)), \
		$(if $(filter $(1),$(PACKAGES)), \
			$(pkg_$(1)_fetch), \
			fail))
endef

define dep_target
$(DEPS_DIR)/$(call dep_name,$1):
	$(eval DEP_NAME := $(call dep_name,$1))
	$(eval DEP_STR := $(if $(filter-out $1,$(DEP_NAME)),$1,"$1 ($(DEP_NAME))"))
	$(verbose) if test -d $(APPS_DIR)/$(DEP_NAME); then \
		echo "Error: Dependency" $(DEP_STR) "conflicts with application found in $(APPS_DIR)/$(DEP_NAME)." >&2; \
		exit 17; \
	fi
	$(verbose) mkdir -p $(DEPS_DIR)
	$(dep_verbose) $(call dep_fetch_$(strip $(call dep_fetch,$(1))),$(1))
	$(verbose) if [ -f $(DEPS_DIR)/$(1)/configure.ac -o -f $(DEPS_DIR)/$(1)/configure.in ] \
			&& [ ! -f $(DEPS_DIR)/$(1)/configure ]; then \
		echo " AUTO  " $(1); \
		cd $(DEPS_DIR)/$(1) && autoreconf -Wall -vif -I m4; \
	fi
	- $(verbose) if [ -f $(DEPS_DIR)/$(DEP_NAME)/configure ]; then \
		echo " CONF  " $(DEP_STR); \
		cd $(DEPS_DIR)/$(DEP_NAME) && ./configure; \
	fi
ifeq ($(filter $(1),$(NO_AUTOPATCH)),)
	$(verbose) if [ "$(1)" = "amqp_client" -a "$(RABBITMQ_CLIENT_PATCH)" ]; then \
		if [ ! -d $(DEPS_DIR)/rabbitmq-codegen ]; then \
			echo " PATCH  Downloading rabbitmq-codegen"; \
			git clone https://github.com/rabbitmq/rabbitmq-codegen.git $(DEPS_DIR)/rabbitmq-codegen; \
		fi; \
		if [ ! -d $(DEPS_DIR)/rabbitmq-server ]; then \
			echo " PATCH  Downloading rabbitmq-server"; \
			git clone https://github.com/rabbitmq/rabbitmq-server.git $(DEPS_DIR)/rabbitmq-server; \
		fi; \
		ln -s $(DEPS_DIR)/amqp_client/deps/rabbit_common-0.0.0 $(DEPS_DIR)/rabbit_common; \
	elif [ "$(1)" = "rabbit" -a "$(RABBITMQ_SERVER_PATCH)" ]; then \
		if [ ! -d $(DEPS_DIR)/rabbitmq-codegen ]; then \
			echo " PATCH  Downloading rabbitmq-codegen"; \
			git clone https://github.com/rabbitmq/rabbitmq-codegen.git $(DEPS_DIR)/rabbitmq-codegen; \
		fi \
	else \
		$$(call dep_autopatch,$(DEP_NAME)) \
	fi
endif
endef

$(foreach dep,$(BUILD_DEPS) $(DEPS),$(eval $(call dep_target,$(dep))))

ifndef IS_APP
clean:: clean-apps

clean-apps:
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep clean IS_APP=1; \
	done

distclean:: distclean-apps

distclean-apps:
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep distclean IS_APP=1; \
	done
endif

ifndef SKIP_DEPS
distclean:: distclean-deps

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)
endif

# Forward-declare variables used in core/deps-tools.mk. This is required
# in case plugins use them.

ERLANG_MK_RECURSIVE_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-deps-list.log
ERLANG_MK_RECURSIVE_DOC_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-doc-deps-list.log
ERLANG_MK_RECURSIVE_REL_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-rel-deps-list.log
ERLANG_MK_RECURSIVE_TEST_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-test-deps-list.log
ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST = $(ERLANG_MK_TMP)/recursive-shell-deps-list.log

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Verbosity.

proto_verbose_0 = @echo " PROTO " $(filter %.proto,$(?F));
proto_verbose = $(proto_verbose_$(V))

# Core targets.

define compile_proto
	$(verbose) mkdir -p ebin/ include/
	$(proto_verbose) $(call erlang,$(call compile_proto.erl,$(1)))
	$(proto_verbose) erlc +debug_info -o ebin/ ebin/*.erl
	$(verbose) rm ebin/*.erl
endef

define compile_proto.erl
	[begin
		protobuffs_compile:generate_source(F,
			[{output_include_dir, "./include"},
				{output_src_dir, "./ebin"}])
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(sort $(call core_find,src/,*.proto))
	$(if $(strip $?),$(call compile_proto,$?))
endif

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-app

# Configuration.

ERLC_OPTS ?= -Werror +debug_info +warn_export_vars +warn_shadow_vars \
	+warn_obsolete_guard # +bin_opt_info +warn_export_all +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))
ERLC_EXCLUDE ?=
ERLC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .erl,$(ERLC_EXCLUDE)))

ERLC_ASN1_OPTS ?=

ERLC_MIB_OPTS ?=
COMPILE_MIB_FIRST ?=
COMPILE_MIB_FIRST_PATHS = $(addprefix mibs/,$(addsuffix .mib,$(COMPILE_MIB_FIRST)))

# Verbosity.

app_verbose_0 = @echo " APP   " $(PROJECT);
app_verbose_2 = set -x;
app_verbose = $(app_verbose_$(V))

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose_2 = set -x;
appsrc_verbose = $(appsrc_verbose_$(V))

makedep_verbose_0 = @echo " DEPEND" $(PROJECT).d;
makedep_verbose_2 = set -x;
makedep_verbose = $(makedep_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(?F)));
erlc_verbose_2 = set -x;
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose_2 = set -x;
xyrl_verbose = $(xyrl_verbose_$(V))

asn1_verbose_0 = @echo " ASN1  " $(filter %.asn1,$(?F));
asn1_verbose_2 = set -x;
asn1_verbose = $(asn1_verbose_$(V))

mib_verbose_0 = @echo " MIB   " $(filter %.bin %.mib,$(?F));
mib_verbose_2 = set -x;
mib_verbose = $(mib_verbose_$(V))

ifneq ($(wildcard src/),)

# Targets.

ifeq ($(wildcard ebin/test),)
app:: deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
else
app:: clean deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build
endif

ifeq ($(wildcard src/$(PROJECT_MOD).erl),)
define app_file
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{id$(comma)$(space)"$(1)"}$(comma))
	{modules, [$(call comma_list,$(2))]},
	{registered, []},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
else
define app_file
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{id$(comma)$(space)"$(1)"}$(comma))
	{modules, [$(call comma_list,$(2))]},
	{registered, [$(call comma_list,$(PROJECT)_sup $(PROJECT_REGISTERED))]},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{mod, {$(PROJECT_MOD), []}},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
endif

app-build: ebin/$(PROJECT).app
	$(verbose) :

# Source files.

ALL_SRC_FILES := $(sort $(call core_find,src/,*))

ERL_FILES := $(filter %.erl,$(ALL_SRC_FILES))
CORE_FILES := $(filter %.core,$(ALL_SRC_FILES))

# ASN.1 files.

ifneq ($(wildcard asn1/),)
ASN1_FILES = $(sort $(call core_find,asn1/,*.asn1))
ERL_FILES += $(addprefix src/,$(patsubst %.asn1,%.erl,$(notdir $(ASN1_FILES))))

define compile_asn1
	$(verbose) mkdir -p include/
	$(asn1_verbose) erlc -v -I include/ -o asn1/ +noobj $(ERLC_ASN1_OPTS) $(1)
	$(verbose) mv asn1/*.erl src/
	$(verbose) mv asn1/*.hrl include/
	$(verbose) mv asn1/*.asn1db include/
endef

$(PROJECT).d:: $(ASN1_FILES)
	$(if $(strip $?),$(call compile_asn1,$?))
endif

# SNMP MIB files.

ifneq ($(wildcard mibs/),)
MIB_FILES = $(sort $(call core_find,mibs/,*.mib))

$(PROJECT).d:: $(COMPILE_MIB_FIRST_PATHS) $(MIB_FILES)
	$(verbose) mkdir -p include/ priv/mibs/
	$(mib_verbose) erlc -v $(ERLC_MIB_OPTS) -o priv/mibs/ -I priv/mibs/ $?
	$(mib_verbose) erlc -o include/ -- $(addprefix priv/mibs/,$(patsubst %.mib,%.bin,$(notdir $?)))
endif

# Leex and Yecc files.

XRL_FILES := $(filter %.xrl,$(ALL_SRC_FILES))
XRL_ERL_FILES = $(addprefix src/,$(patsubst %.xrl,%.erl,$(notdir $(XRL_FILES))))
ERL_FILES += $(XRL_ERL_FILES)

YRL_FILES := $(filter %.yrl,$(ALL_SRC_FILES))
YRL_ERL_FILES = $(addprefix src/,$(patsubst %.yrl,%.erl,$(notdir $(YRL_FILES))))
ERL_FILES += $(YRL_ERL_FILES)

$(PROJECT).d:: $(XRL_FILES) $(YRL_FILES)
	$(if $(strip $?),$(xyrl_verbose) erlc -v -o src/ $(YRL_ERLC_OPTS) $?)

# Erlang and Core Erlang files.

define makedep.erl
	E = ets:new(makedep, [bag]),
	G = digraph:new([acyclic]),
	ErlFiles = lists:usort(string:tokens("$(ERL_FILES)", " ")),
	DepsDir = "$(call core_native_path,$(DEPS_DIR))",
	AppsDir = "$(call core_native_path,$(APPS_DIR))",
	DepsDirsSrc = "$(if $(wildcard $(DEPS_DIR)/*/src), $(call core_native_path,$(wildcard $(DEPS_DIR)/*/src)))",
	DepsDirsInc = "$(if $(wildcard $(DEPS_DIR)/*/include), $(call core_native_path,$(wildcard $(DEPS_DIR)/*/include)))",
	AppsDirsSrc = "$(if $(wildcard $(APPS_DIR)/*/src), $(call core_native_path,$(wildcard $(APPS_DIR)/*/src)))",
	AppsDirsInc = "$(if $(wildcard $(APPS_DIR)/*/include), $(call core_native_path,$(wildcard $(APPS_DIR)/*/include)))",
	DepsDirs = lists:usort(string:tokens(DepsDirsSrc++DepsDirsInc, " ")),
	AppsDirs = lists:usort(string:tokens(AppsDirsSrc++AppsDirsInc, " ")),
	Modules = [{list_to_atom(filename:basename(F, ".erl")), F} || F <- ErlFiles],
	Add = fun (Mod, Dep) ->
		case lists:keyfind(Dep, 1, Modules) of
			false -> ok;
			{_, DepFile} ->
				{_, ModFile} = lists:keyfind(Mod, 1, Modules),
				ets:insert(E, {ModFile, DepFile}),
				digraph:add_vertex(G, Mod),
				digraph:add_vertex(G, Dep),
				digraph:add_edge(G, Mod, Dep)
		end
	end,
	AddHd = fun (F, Mod, DepFile) ->
		case file:open(DepFile, [read]) of
			{error, enoent} ->
				ok;
			{ok, Fd} ->
				{_, ModFile} = lists:keyfind(Mod, 1, Modules),
				case ets:match(E, {ModFile, DepFile}) of
					[] ->
						ets:insert(E, {ModFile, DepFile}),
						F(F, Fd, Mod,0);
					_ -> ok
				end
		end
	end,
	SearchHrl = fun
		F(_Hrl, []) -> {error,enoent};
		F(Hrl, [Dir|Dirs]) ->
			HrlF = filename:join([Dir,Hrl]),
			case filelib:is_file(HrlF) of
				true  ->
				{ok, HrlF};
				false -> F(Hrl,Dirs)
			end
	end,
	Attr = fun
		(_F, Mod, behavior, Dep) ->
			Add(Mod, Dep);
		(_F, Mod, behaviour, Dep) ->
			Add(Mod, Dep);
		(_F, Mod, compile, {parse_transform, Dep}) ->
			Add(Mod, Dep);
		(_F, Mod, compile, Opts) when is_list(Opts) ->
			case proplists:get_value(parse_transform, Opts) of
				undefined -> ok;
				Dep -> Add(Mod, Dep)
			end;
		(F, Mod, include, Hrl) ->
			case SearchHrl(Hrl, ["src", "include",AppsDir,DepsDir]++AppsDirs++DepsDirs) of
				{ok, FoundHrl} -> AddHd(F, Mod, FoundHrl);
				{error, _} -> false
			end;
		(F, Mod, include_lib, Hrl) ->
			case SearchHrl(Hrl, ["src", "include",AppsDir,DepsDir]++AppsDirs++DepsDirs) of
				{ok, FoundHrl} -> AddHd(F, Mod, FoundHrl);
				{error, _} -> false
			end;
		(F, Mod, import, {Imp, _}) ->
			IsFile =
				case lists:keyfind(Imp, 1, Modules) of
					false -> false;
					{_, FilePath} -> filelib:is_file(FilePath)
				end,
			case IsFile of
				false -> ok;
				true -> Add(Mod, Imp)
			end;
		(_, _, _, _) -> ok
	end,
	MakeDepend = fun
		(F, Fd, Mod, StartLocation) ->
			{ok, Filename} = file:pid2name(Fd),
			case io:parse_erl_form(Fd, undefined, StartLocation) of
				{ok, AbsData, EndLocation} ->
					case AbsData of
						{attribute, _, Key, Value} ->
							Attr(F, Mod, Key, Value),
							F(F, Fd, Mod, EndLocation);
						_ -> F(F, Fd, Mod, EndLocation)
					end;
				{eof, _ } -> file:close(Fd);
				{error, ErrorDescription } ->
					file:close(Fd);
				{error, ErrorInfo, ErrorLocation} ->
					F(F, Fd, Mod, ErrorLocation)
			end,
			ok
	end,
	[begin
		Mod = list_to_atom(filename:basename(F, ".erl")),
		{ok, Fd} = file:open(F, [read]),
		MakeDepend(MakeDepend, Fd, Mod,0)
	end || F <- ErlFiles],
	Depend = sofs:to_external(sofs:relation_to_family(sofs:relation(ets:tab2list(E)))),
	CompileFirst = [X || X <- lists:reverse(digraph_utils:topsort(G)), [] =/= digraph:in_neighbours(G, X)],
	TargetPath = fun(Target) ->
		case lists:keyfind(Target, 1, Modules) of
			false -> "";
			{_, DepFile} ->
				DirSubname = tl(string:tokens(filename:dirname(DepFile), "/")),
				string:join(DirSubname ++ [atom_to_list(Target)], "/")
		end
	end,
	ok = file:write_file("$(1)", [
		[[F, "::", [[" ", D] || D <- Deps], "; @touch \$$@\n"] || {F, Deps} <- Depend],
		"\nCOMPILE_FIRST +=", [[" ", TargetPath(CF)] || CF <- CompileFirst], "\n"
	]),
	halt()
endef

ifeq ($(if $(NO_MAKEDEP),$(wildcard $(PROJECT).d),),)
$(PROJECT).d:: $(ERL_FILES) $(call core_find,include/,*.hrl) $(MAKEFILE_LIST)
	$(makedep_verbose) $(call erlang,$(call makedep.erl,$@))
endif

ifneq ($(words $(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES)),0)
# Rebuild everything when the Makefile changes.
$(ERLANG_MK_TMP)/last-makefile-change: $(MAKEFILE_LIST)
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) if test -f $@; then \
		touch $(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES); \
		touch -c $(PROJECT).d; \
	fi
	$(verbose) touch $@

$(ERL_FILES) $(CORE_FILES) $(ASN1_FILES) $(MIB_FILES) $(XRL_FILES) $(YRL_FILES):: $(ERLANG_MK_TMP)/last-makefile-change
ebin/$(PROJECT).app:: $(ERLANG_MK_TMP)/last-makefile-change
endif

include $(wildcard $(PROJECT).d)

ebin/$(PROJECT).app:: ebin/

ebin/:
	$(verbose) mkdir -p ebin/

define compile_erl
	$(erlc_verbose) erlc -v $(if $(IS_DEP),$(filter-out -Werror,$(ERLC_OPTS)),$(ERLC_OPTS)) -o ebin/ \
		-pa ebin/ -I include/ $(filter-out $(ERLC_EXCLUDE_PATHS),$(COMPILE_FIRST_PATHS) $(1))
endef

ebin/$(PROJECT).app:: $(ERL_FILES) $(CORE_FILES) $(wildcard src/$(PROJECT).app.src)
	$(eval FILES_TO_COMPILE := $(filter-out src/$(PROJECT).app.src,$?))
	$(if $(strip $(FILES_TO_COMPILE)),$(call compile_erl,$(FILES_TO_COMPILE)))
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(filter-out $(ERLC_EXCLUDE_PATHS),$(ERL_FILES) $(CORE_FILES) $(BEAM_FILES)))))))
ifeq ($(wildcard src/$(PROJECT).app.src),)
	$(app_verbose) printf '$(subst %,%%,$(subst $(newline),\n,$(subst ','\'',$(call app_file,$(GITDESCRIBE),$(MODULES)))))' \
		> ebin/$(PROJECT).app
else
	$(verbose) if [ -z "$$(grep -e '^[^%]*{\s*modules\s*,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk README for instructions." >&2; \
		exit 1; \
	fi
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{[[:space:]]*modules[[:space:]]*,[[:space:]]*\[\]}/{modules, \[$(call comma_list,$(MODULES))\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(subst /,\/,$(GITDESCRIBE))\"}/" \
		> ebin/$(PROJECT).app
endif

clean:: clean-app

clean-app:
	$(gen_verbose) rm -rf $(PROJECT).d ebin/ priv/mibs/ $(XRL_ERL_FILES) $(YRL_ERL_FILES) \
		$(addprefix include/,$(patsubst %.mib,%.hrl,$(notdir $(MIB_FILES)))) \
		$(addprefix include/,$(patsubst %.asn1,%.hrl,$(notdir $(ASN1_FILES)))) \
		$(addprefix include/,$(patsubst %.asn1,%.asn1db,$(notdir $(ASN1_FILES)))) \
		$(addprefix src/,$(patsubst %.asn1,%.erl,$(notdir $(ASN1_FILES))))

endif

# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: docs-deps

# Configuration.

ALL_DOC_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DOC_DEPS))

# Targets.

$(foreach dep,$(DOC_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
doc-deps:
else
doc-deps: $(ALL_DOC_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_DOC_DEPS_DIRS) ; do $(MAKE) -C $$dep; done
endif

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: rel-deps

# Configuration.

ALL_REL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(REL_DEPS))

# Targets.

$(foreach dep,$(REL_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
rel-deps:
else
rel-deps: $(ALL_REL_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_REL_DEPS_DIRS) ; do $(MAKE) -C $$dep; done
endif

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: test-deps test-dir test-build clean-test-dir

# Configuration.

TEST_DIR ?= $(CURDIR)/test

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS += -DTEST=1

# Targets.

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
test-deps:
else
test-deps: $(ALL_TEST_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep IS_DEP=1; done
endif

ifneq ($(wildcard $(TEST_DIR)),)
test-dir:
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(call core_find,$(TEST_DIR)/,*.erl) -pa ebin/
endif

ifeq ($(wildcard src),)
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: clean deps test-deps
	$(verbose) $(MAKE) --no-print-directory test-dir ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
else
ifeq ($(wildcard ebin/test),)
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: clean deps test-deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
	$(gen_verbose) touch ebin/test
else
test-build:: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build:: deps test-deps $(PROJECT).d
	$(verbose) $(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(call escape_dquotes,$(TEST_ERLC_OPTS))"
endif

clean:: clean-test-dir

clean-test-dir:
ifneq ($(wildcard $(TEST_DIR)/*.beam),)
	$(gen_verbose) rm -f $(TEST_DIR)/*.beam
endif
endif

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: rebar.config

# We strip out -Werror because we don't want to fail due to
# warnings when used as a dependency.

compat_prepare_erlc_opts = $(shell echo "$1" | sed 's/, */,/g')

define compat_convert_erlc_opts
$(if $(filter-out -Werror,$1),\
	$(if $(findstring +,$1),\
		$(shell echo $1 | cut -b 2-)))
endef

define compat_erlc_opts_to_list
[$(call comma_list,$(foreach o,$(call compat_prepare_erlc_opts,$1),$(call compat_convert_erlc_opts,$o)))]
endef

define compat_rebar_config
{deps, [
$(call comma_list,$(foreach d,$(DEPS),\
	$(if $(filter hex,$(call dep_fetch,$d)),\
		{$(call dep_name,$d)$(comma)"$(call dep_repo,$d)"},\
		{$(call dep_name,$d)$(comma)".*"$(comma){git,"$(call dep_repo,$d)"$(comma)"$(call dep_commit,$d)"}})))
]}.
{erl_opts, $(call compat_erlc_opts_to_list,$(ERLC_OPTS))}.
endef

$(eval _compat_rebar_config = $$(compat_rebar_config))
$(eval export _compat_rebar_config)

rebar.config:
	$(gen_verbose) echo "$${_compat_rebar_config}" > rebar.config

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter asciideck,$(DEPS) $(DOC_DEPS)),asciideck)

.PHONY: asciidoc asciidoc-guide asciidoc-manual install-asciidoc distclean-asciidoc-guide distclean-asciidoc-manual

# Core targets.

docs:: asciidoc

distclean:: distclean-asciidoc-guide distclean-asciidoc-manual

# Plugin-specific targets.

asciidoc: asciidoc-guide asciidoc-manual

# User guide.

ifeq ($(wildcard doc/src/guide/book.asciidoc),)
asciidoc-guide:
else
asciidoc-guide: distclean-asciidoc-guide doc-deps
	a2x -v -f pdf doc/src/guide/book.asciidoc && mv doc/src/guide/book.pdf doc/guide.pdf
	a2x -v -f chunked doc/src/guide/book.asciidoc && mv doc/src/guide/book.chunked/ doc/html/

distclean-asciidoc-guide:
	$(gen_verbose) rm -rf doc/html/ doc/guide.pdf
endif

# Man pages.

ASCIIDOC_MANUAL_FILES := $(wildcard doc/src/manual/*.asciidoc)

ifeq ($(ASCIIDOC_MANUAL_FILES),)
asciidoc-manual:
else

# Configuration.

MAN_INSTALL_PATH ?= /usr/local/share/man
MAN_SECTIONS ?= 3 7
MAN_PROJECT ?= $(shell echo $(PROJECT) | sed 's/^./\U&\E/')
MAN_VERSION ?= $(PROJECT_VERSION)

# Plugin-specific targets.

define asciidoc2man.erl
try
	[begin
		io:format(" ADOC   ~s~n", [F]),
		ok = asciideck:to_manpage(asciideck:parse_file(F), #{
			compress => gzip,
			outdir => filename:dirname(F),
			extra2 => "$(MAN_PROJECT) $(MAN_VERSION)",
			extra3 => "$(MAN_PROJECT) Function Reference"
		})
	end || F <- [$(shell echo $(addprefix $(comma)\",$(addsuffix \",$1)) | sed 's/^.//')]],
	halt(0)
catch C:E ->
	io:format("Exception ~p:~p~nStacktrace: ~p~n", [C, E, erlang:get_stacktrace()]),
	halt(1)
end.
endef

asciidoc-manual:: doc-deps

asciidoc-manual:: $(ASCIIDOC_MANUAL_FILES)
	$(call erlang,$(call asciidoc2man.erl,$?))
	$(foreach s,$(MAN_SECTIONS),mkdir -p doc/man$s/ && mv doc/src/manual/*.$s.gz doc/man$s/;)

install-docs:: install-asciidoc

install-asciidoc: asciidoc-manual
	$(foreach s,$(MAN_SECTIONS),\
		mkdir -p $(MAN_INSTALL_PATH)/man$s/ && \
		install -g `id -g` -o `id -u` -m 0644 doc/man$s/*.gz $(MAN_INSTALL_PATH)/man$s/;)

distclean-asciidoc-manual:
	$(gen_verbose) rm -rf $(addprefix doc/man,$(MAN_SECTIONS))
endif
endif

# Copyright (c) 2014-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: bootstrap bootstrap-lib bootstrap-rel new list-templates

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"Bootstrap targets:" \
		"  bootstrap          Generate a skeleton of an OTP application" \
		"  bootstrap-lib      Generate a skeleton of an OTP library" \
		"  bootstrap-rel      Generate the files needed to build a release" \
		"  new-app in=NAME    Create a new local OTP application NAME" \
		"  new-lib in=NAME    Create a new local OTP library NAME" \
		"  new t=TPL n=NAME   Generate a module NAME based on the template TPL" \
		"  new t=T n=N in=APP Generate a module NAME based on the template TPL in APP" \
		"  list-templates     List available templates"

# Bootstrap templates.

define bs_appsrc
{application, $p, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]},
	{mod, {$p_app, []}},
	{env, []}
]}.
endef

define bs_appsrc_lib
{application, $p, [
	{description, ""},
	{vsn, "0.1.0"},
	{id, "git"},
	{modules, []},
	{registered, []},
	{applications, [
		kernel,
		stdlib
	]}
]}.
endef

# To prevent autocompletion issues with ZSH, we add "include erlang.mk"
# separately during the actual bootstrap.
ifdef SP
define bs_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = $(SP)

endef
else
define bs_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

endef
endif

define bs_apps_Makefile
PROJECT = $p
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include $(call core_relpath,$(dir $(ERLANG_MK_FILENAME)),$(APPS_DIR)/app)/erlang.mk
endef

define bs_app
-module($p_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	$p_sup:start_link().

stop(_State) ->
	ok.
endef

define bs_relx_config
{release, {$p_release, "1"}, [$p, sasl, runtime_tools]}.
{extended_start_script, true}.
{sys_config, "rel/sys.config"}.
{vm_args, "rel/vm.args"}.
endef

define bs_sys_config
[
].
endef

define bs_vm_args
-name $p@127.0.0.1
-setcookie $p
-heart
endef

# Normal templates.

define tpl_supervisor
-module($(n)).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
endef

define tpl_gen_server
-module($(n)).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
endef

define tpl_module
-module($(n)).
-export([]).
endef

define tpl_cowboy_http
-module($(n)).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_gen_fsm
-module($(n)).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).

%% gen_fsm.
-export([init/1]).
-export([state_name/2]).
-export([handle_event/3]).
-export([state_name/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%% gen_fsm.

init([]) ->
	{ok, state_name, #state{}}.

state_name(_Event, StateData) ->
	{next_state, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

state_name(_Event, _From, StateData) ->
	{reply, ignored, state_name, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
endef

define tpl_gen_statem
-module($(n)).
-behaviour(gen_statem).

%% API.
-export([start_link/0]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([state_name/3]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_statem:start_link(?MODULE, [], []).

%% gen_statem.

callback_mode() ->
	state_functions.

init([]) ->
	{ok, state_name, #state{}}.

state_name(_EventType, _EventData, StateData) ->
	{next_state, state_name, StateData}.

handle_event(_EventType, _EventData, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
endef

define tpl_cowboy_loop
-module($(n)).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{loop, Req, #state{}, 5000, hibernate}.

info(_Info, Req, State) ->
	{loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_cowboy_rest
-module($(n)).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.
endef

define tpl_cowboy_ws
-module($(n)).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, #state{}}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
endef

define tpl_ranch_protocol
-module($(n)).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-type opts() :: [].
-export_type([opts/0]).

-record(state, {
	socket :: inet:socket(),
	transport :: module()
}).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, _Opts) ->
	ok = ranch:accept_ack(Ref),
	loop(#state{socket=Socket, transport=Transport}).

loop(State) ->
	loop(State).
endef

# Plugin-specific targets.

define render_template
	$(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
endef

ifndef WS
ifdef SP
WS = $(subst a,,a $(wordlist 1,$(SP),a a a a a a a a a a a a a a a a a a a a))
else
WS = $(tab)
endif
endif

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(eval n := $(PROJECT)_sup)
	$(call render_template,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
ifdef LEGACY
	$(call render_template,bs_appsrc,src/$(PROJECT).app.src)
endif
	$(call render_template,bs_app,src/$(PROJECT)_app.erl)
	$(call render_template,tpl_supervisor,src/$(PROJECT)_sup.erl)

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(call render_template,bs_Makefile,Makefile)
	$(verbose) echo "include erlang.mk" >> Makefile
	$(verbose) mkdir src/
ifdef LEGACY
	$(call render_template,bs_appsrc_lib,src/$(PROJECT).app.src)
endif

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard rel/),)
	$(error Error: rel/ directory already exists)
endif
	$(eval p := $(PROJECT))
	$(call render_template,bs_relx_config,relx.config)
	$(verbose) mkdir rel/
	$(call render_template,bs_sys_config,rel/sys.config)
	$(call render_template,bs_vm_args,rel/vm.args)

new-app:
ifndef in
	$(error Usage: $(MAKE) new-app in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(eval n := $(in)_sup)
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(call render_template,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(call render_template,bs_appsrc,$(APPS_DIR)/$p/src/$p.app.src)
endif
	$(call render_template,bs_app,$(APPS_DIR)/$p/src/$p_app.erl)
	$(call render_template,tpl_supervisor,$(APPS_DIR)/$p/src/$p_sup.erl)

new-lib:
ifndef in
	$(error Usage: $(MAKE) new-lib in=APP)
endif
ifneq ($(wildcard $(APPS_DIR)/$in),)
	$(error Error: Application $in already exists)
endif
	$(eval p := $(in))
	$(verbose) mkdir -p $(APPS_DIR)/$p/src/
	$(call render_template,bs_apps_Makefile,$(APPS_DIR)/$p/Makefile)
ifdef LEGACY
	$(call render_template,bs_appsrc_lib,$(APPS_DIR)/$p/src/$p.app.src)
endif

new:
ifeq ($(wildcard src/)$(in),)
	$(error Error: src/ directory does not exist)
endif
ifndef t
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
ifndef n
	$(error Usage: $(MAKE) new t=TEMPLATE n=NAME [in=APP])
endif
ifdef in
	$(call render_template,tpl_$(t),$(APPS_DIR)/$(in)/src/$(n).erl)
else
	$(call render_template,tpl_$(t),src/$(n).erl)
endif

list-templates:
	$(verbose) @echo Available templates:
	$(verbose) printf "    %s\n" $(sort $(patsubst tpl_%,%,$(filter tpl_%,$(.VARIABLES))))

# Copyright (c) 2014-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-c_src distclean-c_src-env

# Configuration.

C_SRC_DIR ?= $(CURDIR)/c_src
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk
C_SRC_OUTPUT ?= $(CURDIR)/priv/$(PROJECT)
C_SRC_TYPE ?= shared

# System type and C compiler/flags.

ifeq ($(PLATFORM),msys2)
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?= .exe
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .dll
else
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?=
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .so
endif

ifeq ($(C_SRC_TYPE),shared)
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_SHARED_EXTENSION)
else
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_EXECUTABLE_EXTENSION)
endif

ifeq ($(PLATFORM),msys2)
# We hardcode the compiler used on MSYS2. The default CC=cc does
# not produce working code. The "gcc" MSYS2 package also doesn't.
	CC = /mingw64/bin/gcc
	export CC
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(PLATFORM),darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(PLATFORM),freebsd)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(PLATFORM),linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

ifneq ($(PLATFORM),msys2)
	CFLAGS += -fPIC
	CXXFLAGS += -fPIC
endif

CFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)"
CXXFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)"

LDLIBS += -L"$(ERL_INTERFACE_LIB_DIR)" -lerl_interface -lei

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Targets.

ifeq ($(wildcard $(C_SRC_DIR)),)
else ifneq ($(wildcard $(C_SRC_DIR)/Makefile),)
app:: app-c_src

test-build:: app-c_src

app-c_src:
	$(MAKE) -C $(C_SRC_DIR)

clean::
	$(MAKE) -C $(C_SRC_DIR) clean

else

ifeq ($(SOURCES),)
SOURCES := $(sort $(foreach pat,*.c *.C *.cc *.cpp,$(call core_find,$(C_SRC_DIR)/,$(pat))))
endif
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

app:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

test-build:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

$(C_SRC_OUTPUT_FILE): $(OBJECTS)
	$(verbose) mkdir -p priv/
	$(link_verbose) $(CC) $(OBJECTS) \
		$(LDFLAGS) $(if $(filter $(C_SRC_TYPE),shared),-shared) $(LDLIBS) \
		-o $(C_SRC_OUTPUT_FILE)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:: clean-c_src

clean-c_src:
	$(gen_verbose) rm -f $(C_SRC_OUTPUT_FILE) $(OBJECTS)

endif

ifneq ($(wildcard $(C_SRC_DIR)),)
$(C_SRC_ENV):
	$(verbose) $(ERL) -eval "file:write_file(\"$(call core_native_path,$(C_SRC_ENV))\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt()."

distclean:: distclean-c_src-env

distclean-c_src-env:
	$(gen_verbose) rm -f $(C_SRC_ENV)

-include $(C_SRC_ENV)
endif

# Templates.

define bs_c_nif
#include "erl_nif.h"

static int loads = 0;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	/* Initialize private data. */
	*priv_data = NULL;

	loads++;

	return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	/* Convert the private data to the new version. */
	*priv_data = *old_priv_data;

	loads++;

	return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
	if (loads == 1) {
		/* Destroy the private data. */
	}

	loads--;
}

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	if (enif_is_atom(env, argv[0])) {
		return enif_make_tuple2(env,
			enif_make_atom(env, "hello"),
			argv[0]);
	}

	return enif_make_tuple2(env,
		enif_make_atom(env, "error"),
		enif_make_atom(env, "badarg"));
}

static ErlNifFunc nif_funcs[] = {
	{"hello", 1, hello}
};

ERL_NIF_INIT($n, nif_funcs, load, NULL, upgrade, unload)
endef

define bs_erl_nif
-module($n).

-export([hello/1]).

-on_load(on_load/0).
on_load() ->
	PrivDir = case code:priv_dir(?MODULE) of
		{error, _} ->
			AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

hello(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).
endef

new-nif:
ifneq ($(wildcard $(C_SRC_DIR)/$n.c),)
	$(error Error: $(C_SRC_DIR)/$n.c already exists)
endif
ifneq ($(wildcard src/$n.erl),)
	$(error Error: src/$n.erl already exists)
endif
ifdef in
	$(verbose) $(MAKE) -C $(APPS_DIR)/$(in)/ new-nif n=$n in=
else
	$(verbose) mkdir -p $(C_SRC_DIR) src/
	$(call render_template,bs_c_nif,$(C_SRC_DIR)/$n.c)
	$(call render_template,bs_erl_nif,src/$n.erl)
endif

# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ci ci-prepare ci-setup

CI_OTP ?=
CI_HIPE ?=
CI_ERLLVM ?=

ifeq ($(CI_VM),native)
ERLC_OPTS += +native
TEST_ERLC_OPTS += +native
else ifeq ($(CI_VM),erllvm)
ERLC_OPTS += +native +'{hipe, [to_llvm]}'
TEST_ERLC_OPTS += +native +'{hipe, [to_llvm]}'
endif

ifeq ($(strip $(CI_OTP) $(CI_HIPE) $(CI_ERLLVM)),)
ci::
else

ci:: $(addprefix ci-,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)) $(addsuffix -erllvm,$(CI_ERLLVM)))

ci-prepare: $(addprefix $(KERL_INSTALL_DIR)/,$(CI_OTP) $(addsuffix -native,$(CI_HIPE)))

ci-setup::

ci-extra::

ci_verbose_0 = @echo " CI    " $(1);
ci_verbose = $(ci_verbose_$(V))

define ci_target
ci-$1: $(KERL_INSTALL_DIR)/$2
	$(verbose) $(MAKE) --no-print-directory clean
	$(ci_verbose) \
		PATH="$(KERL_INSTALL_DIR)/$2/bin:$(PATH)" \
		CI_OTP_RELEASE="$1" \
		CT_OPTS="-label $1" \
		CI_VM="$3" \
		$(MAKE) ci-setup tests
	$(verbose) $(MAKE) --no-print-directory ci-extra
endef

$(foreach otp,$(CI_OTP),$(eval $(call ci_target,$(otp),$(otp),otp)))
$(foreach otp,$(CI_HIPE),$(eval $(call ci_target,$(otp)-native,$(otp)-native,native)))
$(foreach otp,$(CI_ERLLVM),$(eval $(call ci_target,$(otp)-erllvm,$(otp)-native,erllvm)))

$(foreach otp,$(CI_OTP),$(eval $(call kerl_otp_target,$(otp))))
$(foreach otp,$(sort $(CI_HIPE) $(CI_ERLLLVM)),$(eval $(call kerl_hipe_target,$(otp))))

help::
	$(verbose) printf "%s\n" "" \
		"Continuous Integration targets:" \
		"  ci          Run '$(MAKE) tests' on all configured Erlang versions." \
		"" \
		"The CI_OTP variable must be defined with the Erlang versions" \
		"that must be tested. For example: CI_OTP = OTP-17.3.4 OTP-17.5.3"

endif

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: ct apps-ct distclean-ct

# Configuration.

CT_OPTS ?=

ifneq ($(wildcard $(TEST_DIR)),)
ifndef CT_SUITES
CT_SUITES := $(sort $(subst _SUITE.erl,,$(notdir $(call core_find,$(TEST_DIR)/,*_SUITE.erl))))
endif
endif
CT_SUITES ?=
CT_LOGS_DIR ?= $(CURDIR)/logs

# Core targets.

tests:: ct

distclean:: distclean-ct

help::
	$(verbose) printf "%s\n" "" \
		"Common_test targets:" \
		"  ct          Run all the common_test suites for this project" \
		"" \
		"All your common_test suites have their associated targets." \
		"A suite named http_SUITE can be ran using the ct-http target."

# Plugin-specific targets.

CT_RUN = ct_run \
	-no_auto_compile \
	-noinput \
	-pa $(CURDIR)/ebin $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin $(TEST_DIR) \
	-dir $(TEST_DIR) \
	-logdir $(CT_LOGS_DIR)

ifeq ($(CT_SUITES),)
ct: $(if $(IS_APP),,apps-ct)
else
# We do not run tests if we are in an apps/* with no test directory.
ifneq ($(IS_APP)$(wildcard $(TEST_DIR)),1)
ct: test-build $(if $(IS_APP),,apps-ct)
	$(verbose) mkdir -p $(CT_LOGS_DIR)
	$(gen_verbose) $(CT_RUN) -sname ct_$(PROJECT) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)
endif
endif

ifneq ($(ALL_APPS_DIRS),)
define ct_app_target
apps-ct-$1:
	$(MAKE) -C $1 ct IS_APP=1
endef

$(foreach app,$(ALL_APPS_DIRS),$(eval $(call ct_app_target,$(app))))

apps-ct: test-build $(addprefix apps-ct-,$(ALL_APPS_DIRS))
endif

ifndef t
CT_EXTRA =
else
ifeq (,$(findstring :,$t))
CT_EXTRA = -group $t
else
t_words = $(subst :, ,$t)
CT_EXTRA = -group $(firstword $(t_words)) -case $(lastword $(t_words))
endif
endif

define ct_suite_target
ct-$(1): test-build
	$(verbose) mkdir -p $(CT_LOGS_DIR)
	$(gen_verbose) $(CT_RUN) -sname ct_$(PROJECT) -suite $(addsuffix _SUITE,$(1)) $(CT_EXTRA) $(CT_OPTS)
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

distclean-ct:
	$(gen_verbose) rm -rf $(CT_LOGS_DIR)

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: plt distclean-plt dialyze

# Configuration.

DIALYZER_PLT ?= $(CURDIR)/.$(PROJECT).plt
export DIALYZER_PLT

PLT_APPS ?=
DIALYZER_DIRS ?= --src -r $(wildcard src) $(ALL_APPS_DIRS)
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
DIALYZER_PLT_OPTS ?=

# Core targets.

check:: dialyze

distclean:: distclean-plt

help::
	$(verbose) printf "%s\n" "" \
		"Dialyzer targets:" \
		"  plt         Build a PLT file for this project" \
		"  dialyze     Analyze the project using Dialyzer"

# Plugin-specific targets.

define filter_opts.erl
	Opts = init:get_plain_arguments(),
	{Filtered, _} = lists:foldl(fun
		(O,                         {Os, true}) -> {[O|Os], false};
		(O = "-D",                  {Os, _})    -> {[O|Os], true};
		(O = [\\$$-, \\$$D, _ | _], {Os, _})    -> {[O|Os], false};
		(O = "-I",                  {Os, _})    -> {[O|Os], true};
		(O = [\\$$-, \\$$I, _ | _], {Os, _})    -> {[O|Os], false};
		(O = "-pa",                 {Os, _})    -> {[O|Os], true};
		(_,                         Acc)        -> Acc
	end, {[], false}, Opts),
	io:format("~s~n", [string:join(lists:reverse(Filtered), " ")]),
	halt().
endef

$(DIALYZER_PLT): deps app
	$(eval DEPS_LOG := $(shell test -f $(ERLANG_MK_TMP)/deps.log && \
		while read p; do test -d $$p/ebin && echo $$p/ebin; done <$(ERLANG_MK_TMP)/deps.log))
	$(verbose) dialyzer --build_plt $(DIALYZER_PLT_OPTS) --apps \
		erts kernel stdlib $(PLT_APPS) $(OTP_DEPS) $(LOCAL_DEPS) $(DEPS_LOG)

plt: $(DIALYZER_PLT)

distclean-plt:
	$(gen_verbose) rm -f $(DIALYZER_PLT)

ifneq ($(wildcard $(DIALYZER_PLT)),)
dialyze:
else
dialyze: $(DIALYZER_PLT)
endif
	$(verbose) dialyzer --no_native `$(ERL) -eval "$(subst $(newline),,$(call escape_dquotes,$(call filter_opts.erl)))" -extra $(ERLC_OPTS)` $(DIALYZER_DIRS) $(DIALYZER_OPTS)

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc edoc

# Configuration.

EDOC_OPTS ?=
EDOC_SRC_DIRS ?=
EDOC_OUTPUT ?= doc

define edoc.erl
	SrcPaths = lists:foldl(fun(P, Acc) ->
		filelib:wildcard(atom_to_list(P) ++ "/{src,c_src}") ++ Acc
	end, [], [$(call comma_list,$(patsubst %,'%',$(EDOC_SRC_DIRS)))]),
	DefaultOpts = [{dir, "$(EDOC_OUTPUT)"}, {source_path, SrcPaths}, {subpackages, false}],
	edoc:application($(1), ".", [$(2)] ++ DefaultOpts),
	halt(0).
endef

# Core targets.

ifneq ($(strip $(EDOC_SRC_DIRS)$(wildcard doc/overview.edoc)),)
docs:: edoc
endif

distclean:: distclean-edoc

# Plugin-specific targets.

edoc: distclean-edoc doc-deps
	$(gen_verbose) $(call erlang,$(call edoc.erl,$(PROJECT),$(EDOC_OPTS)))

distclean-edoc:
	$(gen_verbose) rm -f $(EDOC_OUTPUT)/*.css $(EDOC_OUTPUT)/*.html $(EDOC_OUTPUT)/*.png $(EDOC_OUTPUT)/edoc-info

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Configuration.

DTL_FULL_PATH ?=
DTL_PATH ?= templates/
DTL_SUFFIX ?= _dtl
DTL_OPTS ?=

# Verbosity.

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

# Core targets.

DTL_PATH := $(abspath $(DTL_PATH))
DTL_FILES := $(sort $(call core_find,$(DTL_PATH),*.dtl))

ifneq ($(DTL_FILES),)

DTL_NAMES   = $(addsuffix $(DTL_SUFFIX),$(DTL_FILES:$(DTL_PATH)/%.dtl=%))
DTL_MODULES = $(if $(DTL_FULL_PATH),$(subst /,_,$(DTL_NAMES)),$(notdir $(DTL_NAMES)))
BEAM_FILES += $(addsuffix .beam,$(addprefix ebin/,$(DTL_MODULES)))

ifneq ($(words $(DTL_FILES)),0)
# Rebuild templates when the Makefile changes.
$(ERLANG_MK_TMP)/last-makefile-change-erlydtl: $(MAKEFILE_LIST)
	@mkdir -p $(ERLANG_MK_TMP)
	@if test -f $@; then \
		touch $(DTL_FILES); \
	fi
	@touch $@

ebin/$(PROJECT).app:: $(ERLANG_MK_TMP)/last-makefile-change-erlydtl
endif

define erlydtl_compile.erl
	[begin
		Module0 = case "$(strip $(DTL_FULL_PATH))" of
			"" ->
				filename:basename(F, ".dtl");
			_ ->
				"$(DTL_PATH)/" ++ F2 = filename:rootname(F, ".dtl"),
				re:replace(F2, "/",  "_",  [{return, list}, global])
		end,
		Module = list_to_atom(string:to_lower(Module0) ++ "$(DTL_SUFFIX)"),
		case erlydtl:compile(F, Module, [$(DTL_OPTS)] ++ [{out_dir, "ebin/"}, return_errors]) of
			ok -> ok;
			{ok, _} -> ok
		end
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ebin/$(PROJECT).app:: $(DTL_FILES) | ebin/
	$(if $(strip $?),\
		$(dtl_verbose) $(call erlang,$(call erlydtl_compile.erl,$(call core_native_path,$?)),\
			-pa ebin/ $(DEPS_DIR)/erlydtl/ebin/))

endif

# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, Dave Cottlehuber <dch@skunkwerks.at>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-escript escript escript-zip

# Configuration.

ESCRIPT_NAME ?= $(PROJECT)
ESCRIPT_FILE ?= $(ESCRIPT_NAME)

ESCRIPT_SHEBANG ?= /usr/bin/env escript
ESCRIPT_COMMENT ?= This is an -*- erlang -*- file
ESCRIPT_EMU_ARGS ?= -escript main $(ESCRIPT_NAME)

ESCRIPT_ZIP ?= 7z a -tzip -mx=9 -mtc=off $(if $(filter-out 0,$(V)),,> /dev/null)
ESCRIPT_ZIP_FILE ?= $(ERLANG_MK_TMP)/escript.zip

# Core targets.

distclean:: distclean-escript

help::
	$(verbose) printf "%s\n" "" \
		"Escript targets:" \
		"  escript     Build an executable escript archive" \

# Plugin-specific targets.

escript-zip:: deps app
	$(verbose) mkdir -p $(dir $(ESCRIPT_ZIP))
	$(verbose) rm -f $(ESCRIPT_ZIP_FILE)
	$(gen_verbose) cd .. && $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) $(PROJECT)/ebin/*
ifneq ($(DEPS),)
	$(verbose) cd $(DEPS_DIR) && $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) \
		`cat $(ERLANG_MK_TMP)/deps.log | sed 's/^$(subst /,\/,$(DEPS_DIR))\///' | sed 's/$$/\/ebin\/\*/'`
endif

escript:: escript-zip
	$(gen_verbose) printf "%s\n" \
		"#!$(ESCRIPT_SHEBANG)" \
		"%% $(ESCRIPT_COMMENT)" \
		"%%! $(ESCRIPT_EMU_ARGS)" > $(ESCRIPT_FILE)
	$(verbose) cat $(ESCRIPT_ZIP_FILE) >> $(ESCRIPT_FILE)
	$(verbose) chmod +x $(ESCRIPT_FILE)

distclean-escript:
	$(gen_verbose) rm -f $(ESCRIPT_FILE)

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, Enrique Fernandez <enrique.fernandez@erlang-solutions.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: eunit apps-eunit

# Configuration

EUNIT_OPTS ?=
EUNIT_ERL_OPTS ?=

# Core targets.

tests:: eunit

help::
	$(verbose) printf "%s\n" "" \
		"EUnit targets:" \
		"  eunit       Run all the EUnit tests for this project"

# Plugin-specific targets.

define eunit.erl
	Enabled = case "$(COVER)" of
		"" -> false;
		_ ->
			case filelib:is_dir("ebin") of
				false -> false;
				true ->
					case cover:compile_beam_directory("ebin") of
						{error, _} -> halt(1);
						_ -> true
					end
			end
	end,
	case eunit:test($1, [$(EUNIT_OPTS)]) of
		ok -> ok;
		error -> halt(2)
	end,
	case {Enabled, "$(COVER)"} of
		{false, _} -> ok;
		{_, ""} -> ok;
		_ ->
			cover:export("$(COVER_DATA_DIR)/eunit.coverdata")
	end,
	halt()
endef

EUNIT_ERL_OPTS += -pa $(TEST_DIR) $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin $(CURDIR)/ebin

ifdef t
ifeq (,$(findstring :,$(t)))
eunit: test-build cover-data-dir
	$(gen_verbose) $(call erlang,$(call eunit.erl,['$(t)']),$(EUNIT_ERL_OPTS))
else
eunit: test-build cover-data-dir
	$(gen_verbose) $(call erlang,$(call eunit.erl,fun $(t)/0),$(EUNIT_ERL_OPTS))
endif
else
EUNIT_EBIN_MODS = $(notdir $(basename $(ERL_FILES) $(BEAM_FILES)))
EUNIT_TEST_MODS = $(notdir $(basename $(call core_find,$(TEST_DIR)/,*.erl)))

EUNIT_MODS = $(foreach mod,$(EUNIT_EBIN_MODS) $(filter-out \
	$(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(EUNIT_TEST_MODS)),'$(mod)')

eunit: test-build $(if $(IS_APP),,apps-eunit) cover-data-dir
	$(gen_verbose) $(call erlang,$(call eunit.erl,[$(call comma_list,$(EUNIT_MODS))]),$(EUNIT_ERL_OPTS))

ifneq ($(ALL_APPS_DIRS),)
apps-eunit:
	$(verbose) eunit_retcode=0 ; for app in $(ALL_APPS_DIRS); do $(MAKE) -C $$app eunit IS_APP=1; \
		[ $$? -ne 0 ] && eunit_retcode=1 ; done ; \
		exit $$eunit_retcode
endif
endif

# Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter proper,$(DEPS) $(TEST_DEPS)),proper)
.PHONY: proper

# Targets.

tests:: proper

define proper_check.erl
	code:add_pathsa([
		"$(call core_native_path,$(CURDIR)/ebin)",
		"$(call core_native_path,$(DEPS_DIR)/*/ebin)",
		"$(call core_native_path,$(TEST_DIR))"]),
	Module = fun(M) ->
		[true] =:= lists:usort([
			case atom_to_list(F) of
				"prop_" ++ _ ->
					io:format("Testing ~p:~p/0~n", [M, F]),
					proper:quickcheck(M:F(), nocolors);
				_ ->
					true
			end
		|| {F, 0} <- M:module_info(exports)])
	end,
	try
		case $(1) of
			all -> [true] =:= lists:usort([Module(M) || M <- [$(call comma_list,$(3))]]);
			module -> Module($(2));
			function -> proper:quickcheck($(2), nocolors)
		end
	of
		true -> halt(0);
		_ -> halt(1)
	catch error:undef ->
		io:format("Undefined property or module?~n~p~n", [erlang:get_stacktrace()]),
		halt(0)
	end.
endef

ifdef t
ifeq (,$(findstring :,$(t)))
proper: test-build
	$(verbose) $(call erlang,$(call proper_check.erl,module,$(t)))
else
proper: test-build
	$(verbose) echo Testing $(t)/0
	$(verbose) $(call erlang,$(call proper_check.erl,function,$(t)()))
endif
else
proper: test-build
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(wildcard ebin/*.beam) $(call core_find,$(TEST_DIR)/,*.beam))))))
	$(gen_verbose) $(call erlang,$(call proper_check.erl,all,undefined,$(MODULES)))
endif
endif

# Copyright (c) 2013-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel relx-relup distclean-relx-rel run

# Configuration.

RELX ?= $(ERLANG_MK_TMP)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config

RELX_URL ?= https://erlang.mk/res/relx-v3.24.5
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel
RELX_REL_EXT ?=
RELX_TAR ?= 1

ifdef SFX
	RELX_TAR = 1
endif

ifeq ($(firstword $(RELX_OPTS)),-o)
	RELX_OUTPUT_DIR = $(word 2,$(RELX_OPTS))
else
	RELX_OPTS += -o $(RELX_OUTPUT_DIR)
endif

# Core targets.

ifeq ($(IS_DEP),)
ifneq ($(wildcard $(RELX_CONFIG)),)
rel:: relx-rel

relup:: relx-relup
endif
endif

distclean:: distclean-relx-rel

# Plugin-specific targets.

$(RELX):
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) $(call core_http_get,$(RELX),$(RELX_URL))
	$(verbose) chmod +x $(RELX)

relx-rel: $(RELX) rel-deps app
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release $(if $(filter 1,$(RELX_TAR)),tar)

relx-relup: $(RELX) rel-deps app
	$(verbose) $(RELX) -c $(RELX_CONFIG) $(RELX_OPTS) release relup $(if $(filter 1,$(RELX_TAR)),tar)

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

# Run target.

ifeq ($(wildcard $(RELX_CONFIG)),)
run::
else

define get_relx_release.erl
	{ok, Config} = file:consult("$(call core_native_path,$(RELX_CONFIG))"),
	{release, {Name, Vsn0}, _} = lists:keyfind(release, 1, Config),
	Vsn = case Vsn0 of
		{cmd, Cmd} -> os:cmd(Cmd);
		semver -> "";
		{semver, _} -> "";
		VsnStr -> Vsn0
	end,
	io:format("~s ~s", [Name, Vsn]),
	halt(0).
endef

RELX_REL := $(shell $(call erlang,$(get_relx_release.erl)))
RELX_REL_NAME := $(word 1,$(RELX_REL))
RELX_REL_VSN := $(word 2,$(RELX_REL))

ifeq ($(PLATFORM),msys2)
RELX_REL_EXT := .cmd
endif

run:: all
	$(verbose) $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/bin/$(RELX_REL_NAME)$(RELX_REL_EXT) console

help::
	$(verbose) printf "%s\n" "" \
		"Relx targets:" \
		"  run         Compile the project, build the release and run it"

endif

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, M Robert Martin <rob@version2beta.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: shell

# Configuration.

SHELL_ERL ?= erl
SHELL_PATHS ?= $(CURDIR)/ebin $(APPS_DIR)/*/ebin $(DEPS_DIR)/*/ebin $(TEST_DIR)
SHELL_OPTS ?=

ALL_SHELL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(SHELL_DEPS))

# Core targets

help::
	$(verbose) printf "%s\n" "" \
		"Shell targets:" \
		"  shell       Run an erlang shell with SHELL_OPTS or reasonable default"

# Plugin-specific targets.

$(foreach dep,$(SHELL_DEPS),$(eval $(call dep_target,$(dep))))

build-shell-deps: $(ALL_SHELL_DEPS_DIRS)
	$(verbose) set -e; for dep in $(ALL_SHELL_DEPS_DIRS) ; do $(MAKE) -C $$dep ; done

shell: build-shell-deps
	$(gen_verbose) $(SHELL_ERL) -pa $(SHELL_PATHS) $(SHELL_OPTS)

# Copyright 2017, Stanislaw Klekot <dozzie@jarowit.net>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-sphinx sphinx

# Configuration.

SPHINX_BUILD ?= sphinx-build
SPHINX_SOURCE ?= doc
SPHINX_CONFDIR ?=
SPHINX_FORMATS ?= html
SPHINX_DOCTREES ?= $(ERLANG_MK_TMP)/sphinx.doctrees
SPHINX_OPTS ?=

#sphinx_html_opts =
#sphinx_html_output = html
#sphinx_man_opts =
#sphinx_man_output = man
#sphinx_latex_opts =
#sphinx_latex_output = latex

# Helpers.

sphinx_build_0 = @echo " SPHINX" $1; $(SPHINX_BUILD) -N -q
sphinx_build_1 = $(SPHINX_BUILD) -N
sphinx_build_2 = set -x; $(SPHINX_BUILD)
sphinx_build = $(sphinx_build_$(V))

define sphinx.build
$(call sphinx_build,$1) -b $1 -d $(SPHINX_DOCTREES) $(if $(SPHINX_CONFDIR),-c $(SPHINX_CONFDIR)) $(SPHINX_OPTS) $(sphinx_$1_opts) -- $(SPHINX_SOURCE) $(call sphinx.output,$1)

endef

define sphinx.output
$(if $(sphinx_$1_output),$(sphinx_$1_output),$1)
endef

# Targets.

ifneq ($(wildcard $(if $(SPHINX_CONFDIR),$(SPHINX_CONFDIR),$(SPHINX_SOURCE))/conf.py),)
docs:: sphinx
distclean:: distclean-sphinx
endif

help::
	$(verbose) printf "%s\n" "" \
		"Sphinx targets:" \
		"  sphinx      Generate Sphinx documentation." \
		"" \
		"ReST sources and 'conf.py' file are expected in directory pointed by" \
		"SPHINX_SOURCE ('doc' by default). SPHINX_FORMATS lists formats to build (only" \
		"'html' format is generated by default); target directory can be specified by" \
		'setting sphinx_$${format}_output, for example: sphinx_html_output = output/html' \
		"Additional Sphinx options can be set in SPHINX_OPTS."

# Plugin-specific targets.

sphinx:
	$(foreach F,$(SPHINX_FORMATS),$(call sphinx.build,$F))

distclean-sphinx:
	$(gen_verbose) rm -rf $(filter-out $(SPHINX_SOURCE),$(foreach F,$(SPHINX_FORMATS),$(call sphinx.output,$F)))

# Copyright (c) 2017, Jean-Sébastien Pédron <jean-sebastien@rabbitmq.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: show-ERL_LIBS show-ERLC_OPTS show-TEST_ERLC_OPTS

show-ERL_LIBS:
	@echo $(ERL_LIBS)

show-ERLC_OPTS:
	@$(foreach opt,$(ERLC_OPTS) -pa ebin -I include,echo "$(opt)";)

show-TEST_ERLC_OPTS:
	@$(foreach opt,$(TEST_ERLC_OPTS) -pa ebin -I include,echo "$(opt)";)

# Copyright (c) 2015-2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

ifeq ($(filter triq,$(DEPS) $(TEST_DEPS)),triq)
.PHONY: triq

# Targets.

tests:: triq

define triq_check.erl
	code:add_pathsa([
		"$(call core_native_path,$(CURDIR)/ebin)",
		"$(call core_native_path,$(DEPS_DIR)/*/ebin)",
		"$(call core_native_path,$(TEST_DIR))"]),
	try
		case $(1) of
			all -> [true] =:= lists:usort([triq:check(M) || M <- [$(call comma_list,$(3))]]);
			module -> triq:check($(2));
			function -> triq:check($(2))
		end
	of
		true -> halt(0);
		_ -> halt(1)
	catch error:undef ->
		io:format("Undefined property or module?~n~p~n", [erlang:get_stacktrace()]),
		halt(0)
	end.
endef

ifdef t
ifeq (,$(findstring :,$(t)))
triq: test-build
	$(verbose) $(call erlang,$(call triq_check.erl,module,$(t)))
else
triq: test-build
	$(verbose) echo Testing $(t)/0
	$(verbose) $(call erlang,$(call triq_check.erl,function,$(t)()))
endif
else
triq: test-build
	$(eval MODULES := $(patsubst %,'%',$(sort $(notdir $(basename \
		$(wildcard ebin/*.beam) $(call core_find,$(TEST_DIR)/,*.beam))))))
	$(gen_verbose) $(call erlang,$(call triq_check.erl,all,undefined,$(MODULES)))
endif
endif

# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015, Erlang Solutions Ltd.
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: xref distclean-xref

# Configuration.

ifeq ($(XREF_CONFIG),)
	XREFR_ARGS :=
else
	XREFR_ARGS := -c $(XREF_CONFIG)
endif

XREFR ?= $(CURDIR)/xrefr
export XREFR

XREFR_URL ?= https://github.com/inaka/xref_runner/releases/download/1.1.0/xrefr

# Core targets.

help::
	$(verbose) printf '%s\n' '' \
		'Xref targets:' \
		'  xref        Run Xrefr using $$XREF_CONFIG as config file if defined'

distclean:: distclean-xref

# Plugin-specific targets.

$(XREFR):
	$(gen_verbose) $(call core_http_get,$(XREFR),$(XREFR_URL))
	$(verbose) chmod +x $(XREFR)

xref: deps app $(XREFR)
	$(gen_verbose) $(XREFR) $(XREFR_ARGS)

distclean-xref:
	$(gen_verbose) rm -rf $(XREFR)

# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015, Viktor Söderqvist <viktor@zuiderkwast.se>
# This file is part of erlang.mk and subject to the terms of the ISC License.

COVER_REPORT_DIR ?= cover
COVER_DATA_DIR ?= $(CURDIR)

# Hook in coverage to ct

ifdef COVER
ifdef CT_RUN
ifneq ($(wildcard $(TEST_DIR)),)
test-build:: $(TEST_DIR)/ct.cover.spec

$(TEST_DIR)/ct.cover.spec: cover-data-dir
	$(gen_verbose) printf "%s\n" \
		"{incl_app, '$(PROJECT)', details}." \
		'{export,"$(abspath $(COVER_DATA_DIR))/ct.coverdata"}.' > $@

CT_RUN += -cover $(TEST_DIR)/ct.cover.spec
endif
endif
endif

# Core targets

ifdef COVER
ifneq ($(COVER_REPORT_DIR),)
tests::
	$(verbose) $(MAKE) --no-print-directory cover-report
endif

cover-data-dir: | $(COVER_DATA_DIR)

$(COVER_DATA_DIR):
	$(verbose) mkdir -p $(COVER_DATA_DIR)
else
cover-data-dir:
endif

clean:: coverdata-clean

ifneq ($(COVER_REPORT_DIR),)
distclean:: cover-report-clean
endif

help::
	$(verbose) printf "%s\n" "" \
		"Cover targets:" \
		"  cover-report  Generate a HTML coverage report from previously collected" \
		"                cover data." \
		"  all.coverdata Merge all coverdata files into all.coverdata." \
		"" \
		"If COVER=1 is set, coverage data is generated by the targets eunit and ct. The" \
		"target tests additionally generates a HTML coverage report from the combined" \
		"coverdata files from each of these testing tools. HTML reports can be disabled" \
		"by setting COVER_REPORT_DIR to empty."

# Plugin specific targets

COVERDATA = $(filter-out $(COVER_DATA_DIR)/all.coverdata,$(wildcard $(COVER_DATA_DIR)/*.coverdata))

.PHONY: coverdata-clean
coverdata-clean:
	$(gen_verbose) rm -f $(COVER_DATA_DIR)/*.coverdata $(TEST_DIR)/ct.cover.spec

# Merge all coverdata files into one.
define cover_export.erl
	$(foreach f,$(COVERDATA),cover:import("$(f)") == ok orelse halt(1),)
	cover:export("$(COVER_DATA_DIR)/$@"), halt(0).
endef

all.coverdata: $(COVERDATA) cover-data-dir
	$(gen_verbose) $(call erlang,$(cover_export.erl))

# These are only defined if COVER_REPORT_DIR is non-empty. Set COVER_REPORT_DIR to
# empty if you want the coverdata files but not the HTML report.
ifneq ($(COVER_REPORT_DIR),)

.PHONY: cover-report-clean cover-report

cover-report-clean:
	$(gen_verbose) rm -rf $(COVER_REPORT_DIR)
	$(if $(shell ls -A $(COVER_DATA_DIR)/),,$(verbose) rmdir $(COVER_DATA_DIR))

ifeq ($(COVERDATA),)
cover-report:
else

# Modules which include eunit.hrl always contain one line without coverage
# because eunit defines test/0 which is never called. We compensate for this.
EUNIT_HRL_MODS = $(subst $(space),$(comma),$(shell \
	grep -H -e '^\s*-include.*include/eunit\.hrl"' src/*.erl \
	| sed "s/^src\/\(.*\)\.erl:.*/'\1'/" | uniq))

define cover_report.erl
	$(foreach f,$(COVERDATA),cover:import("$(f)") == ok orelse halt(1),)
	Ms = cover:imported_modules(),
	[cover:analyse_to_file(M, "$(COVER_REPORT_DIR)/" ++ atom_to_list(M)
		++ ".COVER.html", [html])  || M <- Ms],
	Report = [begin {ok, R} = cover:analyse(M, module), R end || M <- Ms],
	EunitHrlMods = [$(EUNIT_HRL_MODS)],
	Report1 = [{M, {Y, case lists:member(M, EunitHrlMods) of
		true -> N - 1; false -> N end}} || {M, {Y, N}} <- Report],
	TotalY = lists:sum([Y || {_, {Y, _}} <- Report1]),
	TotalN = lists:sum([N || {_, {_, N}} <- Report1]),
	Perc = fun(Y, N) -> case Y + N of 0 -> 100; S -> round(100 * Y / S) end end,
	TotalPerc = Perc(TotalY, TotalN),
	{ok, F} = file:open("$(COVER_REPORT_DIR)/index.html", [write]),
	io:format(F, "<!DOCTYPE html><html>~n"
		"<head><meta charset=\"UTF-8\">~n"
		"<title>Coverage report</title></head>~n"
		"<body>~n", []),
	io:format(F, "<h1>Coverage</h1>~n<p>Total: ~p%</p>~n", [TotalPerc]),
	io:format(F, "<table><tr><th>Module</th><th>Coverage</th></tr>~n", []),
	[io:format(F, "<tr><td><a href=\"~p.COVER.html\">~p</a></td>"
		"<td>~p%</td></tr>~n",
		[M, M, Perc(Y, N)]) || {M, {Y, N}} <- Report1],
	How = "$(subst $(space),$(comma)$(space),$(basename $(COVERDATA)))",
	Date = "$(shell date -u "+%Y-%m-%dT%H:%M:%SZ")",
	io:format(F, "</table>~n"
		"<p>Generated using ~s and erlang.mk on ~s.</p>~n"
		"</body></html>", [How, Date]),
	halt().
endef

cover-report:
	$(verbose) mkdir -p $(COVER_REPORT_DIR)
	$(gen_verbose) $(call erlang,$(cover_report.erl))

endif
endif # ifneq ($(COVER_REPORT_DIR),)

# Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: sfx

ifdef RELX_REL
ifdef SFX

# Configuration.

SFX_ARCHIVE ?= $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/$(RELX_REL_NAME)-$(RELX_REL_VSN).tar.gz
SFX_OUTPUT_FILE ?= $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME).run

# Core targets.

rel:: sfx

# Plugin-specific targets.

define sfx_stub
#!/bin/sh

TMPDIR=`mktemp -d`
ARCHIVE=`awk '/^__ARCHIVE_BELOW__$$/ {print NR + 1; exit 0;}' $$0`
FILENAME=$$(basename $$0)
REL=$${FILENAME%.*}

tail -n+$$ARCHIVE $$0 | tar -xzf - -C $$TMPDIR

$$TMPDIR/bin/$$REL console
RET=$$?

rm -rf $$TMPDIR

exit $$RET

__ARCHIVE_BELOW__
endef

sfx:
	$(call render_template,sfx_stub,$(SFX_OUTPUT_FILE))
	$(gen_verbose) cat $(SFX_ARCHIVE) >> $(SFX_OUTPUT_FILE)
	$(verbose) chmod +x $(SFX_OUTPUT_FILE)

endif
endif

# Copyright (c) 2013-2017, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# External plugins.

DEP_PLUGINS ?=

$(foreach p,$(DEP_PLUGINS),\
	$(eval $(if $(findstring /,$p),\
		$(call core_dep_plugin,$p,$(firstword $(subst /, ,$p))),\
		$(call core_dep_plugin,$p/plugins.mk,$p))))

# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2015-2016, Jean-Sébastien Pédron <jean-sebastien@rabbitmq.com>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Fetch dependencies recursively (without building them).

.PHONY: fetch-deps fetch-doc-deps fetch-rel-deps fetch-test-deps \
	fetch-shell-deps

.PHONY: $(ERLANG_MK_RECURSIVE_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
	$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

fetch-deps: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
fetch-doc-deps: $(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST)
fetch-rel-deps: $(ERLANG_MK_RECURSIVE_REL_DEPS_LIST)
fetch-test-deps: $(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST)
fetch-shell-deps: $(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

ifneq ($(SKIP_DEPS),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST):
	$(verbose) :> $@
else
# By default, we fetch "normal" dependencies. They are also included no
# matter the type of requested dependencies.
#
# $(ALL_DEPS_DIRS) includes $(BUILD_DEPS).

$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_DOC_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_REL_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_TEST_DEPS_DIRS)
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST): $(ALL_DEPS_DIRS) $(ALL_SHELL_DEPS_DIRS)

# Allow to use fetch-deps and $(DEP_TYPES) to fetch multiple types of
# dependencies with a single target.
ifneq ($(filter doc,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_DOC_DEPS_DIRS)
endif
ifneq ($(filter rel,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_REL_DEPS_DIRS)
endif
ifneq ($(filter test,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_TEST_DEPS_DIRS)
endif
ifneq ($(filter shell,$(DEP_TYPES)),)
$(ERLANG_MK_RECURSIVE_DEPS_LIST): $(ALL_SHELL_DEPS_DIRS)
endif

ERLANG_MK_RECURSIVE_TMP_LIST := $(abspath $(ERLANG_MK_TMP)/recursive-tmp-deps.log)

$(ERLANG_MK_RECURSIVE_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_REL_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST) \
$(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST):
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(verbose) rm -f $(ERLANG_MK_RECURSIVE_TMP_LIST)
endif
ifndef IS_APP
	$(verbose) set -e; for dep in $(ALL_APPS_DIRS) ; do \
		$(MAKE) -C $$dep $@ \
		 IS_APP=1 \
		 ERLANG_MK_RECURSIVE_TMP_LIST=$(ERLANG_MK_RECURSIVE_TMP_LIST); \
	done
endif
	$(verbose) set -e; for dep in $^ ; do \
		if ! grep -qs ^$$dep$$ $(ERLANG_MK_RECURSIVE_TMP_LIST); then \
			echo $$dep >> $(ERLANG_MK_RECURSIVE_TMP_LIST); \
			if grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk|.*ERLANG_MK_FILENAME.*)$$" \
			 $$dep/GNUmakefile $$dep/makefile $$dep/Makefile; then \
				$(MAKE) -C $$dep fetch-deps \
				 IS_DEP=1 \
				 ERLANG_MK_RECURSIVE_TMP_LIST=$(ERLANG_MK_RECURSIVE_TMP_LIST); \
			fi \
		fi \
	done
ifeq ($(IS_APP)$(IS_DEP),)
	$(verbose) sort < $(ERLANG_MK_RECURSIVE_TMP_LIST) | uniq > $@
	$(verbose) rm $(ERLANG_MK_RECURSIVE_TMP_LIST)
endif
endif # ifneq ($(SKIP_DEPS),)

# List dependencies recursively.

.PHONY: list-deps list-doc-deps list-rel-deps list-test-deps \
	list-shell-deps

list-deps: $(ERLANG_MK_RECURSIVE_DEPS_LIST)
list-doc-deps: $(ERLANG_MK_RECURSIVE_DOC_DEPS_LIST)
list-rel-deps: $(ERLANG_MK_RECURSIVE_REL_DEPS_LIST)
list-test-deps: $(ERLANG_MK_RECURSIVE_TEST_DEPS_LIST)
list-shell-deps: $(ERLANG_MK_RECURSIVE_SHELL_DEPS_LIST)

list-deps list-doc-deps list-rel-deps list-test-deps list-shell-deps:
	$(verbose) cat $^
