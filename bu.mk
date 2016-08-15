# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2016, Grégoire Lejeune
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

.PHONY: doc

all: compile-erl

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

# Utils

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

# Common

CP = cp
CP_R = cp -r
RM = rm
RM_RF = rm -rf
RM_F = rm -f
MKDIR_P = mkdir -p

# Config

ifneq ("$(wildcard config/$(current_dir).config)","")
  ERL_CONFIG="-config config/$(current_dir).config"
else
  ERL_CONFIG=
endif

# Core functions.

empty :=
space := $(empty) $(empty)
tab := $(empty) $(empty)
comma := ,

define newline


endef

# Template

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

# rebar3

FIND_REBAR = \
                REBAR_BIN=; \
                for x in ./rebar3 rebar3; do \
                if type "$${x%% *}" >/dev/null 2>/dev/null; then REBAR_BIN=$$x; break; fi; \
                done; \
                if [ -z "$$REBAR_BIN" ]; then echo 1>&2 "Unable to find rebar3"; exit 2; fi
REBAR = $(FIND_REBAR); $$REBAR_BIN

# mix

MIX = mix

# Default tasks
ifeq ($(HAS_ELIXIR), 1)

compile-ex: elixir clean
	$(verbose) $(MIX) deps.get
	$(verbose) $(MIX) deps.compile
	$(verbose) $(MIX) compile

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

distclean-ex: clean-ex
	$(verbose) $(RM_F) mix.lock

clean-ex:
	$(verbose) $(RM_RF) _build deps

dist-ex: clean compile-ex

COMPILE=compile-erl compile-ex
CLEAN=clean-erl clean-ex
DISTCLEAN=distclean-erl distclean-ex
DIST=dist-erl dist-ex
else
COMPILE=compile-erl
CLEAN=clean-erl
DISTCLEAN=distclean-erl
DIST=dist-erl
endif

ifdef NO_LINT
LINT=
else
lint:
	$(verbose) $(REBAR) lint

LINT=lint
endif

compile-erl:
	$(verbose) $(REBAR) update
	$(verbose) $(REBAR) compile

tests:
	$(verbose) $(REBAR) eunit

doc::
	$(verbose) $(REBAR) as doc edoc

dist: $(DIST)

clean: $(CLEAN)

distclean: $(DISTCLEAN)

dev: compile-erl
	$(verbose) erl -pa _build/default/lib/*/ebin _build/default/lib/*/include $(ERL_CONFIG)

dist-erl: clean compile-erl tests $(LINT) doc

clean-erl:
	$(verbose) $(RM_RF) _build test/eunit

distclean-erl: clean-erl
	$(verbose) $(RM_F) rebar.lock

# Elixir

local.hex:
	$(MIX) local.hex --force

local.rebar:
	$(MIX) local.rebar --force

# Update

BU_MK_REPO ?= https://github.com/botsunit/bu.mk
BU_MK_COMMIT ?=
BU_MK_BUILD_DIR ?= .bu.mk.build

bu-mk:
	git clone $(BU_MK_REPO) $(BU_MK_BUILD_DIR)
ifdef BU_MK_COMMIT
	cd $(BU_MK_BUILD_DIR) && git checkout $(BU_MK_COMMIT)
endif
	$(CP) $(BU_MK_BUILD_DIR)/bu.mk ./bu.mk
	$(RM_RF) $(BU_MK_BUILD_DIR)

