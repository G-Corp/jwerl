PROJECT = jwerl

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
ELIXIR_BINDINGS = jwerl

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = jsx
dep_jsx = hex 2.6.2

include erlang.mk
