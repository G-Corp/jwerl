HAS_ELIXIR=1

include bu.mk

release: dist
	$(verbose) $(REBAR) hex publish

