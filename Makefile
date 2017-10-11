HAS_ELIXIR=1
NO_XREF=1
NO_REGISTRY_UPDATE=1

include bu.mk

release: dist lint tag
	$(verbose) $(REBAR) hex publish

