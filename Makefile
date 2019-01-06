HAS_ELIXIR=1
NO_XREF=1
NO_REGISTRY_UPDATE=1

include bu.mk

release: dist lint tag
	$(verbose) $(REBAR) hex publish

doc::
	$(verbose) echo '## Contributors ##' >> README.md
	$(verbose) echo >> README.md
	$(verbose) echo 'Thanks goes to these wonderful people ([emoji key](https://github.com/kentcdodds/all-contributors#emoji-key)):' >> README.md
	$(verbose) echo >> README.md
	$(verbose) echo '<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->' >> README.md
	$(verbose) echo '<!-- prettier-ignore -->' >> README.md
	$(verbose) echo '<!-- ALL-CONTRIBUTORS-LIST:END -->' >> README.md
	$(verbose) echo >> README.md
	$(verbose) echo 'This project follows the [all-contributors](https://github.com/kentcdodds/all-contributors) specification. Contributions of any kind welcome!' >> README.md
	$(verbose) all-contributors generate
