
.PHONY: sdist
sdist: README.md
	stack test
	stack sdist
	echo "Upload the package to hackage with 'stack upload .'"

README.md: README.org
	pandoc -f org -t markdown_mmd $< > $@
