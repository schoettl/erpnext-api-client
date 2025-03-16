
README.md: README.org
	pandoc -f org -t markdown_mmd $< > $@
