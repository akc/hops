all: dist                      \
     dist/man                  \
     dist/tutorial             \
     dist/language             \
     dist/hops.png             \
     dist/github-pandoc.css    \
     dist/index.html           \
     dist/man/index.html       \
     dist/tutorial/index.html  \
     dist/language/index.html

dist:
	mkdir -p dist

dist/tutorial:
	mkdir -p dist/tutorial

dist/language:
	mkdir -p dist/language

dist/man:
	mkdir -p dist/man

dist/%.png: %.png
	cp $< $@

dist/%.css: %.css
	cp $< $@

dist/index.html: index.md github-pandoc.css template.html
	pandoc -s -t html --template=template.html --css github-pandoc.css $< -o $@

dist/man/index.html: hops.md github-pandoc.css
	pandoc -s -t html --css ../github-pandoc.css $< -o $@

dist/%/index.html: %.md github-pandoc.css template.html
	pandoc -s -t html --template=template.html --css ../github-pandoc.css $< -o $@
