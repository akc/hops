gfscript.1: gfscript.md
	pandoc -s -t man gfscript.md -o gfscript.1

gfscript.html: gfscript.md
	pandoc -s -t html gfscript.md -o gfscript.html

README.html: README.md
	pandoc -s -t html README.md -o README.html
