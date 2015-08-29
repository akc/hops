hops.1: hops.md
	pandoc -s -t man hops.md -o hops.1

hops.html: hops.md
	pandoc -s -t html hops.md -o hops.html

README.html: README.md
	pandoc -s -t html README.md -o README.html
