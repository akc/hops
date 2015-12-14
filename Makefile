hops.1: docs/hops.md
	pandoc -s -t man docs/hops.md -o hops.1

README.html: README.md
	pandoc -s -t html README.md -o README.html
