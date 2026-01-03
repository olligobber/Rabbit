index.js: src/*.purs *.dhall
	spago bundle-app

.PHONY: clean
clean:
	rm -rf .spago
	rm -rf output
	rm -f .purs-repl
	rm -f index.js