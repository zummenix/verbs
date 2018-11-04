.PHONY: build clean open

build:
	elm make "src/Main.elm" --output "target/index.html"

open: build
	open "target/index.html"

clean:
	rm -rf elm-stuff/
