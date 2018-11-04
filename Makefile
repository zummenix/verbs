output = "target/index.html"

.PHONY: build clean open

build:
	elm make "src/Main.elm" --output $(output)

open: build
	open $(output)

clean:
	rm -rf elm-stuff/
