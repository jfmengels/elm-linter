SOURCES = $(shell find ../src ./*.elm -type f -name "*.elm" -print)

elm.js: Main.elm $(SOURCES)
	elm make --output=elm.js Main.elm
