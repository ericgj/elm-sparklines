SOURCES = $(shell find src/ -type f -name '*.elm')
EXAMPLE_SOURCES = $(shell find examples/ -type f -name '*.elm')
TEST_SOURCES = $(shell find tests/ -type f -name '*.elm')
TARGET = examples/elm.js

$(TARGET): $(SOURCES) $(EXAMPLE_SOURCES)  
	cd examples && elm make src/Main.elm --output=../$@

build: 
	elm make src/View/Simple.elm

build-examples: $(TARGET)

format: 
	elm-format src/ examples/ tests/ --yes

review:
	./node_modules/elm-review/bin/elm-review

review-examples:
	cd examples && ../node_modules/elm-review/bin/elm-review .

test:
	./node_modules/elm-test/bin/elm-test

watch:
	python -m http.server 8080 & cd examples && node ../node_modules/elm-watch/index.js hot && kill %1

.PHONY: build build-examples format review review-examples test

