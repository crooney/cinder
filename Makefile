FAY=fay
FAYFLAGS=--pretty --include src

DEPENDS=$(wildcard src/*/*.hs)
TARGET=build/DOM.js
SOURCE=src/Cinder/DOM.hs

EX_SRC=$(wildcard examples/*.hs)
EXAMPLES=$(patsubst %.hs,%.js,$(EX_SRC))
EX_SVG=$(patsubst %.hs,%.svg,$(EX_SRC))
EX_HTML=$(patsubst %.hs,%.html,$(EX_SRC))

LANGS=SVG
LANGS_GEN=$(patsubst %,src/Cinder/%/Attributes.hs,$(LANGS))
LANGS_GEN+=$(patsubst %,src/Cinder/%/Elements.hs,$(LANGS))

UTILS=$(wildcard util/*)

all: $(TARGET) langs examples

$(TARGET): FAYFLAGS+= --library
$(TARGET): build $(SOURCE) $(DEPENDS)
	$(FAY) $(FAYFLAGS) -o $@ $(SOURCE)

examples: $(EX_SVG) $(EX_HTML) $(TARGET) $(EXAMPLES)

release: FAYFLAGS=--closure --no-builtins -O --include src
release: clean all

%.js: %.hs
	$(FAY) $(FAYFLAGS) -o $@ $<

build:
	mkdir -p build

$(EX_SVG): template.svg

$(EX_HTML): template.html

$(EXAMPLES): $(SOURCE) $(DEPENDS)

$(LANGS_GEN):
	cd util/; ./combos.sh $(LANGS) ; cd ..

langs: $(UTILS) $(LANGS_GEN)

%.svg: %.SVG
	ln -s $(<F) $@

%.svg: %.hs
	cp -n template.svg $@
	sed -i -e "s/TEMPLATE/$(*F)/g" $@

%.html: %.hs
	cp -n template.html $@
	sed -i -e "s/TEMPLATE/$(*F)/g" $@

.PHONY: clean
clean:
	rm -fr build
	rm -f examples/*.js examples/*.html examples/*.svg

.PHONY: squeaky
squeaky: clean
	rm -f $(LANGS_GEN)
