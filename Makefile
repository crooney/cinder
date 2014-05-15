FAY=fay
FAYFLAGS=--pretty --package fay-base,fay-text,cinder

DEPENDS=$(wildcard src/*/*.hs)

EX_SRC=$(wildcard examples/*.hs)
EXAMPLES=$(patsubst %.hs,%.js,$(EX_SRC))
EX_SVG=$(patsubst %.hs,%.svg,$(EX_SRC))
EX_HTML=$(patsubst %.hs,%.html,$(EX_SRC))

LANGS=SVG HTML
LANGS_GEN=$(patsubst %,src/Cinder/%/Attributes.hs,$(LANGS))
LANGS_GEN+=$(patsubst %,src/Cinder/%/Elements.hs,$(LANGS))

UTILS=$(wildcard util/*)

all: langs cabal examples

cabal: $(DEPENDS)
	cabal install

examples: $(EX_SVG) $(EX_HTML) cabal $(EXAMPLES)

release: FAYFLAGS=--closure --no-builtins -O --package fay-base,fay-text,cinder
release: clean all

%.js: %.hs
	$(FAY) $(FAYFLAGS) -o $@ $<

$(EX_SVG): template.svg

$(EX_HTML): template.html

$(EXAMPLES): $(SOURCE) $(DEPENDS)

$(LANGS_GEN):
	cd util/; ./combos.sh $(LANGS) ; cd ..

langs: $(UTILS) $(LANGS_GEN)

%.svg: %.SVG
	ln -s $(<F) $@

%.html: %.HTML
	ln -s $(<F) $@

%.svg: %.hs
	cp -n template.svg $@
	sed -i -e "s/TEMPLATE/$(*F)/g" $@

%.html: %.hs
	cp -n template.html $@
	sed -i -e "s/TEMPLATE/$(*F)/g" $@

.PHONY: clean
clean:
	rm -f examples/*.js examples/*.html examples/*.svg

.PHONY: squeaky
squeaky: clean
	rm -f $(LANGS_GEN)
