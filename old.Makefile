# Add your modules here
MODULES=movable interactions state block render input process const read_json util ai main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,ANSITerminal,graphics

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

# CUSTOM ENVIORMENT FOR TESTING YOUR OWN MODULES
tank:
	utop -init tank.ml

interactions:
	utop -init interactions.ml
# FOLLOW THE PATTERN

ai:
	utop -init ai.ml

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tank_src.zip *.ml* _tags Makefile *.json 

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out