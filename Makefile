# Add your modules here
MODULES=movable interactions state block render input process const read_json \
	util ai initializer main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,ANSITerminal,graphics,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

tank:
	utop -init tank.ml

interactions:
	utop -init interactions.ml

ai:
	utop -init ai.ml

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip tank_src.zip *.ml* _tags Makefile *.json doc.public -r *.md

docs: docs-public

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
	rm $(MAIN)
	rm $(TEST)