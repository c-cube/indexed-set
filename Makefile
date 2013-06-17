INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = ixSet.cmxa ixSet.cma
TARGETS_DOC = ixSet.docdir/index.html

OPTIONS = -use-ocamlfind
	
all: lib

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGETS_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

clean:
	ocamlbuild -clean

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags
