INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = ixSet.cmxa ixSet.cma ixSet.cmxs
TARGETS_DOC = ixSet.docdir/index.html
INSTALL = $(addprefix _build/, $(TARGETS_LIB)) ixSet.mli

OPTIONS = -use-ocamlfind
	
all: lib

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGETS_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

clean:
	ocamlbuild -clean

install:
	ocamlfind install ixSet META $(INSTALL)

uninstall:
	ocamlfind remove ixSet

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags
