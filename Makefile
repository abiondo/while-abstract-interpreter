#
# Pure OCaml, no packages, no _tags, code in several directories
#

# bin-annot is required for Merlin and other IDE-like tools
# The -I flag introduces sub-directories to search for code

.PHONY: all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte
