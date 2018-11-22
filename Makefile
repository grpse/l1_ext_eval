
#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

#.PHONY:	all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot -verbose 2
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

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

only_test_collect:
	$(OCB) -tag debug tests/test_collect.native

only_test_eval:
	$(OCB) -tag debug tests/test_eval.native

test_collect: only_test_collect
	./tests/test_collect.native

test_eval: only_test_eval
	./tests/test_eval.native