OC_FLAGS = -I tests/
SOURCES = typenames.ml collect.ml applysubs.ml eval.ml main.ml unify.ml typeinfer.ml

build_dir:
	mkdir -p _build

clean:
	sh sanitize.sh

compile_test_collect:
	ocamlc $(OC_FLAGS) -o _build/test_collect $(SOURCES) tests/testexpressions.ml tests/test_collect.ml

compile_test_eval:
	ocamlc $(OC_FLAGS) -o _build/test_eval $(SOURCES) tests/testexpressions.ml tests/test_eval.ml

compile_test_typeinfer:
	ocamlc $(OC_FLAGS) -o _build/test_typeinfer $(SOURCES) tests/testexpressions.ml tests/test_typeinfer.ml

test_collect: build_dir compile_test_collect clean
	./_build/test_collect

test_eval: build_dir compile_test_eval clean
	./_build/test_eval

test_typeinfer:	build_dir compile_test_typeinfer clean
	./_build/test_typeinfer