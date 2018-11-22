OC_FLAGS="-I tests/"
SOURCES="typenames.ml collect.ml eval.ml main.ml"
mkdir -p _build
sh sanitize.sh
ocamlopt $OC_FLAGS -o _build/test_collect $SOURCES tests/testexpressions.ml tests/test_collect.ml
ocamlopt $OC_FLAGS -o _build/test_eval $SOURCES tests/testexpressions.ml tests/test_eval.ml