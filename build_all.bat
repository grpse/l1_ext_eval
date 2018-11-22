mkdir -p _build
ocamlopt -I tests/ -o _build/test_collect typenames.ml collect.ml eval.ml main.ml tests/testexpressions.ml tests/test_collect.ml
ocamlopt -I tests/ -o _build/test_eval typenames.ml collect.ml eval.ml main.ml tests/testexpressions.ml tests/test_eval.ml
ocamlopt -I tests/ -o _build/test_typeinfer typenames.ml collect.ml eval.ml unify.ml main.ml
sanitize.bat