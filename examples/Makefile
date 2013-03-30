.PHONY: all
RESULT = simple.txt include.txt prog.ml test.html test2.c dynamic.txt \
	dynamic2.txt
all: $(RESULT)

simple.txt: simple.mlx
	../camlmix $< -o $@

include.txt: include1.mlx include2.mlx
	../camlmix include2.mlx -o include.txt

prog.ml: xpp.mlx prog.mlx
	../camlmix $^ -o $@
	ocaml prog.ml || :

test.html: test.mlx macro.ml
	../camlmix test.mlx -o $@

test2.c: test2.mlx macro.ml
	../camlmix test2.mlx -o $@

dynamic.txt: dynamic
	./dynamic "`date`" > dynamic.txt
	cat dynamic.txt
dynamic: dynamic.ml dynamic_main.ml
	ocamlc -o dynamic dynamic.ml dynamic_main.ml
dynamic.ml: dynamic.mlx
	../camlmix -c dynamic.mlx -co dynamic.ml -fun
	ocamlc -i dynamic.ml

dynamic2.txt: dynamic2
	./dynamic2 Johnny Smith 33 > dynamic2.txt
	cat dynamic2.txt
dynamic2: dynamic2.ml dynamic2_main.ml
	ocamlc -o dynamic2 dynamic2.ml dynamic2_main.ml
dynamic2.ml: dynamic2.mlx
	../camlmix -c dynamic2.mlx -co dynamic2.ml -fun
	ocamlc -i dynamic2.ml

.PHONY: clean
clean:
	rm -f $(RESULT) *.mlx.ml dynamic.ml dynamic2.ml \
		dynamic dynamic2 *~ *.cmo *.cmi
