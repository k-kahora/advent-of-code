# -I tells the compiler where to look for cmo files
core := `ocamlfind query core`
re := `ocamlfind query re`

set positional-arguments
lexer year:
	ocamlfind ocamlc -g -package angstrom -I {{year}}/lib -c {{year}}/lib/lexer.mli
	ocamlfind ocamlc -g -package angstrom -I {{year}}/lib -c {{year}}/lib/lexer.ml

advent year:
	ocamlc -g -I {{year}}/lib -c {{year}}/lib/advent.mli
	ocamlfind ocamlc -g -package core,stdio, -I {{year}}/lib -c {{year}}/lib/advent.ml

	
debug year day:
	just all {{year}} {{day}}
	ocamldebug {{year}}/build/{{day}}
	
toplevel:
	ocamlfind ocamlmktop -package core,stdio -linkpkg -o build/toplevel lib/advent.cmo src/day4.cmo
	build/toplevel
utop:
	#utop -I lib -I {{core}} src/day4.cmo
	echo {{core}}
	utop -I {{core}} src/test.cmo

compile year day:
	just advent {{year}}
	just lexer {{year}}
	ocamlfind ocamlc -g -package core,angstrom,fmt -I {{year}}/lib -c {{year}}/src/{{day}}.ml
	@# Linking stage 
	ocamlfind ocamlc -g -package core,stdio,angstrom,fmt -linkpkg -o {{year}}/build/{{day}} {{year}}/lib/advent.cmo {{year}}/lib/lexer.cmo  {{year}}/src/{{day}}.cmo

all year day:
	just compile {{year}} {{day}}
	{{year}}/build/{{day}}

run year day:
	{{year}}/build/{{day}}


