# -I tells the compiler where to look for cmo files
core := `ocamlfind query core`
re := `ocamlfind query re`

set positional-arguments

advent year:
	ocamlc -g -I {{year}}/lib -c {{year}}/lib/advent.mli
	ocamlfind ocamlc -g -package core,stdio, -I {{year}}/lib -c {{year}}/lib/advent.ml

default:
	just advent
	ocamlfind ocamlc -g -package core,angstrom,fmt -I lib -c src/day4.ml
	@# Linking stage 
	ocamlfind ocamlc -g -package core,stdio,angstrom,fmt -linkpkg -o build/day4 lib/advent.cmo src/day4.cmo
	build/day4
	
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
	
test:
	ocamlfind ocamlc -package core -c src/test.ml
	ocamlfind ocamlc -package core -linkpkg -o build/test src/test.cmo
	build/test

parse:
	just advent
	ocamlfind ocamlc -g -package angstrom,fmt,core,stdio -I lib -c src/parser.ml
	ocamlfind ocamlc -g -package angstrom,fmt,core,stdio -linkpkg -o build/parse lib/advent.cmo src/parser.cmo
	build/parse
	
all year day:
	just advent {{year}}
	ocamlfind ocamlc -g -package core,angstrom,fmt -I {{year}}/lib -c {{year}}/src/{{day}}.ml
	@# Linking stage 
	ocamlfind ocamlc -g -package core,stdio,angstrom,fmt -linkpkg -o {{year}}/build/{{day}} {{year}}/lib/advent.cmo {{year}}/src/{{day}}.cmo
	{{year}}/build/{{day}}




