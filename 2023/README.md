# Adding days
```
(executable
 (public_name <day>)
 (name <day>)
 (libraries ocaml core re)
 (modules <day>))
```

# How I manange my gitignore
I like using _projectile-find-file_ in emacs and gitignore remove those files form the search with is really nice, but sometimes I like to look at the advent of code inputs on my local filesystem so as I complete a day I add it to the gitignore

# Project structure
*lib* all library functons
*src* all binary code
*build* binarys produced with respective names

# Executing a day 
> just e 2 -> dune exec day2
```
just all <year> <day> 
# Example 
just all 2023 day5
```

- [x] Day 1 _STARTED:_<2024-01-07 Sun> _FINISED:_<2024-01-05 Sun>
- [x] Day 2 _STARTED:_ <2024-01-07 Sun 10:15> _FINISHED_ 
- [x] Day 4 _STARTED:_ <2024-01-07 Sun 10:15> _FINISHED_ 
- [ ] Day 5 _STARTED:_ <2024-01-07 Sun 10:15> _FINISHED_ 

# Things I learned

> compile a ml file with ocamlc -g a.out
> Angstrom library
> ocamlc,ocamlfind and compiling ocaml code without dune
> ocamldebug and debuggin ocaml code effectively
> Trying to make a custom toplevel with external libraries
> You can do a basic trace of a function with #trace <function>

You can use _M-x ocamldebug_ in emacs to quickly debug an ocaml executable

## Merlin
- merlin requires the opam package merlin-dot-reader to work properly
- you need the -I flag to tell ocamlc where the cmo files are usually *-I lib*
- add what external libraries you want to the _.merlin_ file eg *PKG core*


