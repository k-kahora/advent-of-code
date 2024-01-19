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

# Executing a day 
> just e 2 -> dune exec day2
```
just e <day_number> 
```

- [x] Day 1 _STARTED:_<2024-01-07 Sun> _FINISED:_<2024-01-05 Sun>
- [ ] Day 2 _STARTED:_ <2024-01-07 Sun 10:15> _FINISHED_ 

# Things I learned

> compile a ml file with ocamlc -g a.out
> You can do a basic trace of a function with #trace <function>

You can use _M-x ocamldebug_ in emacs to quickly debug an ocaml executable

## Merlin
- merlin requires the opam package merlin-dot-reader to work properly
- you need the -I flag to tell ocamlc where the cmo files are usually *-I lib*


