install: 
	opam switch create . ocaml-base-compiler.4.14.1
	eval $$(opam env)
	opam install . --deps-only
	opam install . irmin-git irmin-server irmin-client

build: 
	rm -rf dist/main.js   
	dune build 

run-server: 
	dune exec ./server/server.exe  

start: 
	rm -rf dist/main.js   
	dune build   
	dune exec ./server/server.exe 
