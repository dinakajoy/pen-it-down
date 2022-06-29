install: 
	opam install . --deps-only   

build: 
	dune build   

start: 
	rm -rf dist/main.js   
	dune build   
	dune exec ./server/server.exe    

run-server: 
	dune exec ./server/server.exe