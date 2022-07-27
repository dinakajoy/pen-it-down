install: 
	opam install . --deps-only   

build: 
	rm -rf dist/main.js   
	dune build 

run-server: 
	dune exec ./server/server.exe  

start: 
	rm -rf dist/main.js   
	dune build   
	dune exec ./server/server.exe 
