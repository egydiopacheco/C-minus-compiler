build:
	dune build

utop:
	dune utop src/frontend

.PHONY: test
test:
	dune exec test/main.exe

clean:
	dune clean
