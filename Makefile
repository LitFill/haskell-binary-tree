build: Main.hs
	@ghc -Wincomplete-patterns -v0 -outputdir temp -o Main Main.hs

buildX: Main.hs
	@ghc -Wall -Wextra -O5 -v0 -outputdir temp -o Main Main.hs

clean: Main
	@rm -rf temp Main

run: build
	@./Main
