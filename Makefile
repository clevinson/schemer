bin/eval: src
	stack ghc -- -o $@ -odir dist -hidir dist -package parsec -fglasgow-exts -isrc src/Main.hs
