pugs ::
	cabal build
	cp dist/build/pugs/pugs pugs

ghci ::
	ghci ./dist/build/pugs/pugs-tmp/cbits/*o -idist/build/autogen -isrc src/Main.hs 

clean :: distclean

distclean purge ::
	rm -fr Pugs.buildinfo dist/ perlxsi.c

