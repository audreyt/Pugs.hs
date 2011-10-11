INSTALLBIN = $(shell perl -MConfig -e 'print $$Config::Config{installbin}')

pugs: dist/build/pugs/pugs
	cp $< $@

dist/build/pugs/pugs: dist/setup-config src/Pugs/*
	cabal build

dist/setup-config:
	cabal configure

install :: pugs
	cp $< $(INSTALLBIN)/pugs
	cp $< $(INSTALLBIN)/perl6

ghci ::
	ghci ./dist/build/pugs/pugs-tmp/cbits/*o -idist/build/autogen -isrc src/Main.hs 

clean :: distclean

distclean purge ::
	rm -fr pugs Pugs.buildinfo dist/ perlxsi.c

