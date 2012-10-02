TESTME=perl test.pl

# basic, until we get our grammar based scripting tool.

runtests::
	$(TESTME) sized-types-example1.stdout sized-types-example1 
	$(TESTME) sized-types-test1.stdout sized-types-test1
	@echo "********* Passed all tests *************"

clean::
	rm -Rf run
	
start::
	cabal configure --with-compiler=/Users/andy/git/ghc/ghc/inplace/bin/ghc-stage2
