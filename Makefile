STDBASE=/usr/local/src/pugs/src/perl6
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib \
       STD5PREFIX=$(STDBASE)/

all:
	$(STDENV) perl CompilerDriver.pm > Program.cs
	gmcs /target:exe Kernel.cs Program.cs

Niecza/Grammar.pmc: Niecza/Grammar.pm6
	$(STDENV) $(STDBASE)/viv -5 -o Niecza/Grammar.pmc Niecza/Grammar.pm6
