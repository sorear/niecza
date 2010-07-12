STDBASE=/usr/local/src/pugs/src/perl6
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

test: all
	perl -MCompilerDriver=:all -e 'header; mainline(q(say "Hello World")); bootstrap' > Program.cs
	gmcs /r:Setting.dll Program.cs
	mono --debug=casts Program.exe

all: Niecza/Grammar.pmc
	perl -MCompilerDriver=:all -e 'header; setting' > Setting.cs
	gmcs /target:library /out:Setting.dll Kernel.cs Setting.cs

Niecza/Grammar.pmc: Niecza/Grammar.pm6
	STD5PREFIX=$(STDBASE)/ $(STDENV) $(STDBASE)/viv -5 -o Niecza/Grammar.pmc Niecza/Grammar.pm6
