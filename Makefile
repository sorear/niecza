STDBASE=/usr/local/src/pugs/src/perl6
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

test: all
	perl -MFile::Slurp -MCompilerDriver=:all -e 'header; mainline(scalar read_file("test.pl")); bootstrap' > Program.cs
	gmcs /r:Setting.dll Program.cs
	prove -e 'mono --debug=casts' Program.exe

all: Niecza/Grammar.pmc
	perl -MCompilerDriver=:all -e 'header; setting' > Setting.cs
	gmcs /target:library /out:Setting.dll Kernel.cs Setting.cs

Niecza/Grammar.pmc: Niecza/Grammar.pm6
	STD5PREFIX=$(STDBASE)/ $(STDENV) $(STDBASE)/viv -5 -o Niecza/Grammar.pmc Niecza/Grammar.pm6
