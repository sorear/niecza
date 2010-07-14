STDBASE=/usr/local/src/pugs/src/perl6
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

COMPILER=Body.pm CodeGen.pm CompilerDriver.pm Decl.pm Op.pm Sig.pm Unit.pm\
	 Niecza/Actions.pm Niecza/Grammar.pmc

test: $(COMPILER) test.pl Setting.dll
	perl -MFile::Slurp -MCompilerDriver=:all -e 'header; mainline(scalar read_file("test.pl")); bootstrap' > Program.cs
	gmcs /r:Setting.dll Program.cs
	prove -e 'mono --debug=casts' Program.exe

all: Setting.dll

Setting.cs: $(COMPILER) setting
	perl -MCompilerDriver=:all -e 'header; setting' > Setting.cs

Setting.dll: Kernel.cs Setting.cs
	gmcs /target:library /out:Setting.dll Kernel.cs Setting.cs
	mono --aot Setting.dll

Niecza/Grammar.pmc: Niecza/Grammar.pm6
	STD5PREFIX=$(STDBASE)/ $(STDENV) $(STDBASE)/viv -5 -o Niecza/Grammar.pmc Niecza/Grammar.pm6
