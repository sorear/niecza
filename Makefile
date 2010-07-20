STDBASE:=$(shell pwd)/STD_checkout
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

COMPILER=Body.pm CgOp.pm CodeGen.pm CompilerDriver.pm Decl.pm Op.pm RxOp.pm\
	 Sig.pm Unit.pm Niecza/Actions.pm Niecza/Grammar.pmc .STD_build_stamp

all: Setting.dll
	git rev-parse HEAD | cut -c1-7 > VERSION

test: $(COMPILER) test.pl Setting.dll
	perl -MFile::Slurp -MCompilerDriver=:all -e 'header; mainline(scalar read_file("test.pl")); bootstrap' > Program.cs
	gmcs /r:Setting.dll Program.cs
	prove -e 'mono --debug=casts' Program.exe

.DELETE_ON_ERROR:

Setting.cs: $(COMPILER) setting
	perl -MCompilerDriver=:all -e 'header; setting' > Setting.cs

Setting.dll: Kernel.cs Setting.cs
	gmcs /target:library /out:Setting.dll Kernel.cs Setting.cs
	mono --aot Setting.dll

Niecza/Grammar.pmc: Niecza/Grammar.pm6 .STD_build_stamp
	STD5PREFIX=$(STDBASE)/ $(STDENV) $(STDBASE)/viv -5 -o Niecza/Grammar.pmc Niecza/Grammar.pm6

.STD_checkout_stamp: STD_REVISION
	if [ ! -d STD_checkout ]; then \
	    svn checkout http://svn.pugscode.org/pugs/src/perl6@`cat STD_REVISION` STD_checkout; \
	else \
	    svn update -r`cat STD_REVISION` STD_checkout; \
	fi
	touch .STD_checkout_stamp

.STD_build_stamp: .STD_checkout_stamp
	cd STD_checkout && make && ./tryfile STD.pm6
	touch .STD_build_stamp
