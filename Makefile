STDBASE:=$(shell pwd)/STD_checkout
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

COMPILER=Body.pm CgOp.pm CodeGen.pm CompilerDriver.pm Decl.pm Op.pm RxOp.pm\
	 Sig.pm Unit.pm Niecza/Actions.pm Niecza/Grammar.pmc .STD_build_stamp

all: CORE.dll
	git rev-parse HEAD | cut -c1-7 > VERSION

test: $(COMPILER) test.pl CORE.dll
	perl -MFile::Slurp -MCompilerDriver=:all -e 'header; mainline(scalar read_file("test.pl"))' > Program.cs
	gmcs /r:Kernel.dll /r:CORE.dll Program.cs
	prove -e 'mono --debug=casts' Program.exe

.DELETE_ON_ERROR:

CORE.cs: $(COMPILER) CORE.setting
	perl -MCompilerDriver=:all -e 'header; setting' > CORE.cs

Kernel.dll: Kernel.cs
	gmcs /target:library /out:Kernel.dll Kernel.cs
	mono --aot Kernel.dll

CORE.dll: Kernel.dll CORE.cs
	gmcs /target:library /out:CORE.dll /r:Kernel.dll CORE.cs
	mono --aot CORE.dll

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
