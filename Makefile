STDBASE:=$(shell pwd)/STD_checkout
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

COMPILER=Body.pm CgOp.pm CodeGen.pm CompilerDriver.pm Decl.pm Op.pm RxOp.pm\
	 Sig.pm Unit.pm Niecza/Actions.pm Niecza/Grammar.pmc .STD_build_stamp

all: CORE.dll
	git rev-parse HEAD | cut -c1-7 > VERSION

test: $(COMPILER) test.pl CORE.dll Test.dll
	perl niecza_eval --cs-only test.pl
	gmcs /r:Kernel.dll /r:CORE.dll /r:Test.dll MAIN.cs
	prove -e 'mono --debug=casts' MAIN.exe

.DELETE_ON_ERROR:

Kernel.dll: Kernel.cs
	gmcs /target:library /out:Kernel.dll Kernel.cs
	mono --aot Kernel.dll

CORE.cs: $(COMPILER) CORE.setting
	perl niecza_eval --language=NULL --cs-only -c CORE.setting

CORE.dll: Kernel.dll CORE.cs
	gmcs /target:library /out:CORE.dll /r:Kernel.dll CORE.cs
	mono --aot CORE.dll

Test.cs: $(COMPILER) CORE.dll Test.pm6
	perl niecza_eval --cs-only -c Test.pm6

Test.dll: Kernel.dll CORE.dll Test.cs
	gmcs /target:library /out:Test.dll /r:Kernel.dll /r:CORE.dll Test.cs
	mono --aot Test.dll

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
