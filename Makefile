STDBASE:=$(shell pwd)/STD_checkout
STDENV=PERL5LIB=$(STDBASE) PERL6LIB=$(STDBASE):$(STDBASE)/lib

COMPILER=Body.pm CgOp.pm CodeGen.pm CompilerDriver.pm Decl.pm Op.pm RxOp.pm\
	 Sig.pm Unit.pm Niecza/Actions.pm Niecza/Grammar.pmc .STD_build_stamp

all: CORE.dll
	git rev-parse HEAD | cut -c1-7 > VERSION

test: $(COMPILER) test.pl CORE.dll Test.dll
	perl niecza_eval -v --stop-after=gmcs test.pl
	prove -e mono MAIN.exe

.DELETE_ON_ERROR:

Kernel.dll: Kernel.cs
	gmcs /target:library /out:Kernel.dll Kernel.cs
	mono --aot Kernel.dll

CORE.dll: $(COMPILER) Kernel.dll CORE.setting
	perl niecza_eval --aot -L NULL -v -c CORE.setting

Test.dll: $(COMPILER) Kernel.dll CORE.dll Test.pm6
	perl niecza_eval --aot -v -c Test.pm6

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

.PHONY: t/spec
t/spec:
	if [ ! -d t/spec ]; then \
	    svn checkout http://svn.pugscode.org/pugs/t/spec t/spec; \
	else \
	    svn update t/spec; \
	fi

t/*.t t/*/*.t t/*/*/*.t: all Test.dll t/spec
	prove --exec t/fudgeandrun --verbosity=1 $@
spectest: testable t/spectest.data
	prove --exec t/fudgeandrun - < t/spectest.data
