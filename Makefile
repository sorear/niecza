### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs
RM=rm -f
CP=cp

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs Utils.cs \
	 ObjModel.cs BigInteger.cs Printf.cs CodeGen.cs \
	 GeneratedTrigFunctions.cs Serialize.cs

# Tell make to regard the following targets as not being filenames
.PHONY: all aot test spectest clean realclean
.PHONY: help

libunits=CORE JSYNC
srcunits=CClass CgOp Op OpHelpers Sig RxOp STD NieczaGrammar Metamodel \
	 OptRxSimple NAMOutput Operator NieczaActions NieczaFrontendSTD \
	 NieczaPassSimplifier OptBeta NieczaPathSearch NieczaBackendDotnet \
	 NieczaCompiler GetOptLong

all: run/Niecza.exe obj/Run.Kernel.dll obj/Run.CORE.dll
	@git describe --tags > VERSION

$(patsubst %,boot/obj/%.nam,$(srcunits)): boot/obj/%.nam: .fetch-stamp src/%.pm6 boot/obj/CORE.nam
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -Bnam -C $*
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj $*.nam $*.dll 0

obj/Run.CORE.dll: run/Niecza.exe obj/Run.Kernel.dll lib/CORE.setting
	$(RUN_CLR) run/Niecza.exe -C CORE

run/Niecza.exe: .fetch-stamp $(patsubst %,boot/obj/%.nam,$(srcunits)) src/niecza
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -c -Bnam niecza
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj MAIN.nam MAIN.exe 1
	$(CP) $(patsubst %,boot/obj/%.dll,Kernel CompilerBlob $(libunits) $(srcunits)) run/
	$(CP) boot/obj/MAIN.exe run/Niecza.exe

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	wget --no-check-certificate -Oboot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
	$(RUN_CLR) boot/run/Niecza.exe -C CORE
	$(RUN_CLR) boot/run/Niecza.exe -C JSYNC
	touch .fetch-stamp

boot/obj/CompilerBlob.dll: .fetch-stamp src/CompilerBlob.cs
	$(CSC) /target:library /out:boot/obj/CompilerBlob.dll /r:Kernel \
	    /lib:boot/obj src/CompilerBlob.cs
obj/Run.Kernel.dll: $(patsubst %,lib/%,$(cskernel))
	$(CSC) /target:exe /out:obj/Run.Kernel.dll /lib:obj /unsafe+ \
	    $(patsubst %,lib/%,$(cskernel))

perl5: obj/Perl5Interpreter.dll obj/p5embed.so
obj/Perl5Interpreter.dll: obj/Kernel.dll lib/Perl5Interpreter.cs
	$(CSC) /target:library /lib:obj /out:obj/Perl5Interpreter.dll /r:Kernel.dll lib/Perl5Interpreter.cs

obj/p5embed.so: lib/p5embed.c
	cc -shared -o obj/p5embed.so lib/p5embed.c `perl -MExtUtils::Embed -e ccopts -e ldopts`

aot: all
	mono --aot run/*.dll obj/Run.CORE.dll run/Niecza.exe

test: all
	$(RUN_CLR) run/Niecza.exe -c test.pl
	prove -e "$(RUN_CLR)" obj/Run.MAIN.exe

spectest: all
	@t/run_spectests

p6eval: all
	$(RUN_CLR) run/Niecza.exe -C CORE Test JSYNC

clean:
	@rm -f obj/*.dll obj/*.exe obj/*.nam obj/*.so
	@rm -f run/Niecza.exe
	@rm -f run/*.dll
	@rm -f run/*.dll.so
	@rm -fr *~

half_reboot: all
	# setup a clean build area
	rm -rf stage2/ stage3/
	mkdir -p stage2/obj stage2/run stage2/boot stage2/boot/obj \
	    stage3/obj stage3/run stage3/boot stage3/boot/obj
	touch stage2/FETCH_URL stage3/FETCH_URL stage2/.fetch-stamp \
	    stage3/.fetch-stamp
	cp -a src/ lib/ Makefile stage2/
	cp -a src/ lib/ Makefile stage3/
	# build a current Niecza with current Niecza
	cp obj/Kernel.dll obj/CLRBackend.exe stage2/boot/obj
	cp -a lib run stage2/boot
	cd stage2 && $(RUN_CLR) boot/run/Niecza.exe -C CORE JSYNC
	cp test.pl stage2/
	cd stage2 && $(MAKE) test

reboot: half_reboot
	# verify that the new Niecza can build itself correctly
	cp stage2/obj/Kernel.dll stage2/obj/CLRBackend.exe stage3/boot/obj
	cp -a lib stage2/run stage3/boot
	cd stage3 && $(RUN_CLR) boot/run/Niecza.exe -C CORE JSYNC
	cp test.pl stage3/
	cd stage3 && $(MAKE) test
	# yay, stage2/ looks like a good new bootstrap version
	# clean up the stuff that should NOT go into the boot
	cd stage2 && rm -rf lib/*.cs obj/* src boot VERSION FETCH_URL \
	    Makefile test.pl
	cp obj/Kernel.dll obj/CLRBackend.exe stage2/obj
	cp -a LICENSE README.pod docs/ stage2/
	cd stage2 && zip -9r ../NewNieczaBootstrap.zip *

realclean: clean
	@rm .fetch-stamp

help:
	@echo ''
	@echo 'You can make the following targets in this Niecza Makefile:'
	@echo ''
	@echo 'all        the main Niecza compiler and runtime files (default)'
	@echo 'aot        Ahead of Time compile run/Niecza.exe and run/*.dll (increases speed)'
	@echo 'test       run/Niecza.exe test.pl'
	@echo 'spectest   t/run_spectests'
	@echo 'clean      remove all generated files'
	@echo 'realclean  clean and also require new download of bootstrap files'
	@echo 'help       this list of targets'
	@echo ''

boot/obj/NieczaBackendDotnet.nam: boot/obj/CompilerBlob.dll

# grep -r '^use' src/*.pm6 | sed 's|src/\(.*\)\.pm6:use \(.*\);|boot/obj/\1.nam: boot/obj/\2.nam|' | grep -v MONKEY_TYPING
boot/obj/NAMOutput.nam: boot/obj/JSYNC.nam
boot/obj/NAMOutput.nam: boot/obj/Metamodel.nam
boot/obj/NAMOutput.nam: boot/obj/Sig.nam
boot/obj/NieczaActions.nam: boot/obj/Op.nam
boot/obj/NieczaActions.nam: boot/obj/RxOp.nam
boot/obj/NieczaActions.nam: boot/obj/Sig.nam
boot/obj/NieczaActions.nam: boot/obj/CClass.nam
boot/obj/NieczaActions.nam: boot/obj/OpHelpers.nam
boot/obj/NieczaActions.nam: boot/obj/Operator.nam
boot/obj/NieczaBackendDotnet.nam: boot/obj/NAMOutput.nam
boot/obj/NieczaCompiler.nam: boot/obj/JSYNC.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/STD.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/NieczaGrammar.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/NieczaActions.nam
boot/obj/NieczaGrammar.nam: boot/obj/STD.nam
boot/obj/Operator.nam: boot/obj/Sig.nam
boot/obj/Operator.nam: boot/obj/OpHelpers.nam
boot/obj/Op.nam: boot/obj/CgOp.nam
boot/obj/OptBeta.nam: boot/obj/CgOp.nam
boot/obj/OptRxSimple.nam: boot/obj/RxOp.nam
boot/obj/RxOp.nam: boot/obj/CgOp.nam
boot/obj/RxOp.nam: boot/obj/CClass.nam
boot/obj/Sig.nam: boot/obj/CgOp.nam
