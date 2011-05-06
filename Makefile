### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs
RM=rm -f
CP=cp

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs Utils.cs \
	 ObjModel.cs
csbackend=CLRBackend.cs
csxdr=CrossDomainReceiver.cs

# Tell make to regard the following targets as not being filenames
.PHONY: all aot test spectest clean realclean
.PHONY: help

libunits=CORE JSYNC
srcunits=CClass Body Unit CgOp Op OpHelpers Sig RxOp NAME Stash STD \
	 NieczaGrammar Metamodel OptRxSimple NAMOutput Operator NieczaActions \
	 NieczaFrontendSTD NieczaPassBegin NieczaPassBeta NieczaPassSimplifier \
	 NieczaPathSearch NieczaBackendNAM NieczaBackendDotnet \
	 NieczaBackendClisp NieczaBackendHoopl NieczaCompiler GetOptLong

all: run/Niecza.exe obj/Kernel.dll obj/CORE.nam obj/CLRBackend.exe
	@git describe --tags > VERSION

$(patsubst %,boot/obj/%.nam,$(srcunits)): boot/obj/%.nam: .fetch-stamp src/%.pm6 boot/obj/CORE.nam
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -Bnam -C $*
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj $*.nam $*.dll 0

obj/CORE.nam: run/Niecza.exe obj/CLRBackend.exe lib/CORE.setting
	$(RUN_CLR) run/Niecza.exe -C CORE

run/Niecza.exe: .fetch-stamp $(patsubst %,boot/obj/%.nam,$(srcunits)) src/niecza
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -c -Bnam niecza
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj MAIN.nam MAIN.exe 1
	$(CP) $(patsubst %,boot/obj/%.dll,Kernel CrossDomainReceiver $(libunits) $(srcunits)) run/
	$(CP) boot/obj/MAIN.exe run/Niecza.exe

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	wget --no-check-certificate -Oboot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
	$(RUN_CLR) boot/run/Niecza.exe -C CORE JSYNC
	touch .fetch-stamp

obj/CrossDomainReceiver.dll: $(patsubst %,lib/%,$(csxdr))
	$(CSC) /target:library /out:obj/CrossDomainReceiver.dll \
	    $(patsubst %,lib/%,$(csxdr))
obj/Kernel.dll: $(patsubst %,lib/%,$(cskernel)) obj/CrossDomainReceiver.dll
	$(CSC) /target:library /out:obj/Kernel.dll /r:CrossDomainReceiver.dll \
	    /lib:obj /unsafe+ $(patsubst %,lib/%,$(cskernel))
obj/CLRBackend.exe: $(patsubst %,lib/%,$(csbackend)) obj/Kernel.dll obj/CrossDomainReceiver.dll
	$(CSC) /target:exe /lib:obj /out:obj/CLRBackend.exe /r:Kernel.dll \
	    /r:CrossDomainReceiver.dll $(patsubst %,lib/%,$(csbackend))

aot: all
	mono --aot run/*.dll run/Niecza.exe

test: all
	$(RUN_CLR) run/Niecza.exe -c test.pl
	prove -e "$(RUN_CLR)" obj/MAIN.exe

spectest: all
	@t/run_spectests

p6eval: all
	$(RUN_CLR) run/Niecza.exe -C CORE Test JSYNC

clean:
	@rm -f obj/*.dll obj/*.exe obj/*.nam
	@rm -f run/Niecza.exe
	@rm -f run/*.dll
	@rm -f run/*.dll.so
	@rm -fr *~

reboot: all
	# setup a clean build area
	rm -rf stage2/ stage3/
	mkdir -p stage2/lib stage2/obj stage2/run stage2/boot stage2/boot/obj \
	    stage3/lib stage3/obj stage3/run stage3/boot stage3/boot/obj
	touch stage2/FETCH_URL stage3/FETCH_URL stage2/.fetch-stamp \
	    stage3/.fetch-stamp
	cp -a src/ Makefile stage2/
	cp -a src/ Makefile stage3/
	# build a current Niecza with current Niecza
	cp obj/Kernel.dll obj/CrossDomainReceiver.dll obj/CLRBackend.exe \
	    stage2/boot/obj
	cp -a lib run stage2/boot
	cd stage2 && $(RUN_CLR) boot/run/Niecza.exe -C CORE JSYNC
	cd stage2 && $(MAKE)
	# verify that the new Niecza can build itself correctly
	cp stage2/obj/Kernel.dll stage2/obj/CrossDomainReceiver.dll \
	    stage2/obj/CLRBackend.exe stage3/boot/obj
	cp -a lib stage2/run stage3/boot
	cd stage3 && $(RUN_CLR) boot/run/Niecza.exe -C CORE JSYNC
	cd stage3 && $(MAKE) test
	# yay, stage2/ looks like a good new bootstrap version
	# clean up the stuff that should NOT go into the s
	cd stage2 && rm -rf lib/*.cs obj/* src
	cp obj/CrossDomainReceiver.dll obj/Kernel.dll obj/CLRBackend.exe \
	    stage2/obj
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

# grep -r '^use' src/*.pm6 | sed 's|src/\(.*\)\.pm6:use \(.*\);|boot/obj/\1.nam: boot/obj/\2.nam|'
boot/obj/Metamodel.nam: boot/obj/NAME.nam
boot/obj/Metamodel.nam: boot/obj/Stash.nam
boot/obj/NAMOutput.nam: boot/obj/JSYNC.nam
boot/obj/NAMOutput.nam: boot/obj/Metamodel.nam
boot/obj/NAMOutput.nam: boot/obj/Sig.nam
boot/obj/NieczaActions.nam: boot/obj/Op.nam
boot/obj/NieczaActions.nam: boot/obj/RxOp.nam
boot/obj/NieczaActions.nam: boot/obj/Body.nam
boot/obj/NieczaActions.nam: boot/obj/Unit.nam
boot/obj/NieczaActions.nam: boot/obj/Sig.nam
boot/obj/NieczaActions.nam: boot/obj/CClass.nam
boot/obj/NieczaActions.nam: boot/obj/OptRxSimple.nam
boot/obj/NieczaActions.nam: boot/obj/OpHelpers.nam
boot/obj/NieczaActions.nam: boot/obj/Operator.nam
boot/obj/NieczaBackendClisp.nam: boot/obj/NieczaBackendNAM.nam
boot/obj/NieczaBackendDotnet.nam: boot/obj/NieczaBackendNAM.nam
boot/obj/NieczaBackendDotnet.nam: boot/obj/NAMOutput.nam
boot/obj/NieczaBackendHoopl.nam: boot/obj/NieczaBackendNAM.nam
boot/obj/NieczaBackendNAM.nam: boot/obj/NAMOutput.nam
boot/obj/NieczaCompiler.nam: boot/obj/JSYNC.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/STD.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/Stash.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/NieczaGrammar.nam
boot/obj/NieczaFrontendSTD.nam: boot/obj/NieczaActions.nam
boot/obj/NieczaGrammar.nam: boot/obj/STD.nam
boot/obj/NieczaPassBegin.nam: boot/obj/Unit.nam
boot/obj/NieczaPassBegin.nam: boot/obj/Sig.nam
boot/obj/NieczaPassBegin.nam: boot/obj/Body.nam
boot/obj/NieczaPassBegin.nam: boot/obj/Op.nam
boot/obj/NieczaPassBegin.nam: boot/obj/Metamodel.nam
boot/obj/NieczaPassBeta.nam: boot/obj/CgOp.nam
boot/obj/Operator.nam: boot/obj/Body.nam
boot/obj/Operator.nam: boot/obj/Sig.nam
boot/obj/Operator.nam: boot/obj/OpHelpers.nam
boot/obj/Op.nam: boot/obj/CgOp.nam
boot/obj/OptRxSimple.nam: boot/obj/RxOp.nam
boot/obj/RxOp.nam: boot/obj/CgOp.nam
boot/obj/RxOp.nam: boot/obj/CClass.nam
boot/obj/Sig.nam: boot/obj/CgOp.nam
boot/obj/STD.nam: boot/obj/NAME.nam
boot/obj/STD.nam: boot/obj/Stash.nam
