### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs
RM=rm -f
CP=cp

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs Utils.cs
csbackend=CLRBackend.cs
csxdr=CrossDomainReceiver.cs

# Tell make to regard the following targets as not being filenames
.PHONY: all aot test spectest clean realclean
.PHONY: help

# keep this in dependency order
libunits=CORE JSYNC
srcunits=CClass Body Unit CgOp Op OpHelpers Sig RxOp NAME Stash STD \
	 NieczaGrammar Metamodel OptRxSimple NAMOutput Operator NieczaActions \
	 NieczaFrontendSTD NieczaPassBegin NieczaPassBeta NieczaPassSimplifier \
	 NieczaPathSearch NieczaBackendNAM NieczaBackendDotnet \
	 NieczaBackendClisp NieczaCompiler GetOptLong

all: run/Niecza.exe obj/Kernel.dll obj/CORE.nam obj/CLRBackend.exe
	@git describe --tags > VERSION

obj/CORE.nam: run/Niecza.exe obj/CLRBackend.exe lib/CORE.setting
	$(RUN_CLR) run/Niecza.exe -C CORE

run/Niecza.exe: .fetch-stamp $(patsubst %,src/%.pm6,$(srcunits)) src/niecza
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -v -c -Bnam niecza
	for nfile in $(libunits) $(srcunits); do \
	    if [ boot/obj/$$nfile.nam -nt boot/obj/$$nfile.dll -o \
			! -e boot/obj/$$nfile.dll ]; then \
		echo $$nfile; \
		$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj $$nfile.nam $$nfile.dll 0; \
	fi; done
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj MAIN.nam MAIN.exe 1
	$(CP) $(patsubst %,boot/obj/%.dll,Kernel CrossDomainReceiver $(libunits) $(srcunits)) run/
	$(CP) boot/obj/MAIN.exe run/Niecza.exe

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	wget --no-check-certificate -Oboot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
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

aot:
	mono --aot run/*.dll run/Niecza.exe

test: all
	$(RUN_CLR) run/Niecza.exe -c test.pl
	prove -e "$(RUN_CLR)" obj/MAIN.exe

spectest: all
	@t/run_spectests

p6eval: all
	$(RUN_CLR) run/Niecza.exe -C CORE Test JSYNC

clean:
	@touch obj/CORE.dll obj/MAIN.exe obj/MAIN.nam
	@rm obj/*.dll obj/*.exe obj/*.nam
	@touch run/Niecza.exe
	@rm run/Niecza.exe
	@touch run/CORE.dll
	@rm run/*.dll
	@touch run/CORE.dll.so
	@rm run/*.dll.so
	@touch a~
	@rm -r *~

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

