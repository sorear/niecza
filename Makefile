### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs /debug+
RM=rm -f
CP=cp
WGET_O=wget --no-check-certificate -O
#WGET_O=curl -L -o

PREFIX=/usr/local
INSTBIN=$(PREFIX)/bin
INSTNIECZA=$(INSTBIN)/niecza
INSTLIB=$(PREFIX)/lib/niecza/lib
INSTEXE=$(PREFIX)/lib/niecza/bin
INSTOBJ=$(PREFIX)/lib/niecza/obj

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs Utils.cs \
	 ObjModel.cs BigInteger.cs Printf.cs CodeGen.cs \
	 GeneratedTrigFunctions.cs Serialize.cs UCD.cs \
	 SpecialMathFunctions.cs CompilerBinding.cs GetLine.cs

# Tell make to regard the following targets as not being filenames
.PHONY: all aot test spectest clean realclean
.PHONY: help

libunits=CORE
srcunits=CClass CgOp Op OpHelpers Sig RxOp STD NieczaGrammar OptRxSimple \
	 Operator NieczaActions NieczaFrontendSTD NieczaPassSimplifier \
	 OptBeta NieczaPathSearch NieczaBackendDotnet NieczaCompiler GetOptLong
precompunits=CORP CORN Test Threads JSYNC

all: run/Niecza.exe run/Kernel.dll obj/CORE.dll

$(patsubst %,run/%.ser,$(srcunits)): run/%.ser: .fetch-stamp src/%.pm6 run/CORE.ser
	NIECZA_KEEP_IL=1 $(RUN_CLR) boot/run/Niecza.exe --obj-dir run -I src -C $*

# hack - put VERSION info in place so the sobj/MAIN.exeetting build can embed it
obj/CORE.dll: run/Niecza.exe run/Kernel.dll lib/CORE.setting
	@git describe --tags > VERSION
	$(RUN_CLR) run/Niecza.exe --obj-dir obj -C CORE

run/Niecza.exe: .fetch-stamp $(patsubst %,run/%.ser,$(srcunits)) src/niecza
	NIECZA_KEEP_IL=1 $(RUN_CLR) boot/run/Niecza.exe --obj-dir run -I src -c src/niecza
	$(CP) run/MAIN.exe run/Niecza.exe

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	$(WGET_O) boot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
	NIECZA_KEEP_IL=1 $(RUN_CLR) boot/run/Niecza.exe --obj-dir=run -C $(libunits)
	touch .fetch-stamp
run/CORE.ser: .fetch-stamp

run/Kernel.dll: $(patsubst %,lib/%,$(cskernel)) lib/unidata
	$(CSC) /target:exe /out:run/Kernel.dll /lib:obj /unsafe+ \
	    /res:lib/unidata $(patsubst %,lib/%,$(cskernel))

.PHONY: Niecza_pm
perl5: obj/Perl5Interpreter.dll obj/p5embed.so Niecza_pm
Niecza_pm:
	cd perl5/Niecza;perl Build.PL;perl Build

obj/Perl5Interpreter.dll: obj/Run.Kernel.dll lib/Perl5Interpreter.cs
	$(CSC) /target:library /lib:obj /out:obj/Perl5Interpreter.dll /r:Run.Kernel.dll lib/Perl5Interpreter.cs

obj/p5embed.so: lib/p5embed.c
	perl perl5/build_interop


aot: all
	mono --aot run/*.dll obj/Run.CORE.dll run/Niecza.exe

test: all
	$(RUN_CLR) run/Niecza.exe --obj-dir obj -c test.pl
	$(CP) run/Kernel.dll obj/
	prove -e "$(RUN_CLR)" obj/MAIN.exe

spectest: all
	@t/run_spectests

precomp: all
	$(RUN_CLR) run/Niecza.exe --obj-dir obj -C $(precompunits)
install: all precomp
	mkdir -p $(INSTBIN) $(INSTEXE) $(INSTLIB) $(INSTOBJ)
	$(CP) run/*.dll run/*.ser run/MAIN.exe $(INSTEXE)
	$(CP) $(patsubst %,obj/%.ser,CORE $(precompunits)) $(patsubst %,obj/%.dll,CORE $(precompunits)) $(INSTOBJ)
	$(CP) lib/*.pm6 lib/*.setting $(INSTLIB)
	echo '#! /bin/sh' > $(INSTNIECZA)
	echo 'exec $(RUN_CLR) $(INSTEXE)/MAIN.exe "$$@"' >> $(INSTNIECZA)
	chmod +x $(INSTNIECZA)

clean:
	@rm -f obj/*.dll obj/*.exe obj/*.ser obj/*.so obj/*.dylib
	@rm -f run/*.exe run/*.dll run/*.so run/*.dylib
	@rm -fr *~

# uses the current niecza to set up a build area for the next stage
mkpackage:
	rm -rf package/
	mkdir package/ package/run/ package/lib/
	cp -a docs README.pod LICENSE package/
	cp -a run/Kernel.dll package/run
	$(RUN_CLR) run/Kernel.dll -regenerate run/ package/run/ Niecza
	cp lib/*.pm6 lib/*.setting package/lib/

mknext: run/Niecza.exe run/Kernel.dll
	rm -rf next/
	mkdir -p next next/boot next/obj next/run next/boot/run next/boot/obj/
	touch next/FETCH_URL next/.fetch-stamp
	cp -a src lib docs README.pod LICENSE Makefile test.pl next/
	cp -a run lib next/boot/
	ln -s ../t next/t
	NIECZA_KEEP_IL=1 $(RUN_CLR) next/boot/run/Niecza.exe --obj-dir next/run -C $(libunits)

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

# grep -r '^use' src/*.pm6 | sed 's|src/\(.*\)\.pm6:use \(.*\);|run/\1.ser: run/\2.ser|' | grep -v MONKEY_TYPING
run/NieczaActions.ser: run/OpHelpers.ser
run/NieczaFrontendSTD.ser: run/STD.ser
run/NieczaFrontendSTD.ser: run/NieczaGrammar.ser
run/NieczaGrammar.ser: run/STD.ser
run/Operator.ser: run/OpHelpers.ser
run/OptRxSimple.ser: run/RxOp.ser
