### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs /debug+
RM=rm -f
CP=cp
WGET_O=wget --no-check-certificate -O
#WGET_O=curl -o

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs Utils.cs \
	 ObjModel.cs BigInteger.cs Printf.cs CodeGen.cs \
	 GeneratedTrigFunctions.cs Serialize.cs UCD.cs

# Tell make to regard the following targets as not being filenames
.PHONY: all aot test spectest clean realclean
.PHONY: help

libunits=CORE JSYNC
srcunits=CClass CgOp Op OpHelpers Sig RxOp STD NieczaGrammar OptRxSimple \
	 Operator NieczaActions NieczaFrontendSTD NieczaPassSimplifier \
	 OptBeta NieczaPathSearch NieczaBackendDotnet NieczaCompiler GetOptLong

all: run/Niecza.exe obj/Run.Kernel.dll obj/Run.CORE.dll

$(patsubst %,boot/obj/Run.%.ser,$(srcunits)): boot/obj/Run.%.ser: .fetch-stamp src/%.pm6 boot/obj/Run.CORE.ser
	cd src && NIECZA_KEEP_IL=1 $(RUN_CLR) ../boot/run/Niecza.exe -C $*

# hack - put VERSION info in place so the setting build can embed it
obj/Run.CORE.dll: run/Niecza.exe obj/Run.Kernel.dll lib/CORE.setting
	@git describe --tags > VERSION
	$(RUN_CLR) run/Niecza.exe -C CORE

run/Niecza.exe: .fetch-stamp $(patsubst %,boot/obj/Run.%.ser,$(srcunits)) src/niecza
	cd src && NIECZA_KEEP_IL=1 $(RUN_CLR) ../boot/run/Niecza.exe -c niecza
	$(CP) boot/obj/Kernel.dll run/
	$(CSC) /target:library /out:run/CompilerBlob.dll /r:Kernel \
	    /lib:run src/CompilerBlob.cs
	$(RUN_CLR) run/Kernel.dll -gen-app Niecza boot/obj

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	$(WGET_O) boot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
	NIECZA_KEEP_IL=1 $(RUN_CLR) boot/run/Niecza.exe -C $(libunits)
	$(CP) boot/run/Kernel.dll boot/obj/
	touch .fetch-stamp

boot/obj/Run.CompilerBlob.dll: .fetch-stamp src/CompilerBlob.cs
	$(CSC) /target:library /out:boot/obj/Run.CompilerBlob.dll /r:Run.Kernel \
	    /lib:boot/obj src/CompilerBlob.cs
obj/Run.Kernel.dll: $(patsubst %,lib/%,$(cskernel)) lib/unidata
	$(CSC) /target:exe /out:obj/Run.Kernel.dll /lib:obj /unsafe+ \
	    /res:lib/unidata $(patsubst %,lib/%,$(cskernel))
obj/Kernel.dll: $(patsubst %,lib/%,$(cskernel)) lib/unidata
	$(CSC) /target:exe /out:obj/Kernel.dll /lib:obj /unsafe+ \
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
	$(RUN_CLR) run/Niecza.exe -c test.pl
	prove -e "$(RUN_CLR)" obj/Run.MAIN.exe

spectest: all
	@t/run_spectests

clean:
	@rm -f obj/*.dll obj/*.exe obj/*.nam obj/*.so
	@rm -f run/Niecza.exe
	@rm -f run/*.dll
	@rm -f run/*.dll.so
	@rm -fr *~

# uses the current niecza to set up a build area for the next stage
mkpackage:
	rm -rf package/
	mkdir package/ package/run/ package/lib/ package/obj/
	cp -a docs README.pod LICENSE package/
	cp -a run/Niecza.exe run/Niecza.ser run/Kernel.dll \
	    run/CompilerBlob.dll package/run/
	cp lib/*.pm6 lib/*.setting package/lib/
	cp obj/Run.Kernel.dll package/obj/

mknext: run/Niecza.exe obj/Run.Kernel.dll obj/Kernel.dll
	rm -rf next/
	mkdir -p next next/boot next/obj next/run next/boot next/boot/obj/
	touch next/FETCH_URL next/.fetch-stamp
	cp -a src lib docs README.pod LICENSE Makefile test.pl next/
	cp -a run lib next/boot/
	cp obj/Run.Kernel.dll obj/Kernel.dll next/boot/obj/
	NIECZA_KEEP_IL=1 $(RUN_CLR) next/boot/run/Niecza.exe -C $(libunits)

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

boot/obj/Run.NieczaBackendDotnet.ser: boot/obj/Run.CompilerBlob.dll

# grep -r '^use' src/*.pm6 | sed 's|src/\(.*\)\.pm6:use \(.*\);|boot/obj/Run.\1.ser: boot/obj/Run.\2.ser|' | grep -v MONKEY_TYPING
boot/obj/Run.NieczaActions.ser: boot/obj/Run.OpHelpers.ser
boot/obj/Run.NieczaFrontendSTD.ser: boot/obj/Run.STD.ser
boot/obj/Run.NieczaFrontendSTD.ser: boot/obj/Run.NieczaGrammar.ser
boot/obj/Run.NieczaGrammar.ser: boot/obj/Run.STD.ser
boot/obj/Run.Operator.ser: boot/obj/Run.OpHelpers.ser
boot/obj/Run.OptRxSimple.ser: boot/obj/Run.RxOp.ser
