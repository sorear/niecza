### CONFIGURATION

# How to run CLR programs; can be blank for Win32
RUN_CLR=mono
CSC=gmcs
RM=rm -f
CP=cp

cskernel=Kernel.cs Builtins.cs Cursor.cs JSYNC.cs NieczaCLR.cs
csbackend=CLRBackend.cs

# keep this in dependency order
units=SAFE CORE CClass Body Unit CgOp Op Sig RxOp NAME Stash JSYNC STD \
      NieczaGrammar Metamodel OptRxSimple NAMOutput NieczaActions \
      NieczaFrontendSTD NieczaPassBegin NieczaPassBeta NieczaPassSimplifier \
      NieczaPathSearch NieczaBackendNAM NieczaBackendDotnet NieczaBackendClisp \
      NieczaCompiler

all: obj/Kernel.dll obj/CLRBackend.exe .fetch-stamp
	cd src && $(RUN_CLR) ../boot/run/Niecza.exe -I../lib -v -c -Bnam niecza
	for nfile in $(units); do echo $$nfile; \
	    $(RUN_CLR) boot/obj/CLRBackend.exe boot/obj $$nfile.nam $$nfile.dll 0; \
	done
	$(RUN_CLR) boot/obj/CLRBackend.exe boot/obj MAIN.nam MAIN.exe 1
	$(CP) $(patsubst %,boot/obj/%.dll,Kernel $(units)) run/
	$(CP) boot/obj/MAIN.exe run/Niecza.exe

.fetch-stamp: FETCH_URL
	-rm -rf boot/
	mkdir boot
	wget --no-check-certificate -Oboot/niecza.zip $$(cat FETCH_URL)
	cd boot && unzip niecza.zip
	touch .fetch-stamp

obj/Kernel.dll: $(patsubst %,lib/%,$(cskernel))
	$(CSC) /target:library /out:obj/Kernel.dll /unsafe+ \
	    $(patsubst %,lib/%,$(cskernel))
obj/CLRBackend.exe: $(patsubst %,lib/%,$(csbackend)) obj/Kernel.dll
	$(CSC) /target:exe /lib:obj /out:obj/CLRBackend.exe /r:Kernel.dll \
	    $(patsubst %,lib/%,$(csbackend))
