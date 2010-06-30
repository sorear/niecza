all:
	perl Compiler.pm > Program.cs
	gmcs /target:exe Kernel.cs Program.cs
