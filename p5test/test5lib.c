#include <stdio.h>
#include <EXTERN.h>
#include <perl.h>

/* So that we can load XS using modules from our perl */
EXTERN_C void xs_init (pTHX);

EXTERN_C void boot_DynaLoader (pTHX_ CV* cv);

EXTERN_C void
xs_init(pTHX)
{
	char *file = __FILE__;
	dXSUB_SYS;

	/* DynaLoader is a special case */
	newXS("DynaLoader::boot_DynaLoader", boot_DynaLoader, file);
}

static PerlInterpreter *my_perl;
int run_test()
{
  int argc = 1;
  char *argv0[] = { "perl", NULL, 0 };
  char **argv = argv0;
  PERL_SYS_INIT(&argc,&argv);
  my_perl = perl_alloc();
  perl_construct(my_perl);
  char *embedding[] = { "", "-e", "0" };
  perl_parse(my_perl, xs_init, 3, embedding, NULL);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
  eval_pv("print \"OK 5\\n\"",TRUE);
  perl_destruct(my_perl);
  perl_free(my_perl);
  PERL_SYS_TERM();
}
