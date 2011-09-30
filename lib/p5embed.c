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
void p5embed_initialize()
{
  PERL_SYS_INIT3(0,NULL,NULL);
  PerlInterpreter* my_perl = perl_alloc();
  PERL_SET_CONTEXT(my_perl);
  perl_construct(my_perl);
  char *embedding[] = { "", "-e", "0" };
  perl_parse(my_perl, xs_init, 3, embedding, NULL);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
  eval_pv("use lib 'perl5';use Niecza::Interoperability",TRUE);
}

SV* p5embed_eval(char* code) { 
  return eval_pv(code,TRUE);
}

void p5embed_dispose()
{
  perl_destruct(my_perl);
  perl_free(my_perl);
  PERL_SYS_TERM();
}

void p5method_call(char* name,int* args,int n) {
  dSP;


  /*ENTER;
  SAVETMPS;*/

  PUSHMARK(SP);
  int i;
  for (i=0;i<n;i++) {
    XPUSHs(args[i]);
  }
  PUTBACK;

  call_method(name,G_DISCARD);

  /*FREETMPS;
  LEAVE;*/
}
