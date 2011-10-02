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


SV* p5method_call(char* name,SV** args,int args_count) {
  dSP;


  PUSHMARK(SP);
  int i;
  for (i=0;i<args_count;i++) {
    XPUSHs(args[i]);
  }
  PUTBACK;


  int count = call_method(name,G_SCALAR);
  SPAGAIN;
  if (count != 1) croak("Big trouble\n");

  SV* ret = POPs;

  /* TODO should i do that? */
  SvREFCNT_inc(ret);

  PUTBACK;

  return ret;

}

SV* p5embed_subcall(SV** args,int args_count) {
  dSP;


  PUSHMARK(SP);
  int i;
  for (i=1;i<args_count;i++) {
    XPUSHs(args[i]);
  }
  PUTBACK;


  int count = call_sv(args[0],G_SCALAR);
  SPAGAIN;
  if (count != 1) croak("Big trouble\n");

  SV* ret = POPs;

  /* TODO should i do that? */
  SvREFCNT_inc(ret);

  PUTBACK;

  return ret;

}

int p5embed_SvIOKp(SV* sv) {
    return SvIOKp(sv);
}
int p5embed_SvNOKp(SV* sv) {
    return SvNOKp(sv);
}
int p5embed_SvPOKp(SV* sv) {
    return SvPOKp(sv);
}

int p5embed_SvIV(SV* sv) {
    return SvIV(sv);
}
double p5embed_SvNV(SV* sv) {
    return SvNV(sv);
}
char* p5embed_SvPV_nolen(SV* sv) {
    return SvPV_nolen(sv);
}
SV* p5embed_newSVpvn(char* str,int len) {
    return newSVpvn(str,len);
}
