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
  int argc = 1;
  char *argv0[] = { "perl", NULL, 0 };
  char **argv = argv0;
  PERL_SYS_INIT(&argc,&argv);
  my_perl = perl_alloc();
  perl_construct(my_perl);
  char *embedding[] = { "", "-e", "0" };
  perl_parse(my_perl, xs_init, 3, embedding, NULL);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
//  eval_pv("use lib 'perl5';use Niecza::Interoperability",TRUE);
}

SV* p5embed_eval(char* code) { 
  eval_pv("1",TRUE);
  SV* ret = eval_pv(code,TRUE);
  return ret;
}

void p5embed_dispose()
{
  /* Disable temorarly as it seems to cause segfaults */
  /* 
  perl_destruct(my_perl);
  perl_free(my_perl);
  PERL_SYS_TERM();
  */
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

SV* p5embed_subcall(int context,SV** args,int args_count) {
  dSP;


  PUSHMARK(SP);
  int i;
  for (i=1;i<args_count;i++) {
    XPUSHs(args[i]);
  }
  PUTBACK;


  /* HACK - list context is NYI */
  if (context == 1 || context == 0) {
    int count = call_sv(args[0],G_SCALAR);
    SPAGAIN;
    if (count != 1) croak("Big trouble\n");

    SV* ret = POPs;

    /* TODO should i do that? */
    SvREFCNT_inc(ret);

    PUTBACK;
    return ret;
  } else if (context == 2) {
    call_sv(args[0],G_VOID);
  }

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
