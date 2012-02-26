#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

extern int (*p5embed_create_LoS)(int,SV**);

MODULE = Niecza		PACKAGE = Niecza		

SV*
create_LoS(SV* arg)
  CODE:
    AV* array = SvRV(arg);
    int len = av_len(array)+1;
    int i = 0;
    SV** svs = malloc(sizeof(SV*) * len);
    for (i=0;i<len;i++) {
        SV** ptr = av_fetch(array,i,0);
        svs[i] = *ptr;
    }
    int LoS = p5embed_create_LoS(len,svs);
    SV* pointer = newSViv(PTR2IV(LoS));
    SV* object = newRV_noinc(pointer);
    HV* class = gv_stashpv("Niecza::Object", 0);
    sv_bless(object, class);
    RETVAL = object;
  OUTPUT:
    RETVAL
