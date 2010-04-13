#ifndef HsPerl5DefinedH
#define HsPerl5DefinedH 1

#define dirent DIRENT
#define _INTPTR_T_DEFINED
#define _UINTPTR_T_DEFINED
#undef RETURN

#if defined(__OpenBSD__)
#define _P5EMBED_INIT _p5embed_init
#else
#define _P5EMBED_INIT __init
#endif

#include "EXTERN.h"
#include "perl.h"
#include "embed.h"

/*
PerlInterpreter * perl5_init ( int argc, char **argv );
bool perl5_SvROK(SV *inv);
bool perl5_can(SV *inv, char *subname);
*/
SV * perl5_sv_undef ();
SV * perl5_sv_yes ();
SV * perl5_sv_no ();
SV ** perl5_eval(char *code, int len, int cxt);
char * perl5_SvPV ( SV * sv );
SV * perl5_newSVpvn ( char * pv, int len );
SV ** perl5_return_conv (int count);
SV ** perl5_apply(SV *sub, SV *inv, SV** args, int cxt);
SV * perl5_newSViv ( int iv );
int perl5_SvIV ( SV * sv );
double perl5_SvNV ( SV * sv );
SV * perl5_newSVnv ( double iv );
bool perl5_SvTRUE ( SV * sv );
SV * perl5_get_sv ( const char *name );
SV * perl5_get_cv ( const char *name );


#include <HsFFI.h>
extern SV ** hsPerl5Apply ( HsStablePtr *sub, SV **args, int cxt );
SV * perl5_make_cv ( HsStablePtr *sub );

#endif
