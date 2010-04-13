#ifndef HsPerl5DefinedC
#define HsPerl5DefinedC 1

#include "p5embed.h"
#include <XSUB.h>
#include "perlxsi.c"

/* define to enable pugsembed debug messages */
#define PERL5_EMBED_DEBUG 0

#if PERL5_EMBED_DEBUG
#define oRZ ""
#define hate Perl_croak(aTHX_ "hate software")
#else
#define oRZ "#"
#define hate
#endif

const char HsPerl5Preamble[] =
"package __HsPerl5__;\n\n"
"sub MkCode {\n"
"    my $val = shift;\n"
"    sub { unshift @_, $val; goto &__HsPerl5__::Invoke }\n"
"}\n"
"1;\n";

/* Workaround for mapstart: the only op which needs a different ppaddr */
#undef Perl_pp_mapstart
#define Perl_pp_mapstart Perl_pp_grepstart
#undef OP_MAPSTART
#define OP_MAPSTART OP_GREPSTART

static PerlInterpreter *my_perl;
int _P5EMBED_INIT = 0;

SV *
perl5_sv_undef ()
{
    return(&PL_sv_undef);
}

SV *
perl5_sv_yes ()
{
    return(&PL_sv_yes);
}

SV *
perl5_sv_no ()
{
    return(&PL_sv_no);
}

SV **
perl5_eval(char *code, int len, int cxt)
{
    dSP;
    SV* sv;
    int count;

    ENTER;
    SAVETMPS;

    sv = newSVpvn(code, len);
#ifdef SvUTF8_on
    SvUTF8_on(sv);
#endif
    count = eval_sv(sv, cxt);
    SvREFCNT_dec(sv);

    return perl5_return_conv(count);
}


SV **
perl5_return_conv (int count) {
    SV **out;
    int i;

    dSP;
    SPAGAIN;

    if (SvTRUE(ERRSV)) {
        Newz(42, out, 3, SV*);
        if (SvROK(ERRSV)) {
            out[0] = newSVsv(ERRSV);
            out[1] = NULL;
        }
        else {
            out[0] = ERRSV;
            out[1] = ERRSV; /* for Haskell-side to read PV */
        }
        out[2] = NULL;
    }
    else {
        Newz(42, out, count+2, SV*);

        out[0] = NULL;

        for (i=count; i>0; --i) {
            out[i] = newSVsv(POPs);
        }
        out[count+1] = NULL;
    }

    PUTBACK;
    FREETMPS;
    LEAVE;

    /* pugs_setenv(old_env); */
    return out;
}

char *
perl5_SvPV ( SV *sv )
{
    char *rv;
    rv = SvPV_nolen(sv);
    return rv;
}

SV *
perl5_newSVpvn ( char * pv, int len )
{
    SV *sv = newSVpvn(pv, len);
#ifdef SvUTF8_on
    SvUTF8_on(sv);
#endif
    return(sv);
}

SV **
perl5_apply(SV *sub, SV *inv, SV** args, int cxt)
{
    SV **arg;
    SV *rv;
    SV *sv;

    dSP;

    ENTER;
    SAVETMPS;

    PUSHMARK(SP);
    if (inv != NULL) {
        XPUSHs(inv);
    }
    for (arg = args; *arg != NULL; arg++) {
        XPUSHs(*arg);
    }
    PUTBACK;

    if (inv != NULL) {
        perl5_return_conv(call_method(SvPV_nolen(sub), cxt|G_EVAL));
    }
    else {
        perl5_return_conv(call_sv(sub, cxt|G_EVAL));
    }
}

SV *
perl5_newSViv ( int iv )
{
    return(newSViv(iv));
}

SV *
perl5_newSVnv ( double iv )
{
    return(newSVnv(iv));
}

int
perl5_SvIV ( SV *sv )
{
    return((int)SvIV(sv));
}

double
perl5_SvNV ( SV *sv )
{
    return((double)SvNV(sv));
}

bool
perl5_SvTRUE ( SV * sv )
{
    bool rv;
    rv = SvTRUE(sv);
    return(rv ? 1 : 0);
}

SV *
perl5_make_cv ( HsStablePtr *sub )
{
    SV *sv = newSV(0);
    SV *ret = NULL;
    int count;
    sv_setref_pv(sv, "__HsPerl5__::Code", sub);
    dSP;

    ENTER;
    SAVETMPS;
    PUSHMARK(SP);
    XPUSHs(sv_2mortal(sv));
    PUTBACK;

    count = call_pv("__HsPerl5__::MkCode", G_SCALAR);

    if (count != 1) {
        croak("Big trouble\n");
    }

    SPAGAIN;
    ret = newSVsv(POPs);

    PUTBACK;
    FREETMPS;
    LEAVE;

    return (ret);
}

XS(__HsPerl5__Invoke) {
    HsStablePtr *sub;
    SV **stack;
    SV **ret;
    SV **cur;
    SV *sv;
    int i;

    IV tmp = 0;
            
    dXSARGS;

    if (items < 1) {
      hate;
    }

    sv = ST(0);

    tmp = SvIV((SV*)SvRV(sv));
    sub = (HsStablePtr *)tmp;

    New(6, stack, items, SV*);

    for (i = 1; i < items; ++i) {
        stack[i-1] = ST(i);
    }
    stack[i-1] = NULL;
    Safefree(stack);
    
    SPAGAIN;

    ret = hsPerl5Apply(sub, stack, GIMME_V);
    if (ret == NULL) {
        XSRETURN(0);
    }

    for (cur = ret; *cur != NULL; ++cur) {
        EXTEND(SP, 1);
        ST(cur - ret) = *cur;
    }
    Safefree(ret);

    XSRETURN(cur - ret);
}

PerlInterpreter *
perl5_init ( int argc, char **argv )
{
    int exitstatus;
    int i;

#ifdef PERL_GPROF_MONCONTROL
    PERL_GPROF_MONCONTROL(0);
#endif

#if (defined(USE_5005THREADS) || defined(USE_ITHREADS)) && defined(HAS_PTHREAD_ATFORK)
    /* XXX Ideally, this should really be happening in perl_alloc() or
     * perl_construct() to keep libperl.a transparently fork()-safe.
     * It is currently done here only because Apache/mod_perl have
     * problems due to lack of a call to cancel pthread_atfork()
     * handlers when shared objects that contain the handlers may
     * be dlclose()d.  This forces applications that embed perl to
     * call PTHREAD_ATFORK() explicitly, but if and only if it hasn't
     * been called at least once before in the current process.
     * --GSAR 2001-07-20 */
    PTHREAD_ATFORK(Perl_atfork_lock,
                   Perl_atfork_unlock,
                   Perl_atfork_unlock);
#endif

    if (!PL_do_undump) {
        my_perl = perl_alloc();
        if (!my_perl)
            exit(1);
        perl_construct( my_perl );
        PL_perl_destruct_level = 0;
    }
#ifdef PERL_EXIT_DESTRUCT_END
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
#endif /* PERL_EXIT_DESTRUCT_END */
#ifdef PERL_EXIT_EXPECTED
    PL_exit_flags |= PERL_EXIT_EXPECTED;
#endif /* PERL_EXIT_EXPECTED */

#if (defined(CSH) && defined(PL_cshname))
    if (!PL_cshlen)
      PL_cshlen = strlen(PL_cshname);
#endif

    exitstatus = perl_parse(my_perl, xs_init, argc, argv, (char **)NULL);

    if (exitstatus == 0)
        exitstatus = perl_run( my_perl );

    _P5EMBED_INIT = 1;

    newXS((char*) "__HsPerl5__::Invoke", __HsPerl5__Invoke, (char*)__FILE__);
    /*
    newXS((char*) "pugs::guts::eval_apply", _pugs_guts_eval_apply, (char*)__FILE__);
    */

#if PERL5_EMBED_DEBUG
    fprintf(stderr, "(%s)", pugs_guts_code);
#endif
    eval_pv(HsPerl5Preamble, TRUE);

    if (SvTRUE(ERRSV)) {
        STRLEN n_a;
        printf("Error init perl: %s\n", SvPV(ERRSV,n_a));
        exit(1);
    }
    return my_perl;
}

SV *
perl5_get_sv(const char *name)
{
    SV *sv = get_sv(name, 1);
    return sv;
}

SV *
perl5_get_cv(const char *name)
{
    SV *cv = (SV*)(get_cv(name, 0));
    return cv;
}


#endif
