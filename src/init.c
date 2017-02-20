#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP tau_copyToNgram(SEXP x, SEXP R_n);
SEXP tau_copyTruncate(SEXP x, SEXP R_n);
SEXP tau_fixEncoding(SEXP x, SEXP R_latin1);
SEXP tau_isASCII(SEXP x);
SEXP tau_isLocale(SEXP x);
SEXP tau_isUTF8(SEXP x);
SEXP tau_removeBlank(SEXP x);
SEXP tau_translateToLocale(SEXP x);
SEXP tau_utf8CountNgram(SEXP x, SEXP R_n, SEXP R_lower, SEXP R_verbose,
			SEXP R_persistent, SEXP R_useBytes);
SEXP tau_utf8CountString(SEXP x, SEXP R_n, SEXP R_lower, SEXP R_type,
			 SEXP R_verbose, SEXP R_persistent, SEXP R_useBytes);

static const R_CallMethodDef CallEntries[] = {
    {"tau_copyToNgram", (DL_FUNC) &tau_copyToNgram, 2},
    {"tau_copyTruncate", (DL_FUNC) &tau_copyTruncate, 2},
    {"tau_fixEncoding", (DL_FUNC) &tau_fixEncoding, 2},
    {"tau_isASCII", (DL_FUNC) &tau_isASCII, 1},
    {"tau_isLocale", (DL_FUNC) &tau_isLocale, 1},
    {"tau_isUTF8", (DL_FUNC) &tau_isUTF8, 1},
    {"tau_removeBlank", (DL_FUNC) &tau_removeBlank, 1},
    {"tau_translateToLocale", (DL_FUNC) &tau_translateToLocale, 1},
    {"tau_utf8CountNgram", (DL_FUNC) &tau_utf8CountNgram, 6},
    {"tau_utf8CountString", (DL_FUNC) &tau_utf8CountString, 7},
    {NULL, NULL, 0}
};

void R_init_tau(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
