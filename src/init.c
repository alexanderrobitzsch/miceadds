#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP miceadds_create_interactions_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP miceadds_kernelpls_1dim_C(SEXP, SEXP, SEXP);
extern SEXP miceadds_kernelplsaux(SEXP, SEXP, SEXP);
extern SEXP miceadds_ma_pmm6_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP miceadds_scale2_C(SEXP);
extern SEXP miceadds_scale2_NA_C(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"miceadds_create_interactions_cpp", (DL_FUNC) &miceadds_create_interactions_cpp, 6},
    {"miceadds_kernelpls_1dim_C",        (DL_FUNC) &miceadds_kernelpls_1dim_C,        3},
    {"miceadds_kernelplsaux",            (DL_FUNC) &miceadds_kernelplsaux,            3},
    {"miceadds_ma_pmm6_C",               (DL_FUNC) &miceadds_ma_pmm6_C,               6},
    {"miceadds_scale2_C",                (DL_FUNC) &miceadds_scale2_C,                1},
    {"miceadds_scale2_NA_C",             (DL_FUNC) &miceadds_scale2_NA_C,             1},
    {NULL, NULL, 0}
};

void R_init_miceadds(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
