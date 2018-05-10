//// File Name: init.c
//// File Version: 2.011041
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _miceadds_create_interactions_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _miceadds_kernelplsaux(SEXP, SEXP, SEXP);
extern SEXP _miceadds_kernelpls_1dim_C(SEXP, SEXP, SEXP);
extern SEXP _miceadds_ma_pmm6_C(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _miceadds_scale2_C(SEXP);
extern SEXP _miceadds_scale2_NA_C(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_miceadds_create_interactions_cpp", (DL_FUNC) &_miceadds_create_interactions_cpp, 6},
    {"_miceadds_kernelplsaux", (DL_FUNC) &_miceadds_kernelplsaux, 3},
    {"_miceadds_kernelpls_1dim_C", (DL_FUNC) &_miceadds_kernelpls_1dim_C, 3},
    {"_miceadds_ma_pmm6_C", (DL_FUNC) &_miceadds_ma_pmm6_C, 6},
    {"_miceadds_scale2_C", (DL_FUNC) &_miceadds_scale2_C, 1},
    {"_miceadds_scale2_NA_C", (DL_FUNC) &_miceadds_scale2_NA_C, 1},
    {NULL, NULL, 0}
};

void R_init_miceadds(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
