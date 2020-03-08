#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP listarrays_split_along_rows(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"listarrays_split_along_rows", (DL_FUNC) &listarrays_split_along_rows, 2},
  {NULL, NULL, 0}
};

void R_init_listarrays(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
