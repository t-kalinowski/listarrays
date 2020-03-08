#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP extract_1row(SEXP);
extern SEXP listarrays_split_along_rows(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"extract_1row",                (DL_FUNC) &extract_1row,                1},
    {"listarrays_split_along_rows", (DL_FUNC) &listarrays_split_along_rows, 1},
    {NULL, NULL, 0}
};

void R_init_listarrays(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}