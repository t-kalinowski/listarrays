#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#define SPLIT_ALONG_ROWS(SOURCE, DESTVEC, RTYPE, CTYPE, DEREF, NROWS, ROWSIZE) \
  CTYPE *rp, *srp, *sp = DEREF(SOURCE);                                        \
  int i = 0, j;                                                                \
  while (i < NROWS)                                                            \
  {                                                                            \
    r = SET_VECTOR_ELT(DESTVEC, i++, Rf_allocVector(RTYPE, ROWSIZE));          \
    rp = DEREF(r);                                                             \
    srp = sp++;                                                                \
    for (j = ROWSIZE; j != 0; j--)                                             \
    {                                                                          \
      *(rp++) = *srp;                                                          \
      srp += NROWS;                                                            \
    }                                                                          \
    Rf_setAttrib(r, R_DimSymbol, rdim);                                        \
  }

SEXP listarrays_split_along_rows(SEXP a)
{

  R_xlen_t asize = XLENGTH(a);
  SEXP adim = Rf_getAttrib(a, R_DimSymbol);
  int *adimp = INTEGER(adim);
  int nr = *(adimp++);
  int rsize = asize / nr;

    // basically cast to as.list()
  if (rsize == 1)
    return Rf_coerceVector(a, VECSXP);

  R_xlen_t len_adim = XLENGTH(adim), len_rdim = 0;
  for (int adim_idx = 1; adim_idx < len_adim; adim_idx++)
  {
    if (*(adimp++) != 1)
      len_rdim++;
  }

  SEXP r, rdim;

  if (len_rdim == 1)
    PROTECT(rdim = R_NilValue);
  else
  {

    rdim = PROTECT(Rf_allocVector(INTSXP, len_rdim));
    int *rdimp = INTEGER(rdim);
    int d;
    adimp = INTEGER(adim) + 1;
    for (int i = 1; i < len_adim; i++)
    {
      d = *(adimp++);
      if (d != 1)
        *(rdimp++) = d;
    }
    MARK_NOT_MUTABLE(rdim);
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));

  switch (TYPEOF(a))
  {
  case LGLSXP:
  {
    SPLIT_ALONG_ROWS(a, out, LGLSXP, int, LOGICAL, nr, rsize);
    break;
  }
  case INTSXP:
  {
    SPLIT_ALONG_ROWS(a, out, INTSXP, int, INTEGER, nr, rsize);
    break;
  }
  case REALSXP:
  {
    SPLIT_ALONG_ROWS(a, out, REALSXP, double, REAL, nr, rsize);
    break;
  }
  case CPLXSXP:
  {
    SPLIT_ALONG_ROWS(a, out, CPLXSXP, Rcomplex, COMPLEX, nr, rsize);
    break;
  }
  }
  UNPROTECT(2);
  return out;
}
