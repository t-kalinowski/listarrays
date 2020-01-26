#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP split_along_rows_default(SEXP obj)
{

  R_len_t ndim, nr;
  SEXP cl = R_NilValue;
  SEXP rho = R_GetCurrentEnv();
  SEXP rowidx = PROTECT(Rf_ScalarInteger(1));
  int *prowidx = INTEGER0(rowidx);

  if (Rf_isFrame(obj))
  {
    ndim = 2;
    nr = Rf_nrows(VECTOR_ELT(obj, 0));
  }
  else
  {
    SEXP dim = Rf_getAttrib(obj, R_DimSymbol);
    ndim = XLENGTH(dim);
    nr = Rf_nrows(obj);
  }

  for (ndim--; ndim != 0; ndim--)
    cl = Rf_cons(R_MissingArg, cl);

  cl = Rf_cons(rowidx, cl);
  cl = Rf_cons(obj, cl);
  cl = Rf_lcons(Rf_install("["), cl);

  // cl = Rf_lcons(Rf_findFun(install("["), rho), cl);
  // Rf_ExtractSubset
  // Rf_fixSubset3Args

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));
  for (int i = 0; i < nr; i++, (*prowidx)++)
    SET_VECTOR_ELT(out, i, Rf_eval(cl, rho));

  UNPROTECT(2);
  return out;
}

#define SPLIT_ALONG_ROWS(SOURCE, DESTVEC, RTYPE, CTYPE, DEREF, NROWS, ROWSIZE) \
  CTYPE *rp, *srp, *sp = DEREF(SOURCE);                                        \
  int i = 0, j;                                                                \
  while (i < NROWS)                                                            \
  {                                                                            \
    r = SET_VECTOR_ELT(DESTVEC, i++, Rf_allocVector(RTYPE, ROWSIZE));          \
    rp = DEREF(r);                                                             \
    if (rdim != R_NilValue)                                                    \
      Rf_dimgets(r, rdim);                                                     \
    srp = sp++;                                                                \
    for (j = ROWSIZE; j != 0; j--, srp += NROWS)                               \
      *(rp++) = *srp;                                                          \
  }

SEXP listarrays_split_along_rows(SEXP a)
{

  SEXP adim = Rf_getAttrib(a, R_DimSymbol);
  if (adim == R_NilValue)
  {
    if (Rf_isFrame(a))
      return split_along_rows_default(a);
    else
      return Rf_coerceVector(a, VECSXP);
  }

  if (Rf_getAttrib(a, R_DimNamesSymbol) != R_NilValue)
    return split_along_rows_default(a);

  R_xlen_t asize = XLENGTH(a);
  int *adimp = INTEGER(adim);
  int nr = *(adimp++);
  int rsize = asize / nr;

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

  // SEXP ratrib = CONS(rdim, R_NilValue);
  // SEXP ratrib;
  // SET_TAG(ratrib, R_DimSymbol);
  // MARK_NOT_MUTABLE(ratrib);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));

  switch (TYPEOF(a))
  {
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
  case LGLSXP:
  {
    SPLIT_ALONG_ROWS(a, out, LGLSXP, int, LOGICAL, nr, rsize);
    break;
  }
  case VECSXP:
  {
  }
  default:
  {
    UNPROTECT(2);
    return split_along_rows_default(a);
  }
  }
  UNPROTECT(2);
  return out;
}
