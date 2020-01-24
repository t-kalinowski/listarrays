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
      srp += ROWSIZE;                                                          \
    }                                                                          \
    Rf_setAttrib(r, R_DimSymbol, rdim);                                        \
  }




#define STRIDED_MEMCPY(SOURCEPTR, DESTPTR, N, STRIDESIZE) \
  for (int n = N; n != 0; n--)                            \
  {                                                       \
    *(DESTPTR++) = *SOURCEPTR;                            \
    SOURCEPTR += STRIDESIZE;                              \
  }                                                       \

SEXP listarrays_split_along_rows(SEXP a, SEXP drop)
{

  R_xlen_t asize = XLENGTH(a);
  SEXP adim = Rf_getAttrib(a, R_DimSymbol);
  int *adimp = INTEGER(adim);
  int nr = *adimp;
  int rsize = asize / nr;

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));
  SEXP r, rdim;
  int doDrop;
  if (drop == R_NilValue)
    doDrop = 1;
  else
    doDrop = Rf_asLogical(drop);

  // int i = 0, ri = 0;

  R_xlen_t nadim = XLENGTH(adim);
  int nrdim = 0, adim_idx = 0;


  if (doDrop)
  {
    adimp++;
    adim_idx++;

    for (; adim_idx < nadim; adim_idx++, adimp++)
    {
      if (*adimp != 1)
        nrdim++;
    }

    if (nrdim == 1)
    {
      PROTECT(rdim = R_NilValue);
    }
    else
    {

      rdim = PROTECT(Rf_allocVector(INTSXP, nrdim));
      int *rdimp = INTEGER(rdim);

      int tmpd;
      adimp = INTEGER(adim) + 1;
      for (int i = doDrop; i < nadim; i++)
      {
        tmpd = *(adimp++);
        if (tmpd == 1)
          continue;
        *(rdimp++) = tmpd;
      }
    }
  }
  else
  {
    rdim = PROTECT(Rf_allocVector(INTSXP, nadim));
    int *rdimp = INTEGER(rdim);
    *rdimp = 1;
    for (adim_idx = 1; adim_idx < nadim; adim_idx++)
      *(++rdimp) = *(++adimp);
  }

  MARK_NOT_MUTABLE(rdim);

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
