#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

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

  int i = 0, ri = 0;

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
    int *ap = LOGICAL(a);
    int *rp, *arp;
    for (; ri < nr; ri++)
    {
      r = SET_VECTOR_ELT(out, ri, Rf_allocVector(LGLSXP, rsize));
      rp = LOGICAL(r);
      arp = ap++;
      for (i = 0; i < rsize; i++)
      {
        *(rp++) = *arp;
        arp += nr;
      }
      // ATTRIB(r) =
      Rf_setAttrib(r, R_DimSymbol, rdim);
    }
    break;
  }
  case INTSXP:
  {
    int *ap = INTEGER(a);
    int *rp, *arp;
    for (; ri < nr; ri++)
    {
      r = SET_VECTOR_ELT(out, ri, Rf_allocVector(INTSXP, rsize));
      rp = INTEGER(r);
      arp = ap++;
      for (i = 0; i < rsize; i++)
      {
        *(rp++) = *arp;
        arp += nr;
      }
      Rf_setAttrib(r, R_DimSymbol, rdim);
    }
    break;
  }
  case REALSXP:
  {
    double *ap = REAL(a);
    double *rp, *arp;
    for (; ri < nr; ri++)
    {
      r = SET_VECTOR_ELT(out, ri, Rf_allocVector(REALSXP, rsize));
      rp = REAL(r);
      arp = ap++;
      for (i = 0; i < rsize; i++)
      {
        *(rp++) = *arp;
        arp += nr;
      }
      Rf_setAttrib(r, R_DimSymbol, rdim);
    }
    break;
  }
  case CPLXSXP:
  {
    Rcomplex *ap = COMPLEX(a);
    Rcomplex *rp, *arp;
    for (; ri < nr; ri++)
    {
      r = SET_VECTOR_ELT(out, ri, Rf_allocVector(CPLXSXP, rsize));
      rp = COMPLEX(r);
      arp = ap++;
      for (i = 0; i < rsize; i++)
      {
        *(rp++) = *arp;
        arp += nr;
      }
      Rf_setAttrib(r, R_DimSymbol, rdim);
    }
    break;
  }
  }
  UNPROTECT(2);
  return out;
}
