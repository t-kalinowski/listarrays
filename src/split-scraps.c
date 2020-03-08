#define R_NO_REMAP
// #define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>

// SEXP split_array(SEXP a)
// {
//  // WORKING VERSION WITH ints only
//   R_xlen_t asize = XLENGTH(a);
//   int nr = INTEGER(Rf_getAttrib(a, R_DimSymbol))[0];
//   int rsize = asize / nr;

//   SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));
//   SEXP r;

//   int *rp;
//   int *ap = INTEGER(a);

//   for (int ri = 0; ri < nr; ri++)
//   {
//     r = SET_VECTOR_ELT(out, ri, Rf_allocVector(TYPEOF(a), rsize));
//     rp = INTEGER(r);
//     int *ai = (ap++);
//     for (int i = 0; i < rsize; i++, ai += nr)
//     {
//       *(rp++) = *ai;
//     }
//   }

//   UNPROTECT(1);
//   return out;
// }

/*
 rinternals <- file.path(R.home("include"), "Rinternals.h")
 file.edit(rinternals)
 r.h <- file.path(R.home("include"), "R.h")
 file.edit(r.h)

 */

/* Array Subscripts.
  dim is the dimension (0 to k-1)
  s is the subscript list,
  dims is the dimensions of x
  dng is a function (usually getAttrib) that obtains the dimnames
  x is the array to be subscripted.

 attribute_hidden SEXP
  int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call)
  {

SEXP split_array(SEXP m, SEXP nr) {
  SEXP result = PROTECT(Rf_allocVector(VECSXP, Rf_asInteger(nr)));
  //result[0] = asReal(a) + asReal(b);
  result =

  UNPROTECT(1);
  return result;
}
  */

// SEXP split_array(SEXP a)
// {

//   int l = LENGTH(a)
//   SEXP dim = Rf_getAttrib(a, R_DimSymbol);
//   R_len_t n = Rf_length(dim);

//   int nd = Rf_length(dim);
//   int *dp = INTEGER(dim);
//   int nr = *dp;
//   int rsize = 1;
//   for (int i = 1; i < nd; i++)
//   {
//     rsize *= *(++dp);
//   }

//   SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));
//   SEXP r;

//   int *rp;
//   int *ap = INTEGER(a);
//   for (int i = 0; i < nr; i++)
//   {
//     r = SET_VECTOR_ELT(out, i, Rf_allocVector(TYPEOF(a), rsize));
//     rp = INTEGER(r);
//     int *ai = (ap++);
//     for (int ri = 0; ri < rsize; ri++, ai += nr)
//     {
//       *(rp++) = *ai;
//     }
//   }

//   UNPROTECT(1);
//   return out;
// }

// printf("%i\n", stride);

// int nr = 3;
// int stride = 3;

// printf(stride);
// int nr = INTEGER(dim)[0];
// int nc = 3;

// SEXP split_array(SEXP m, SEXP dim)
// {
//   // SEXP vec = PROTECT(Rf_allocVector(VECSXP, Rf_asInteger(nr)));
//   // SEXP dbls = PROTECT(Rf_allocVector(REALSXP, 4));
//   // SEXP lgls = PROTECT(Rf_allocVector(LGLSXP, 4));
//   // SEXP ints = PROTECT(Rf_allocVector(INTSXP, 4));

//   // SEXP vec = PROTECT(Rf_allocVector(VECSXP, 3));
//   // SET_VECTOR_ELT(vec, 0, dbls);
//   // SET_VECTOR_ELT(vec, 1, lgls);
//   // SET_VECTOR_ELT(vec, 2, ints);

//   // UNPROTECT(4);
//   //vec = Rf_arraySubscript(asInteger(dim), s, dims, x, call)
//   //PROTECT(s = arraySubscript(1, s, getAttrib(x, install("Dim")), getAttrib, (STRING_ELT), x));
//   //from https://github.com/mhahsler/arules

//   //  SEXP vec = PROTECT(Rf_allocVector(VECSXP, Rf_asInteger(nr)));
//   //  Rf_dimgets()
//   //return vec;

//   int nr = INTEGER(dim)[0];
//   int nc = 3;
//   SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));

//   // int n = Rf_length(a);
//   // double *px, *pout;

//   //SEXP out = PROTECT(allocVector(REALSXP, n));

//   // px = REAL(x);
//   // pout = REAL(out);
//   SEXP r;
//   int* mp = INTEGER(m);
//   for (int i = 0; i < 3; i++) {
//     r = SET_VECTOR_ELT(out, i, Rf_allocVector(TYPEOF(m), nc));
//     int* pr = INTEGER(r);
//     for(int ri = 0, oi=i; i < 3; ri++, oi = oi+3) {
//       pr[ri] = mp[oi];
//     }

//   }
//   //pout[i] = px[i] + 2;
//   // UNPROTECT(1);

//   UNPROTECT(1);
//   return out;
// }



// for (int ri = 0; ri < nr; ri++)
// {
//   r = SET_VECTOR_ELT(out, ri, Rf_allocVector(TYPEOF(a), rsize));
//   switch (TYPEOF(a))
//   {
//   case LGLSXP:
//   case INTSXP:
//   {
//   }
//   case REALSXP:
//   {
//     double *rp = REAL(r);
//     double *ai = ++ap;
//   }
//   case CPLXSXP:
//   {
//     Rcomplex *rp = COMPLEX(r);
//     Rcomplex *ai = ++ap;
//   }
//   }

//   ai = ++ap;
//   for (int i = 0; i < rsize; i++, ai += nr)
//   {
//     *(rp++) = *ai;
//   }
// }

// #define LOGICAL(x)	((int *) DATAPTR(x))
// #define INTEGER(x)	((int *) DATAPTR(x))
// #define RAW(x)		((Rbyte *) DATAPTR(x))
// #define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
// #define REAL(x)		((double *) DATAPTR(x))

// Rf_setAttrib(r, R_DimSymbol, n)
// #define SET_DIM(x, n)     	Rf_setAttrib(x, R_DimSymbol, n)
// #define SET_DIMNAMES(x, n)     	Rf_setAttrib(x, R_DimNamesSymbol, n)


SEXP extract_1row(SEXP obj)
{
  //  int i = 2;
  SEXP dim = Rf_getAttrib(obj, R_DimSymbol);
  R_len_t ndim = XLENGTH(dim);

  int nr = *(INTEGER(dim));
  // SEXP extract_cll, cl;

  // SEXP cl = Rf_cons(PROTECT(Rf_ScalarInteger(i)), R_MissingArg);
  //ndim++;

  SEXP cl = R_NilValue;
  for (; ndim != 1; ndim--)
    cl = Rf_cons(R_MissingArg, cl);

  SEXP rowidx = PROTECT(Rf_ScalarInteger(1));
  int *prowidx = INTEGER0(rowidx);

  cl = Rf_cons(rowidx, cl);
  cl = Rf_cons(obj, cl);
  cl = Rf_lcons(Rf_install("["), cl);
  SEXP rho = R_GetCurrentEnv();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, nr));
  for (int i = 0; i < nr; i++, (*prowidx)++)
    SET_VECTOR_ELT(out, i, Rf_eval(cl, rho));

  UNPROTECT(2);
  return out;

  return Rf_eval(cl, rho);

  int i;
  cl = Rf_cons(PROTECT(Rf_ScalarInteger(i)), R_NilValue);
  cl = Rf_cons(R_MissingArg, cl);
  // cl = Rf_cons(cl, R_NilValue);
  UNPROTECT(1);
  return cl;
  while (ndim != 0)
  {
    cl = Rf_cons(cl, R_MissingArg);
    ndim--;
  }
  UNPROTECT(1);
  return cl;
  // for()
  // extract_cll = cl = PROTECT(Rf_allocList(ndim));
  // SET_TYPEOF(extract_cll, LANGSXP);

  //  SEXP s, t;

  //   t = s = PROTECT(allocList(3));
  //   SET_TYPEOF(s, LANGSXP);
  //   SETCAR(t, install("print")); t = CDR(t);
  //   SETCAR(t,  CAR(a)); t = CDR(t);
  //   SETCAR(t, ScalarInteger(digits));
  //   SET_TAG(t, install("digits"));
  //   eval(s, env);
}



#define STRIDED_MEMCPY(SOURCEPTR, DESTPTR, N, STRIDESIZE) \
  for (int n = N; n != 0; n--)                            \
  {                                                       \
    *(DESTPTR++) = *SOURCEPTR;                            \
    SOURCEPTR += STRIDESIZE;                              \
  }



   // R_len_t ndim;
    // if (Rf_isFrame(a))
    // {
    // }
    // else
    //   ndim = XLENGTH(adim);

    // SEXP cl = R_NilValue;
    // for (ndim--; ndim != 0; ndim--)
    //   cl = Rf_cons(R_MissingArg, cl);

    // SEXP rowidx = PROTECT(Rf_ScalarInteger(1));
    // int *prowidx = INTEGER0(rowidx);

    // cl = Rf_cons(rowidx, cl);
    // cl = Rf_cons(a, cl);
    // cl = Rf_lcons(Rf_install("["), cl);
    // SEXP rho = R_GetCurrentEnv();

    // // Rf_PrintValue(cl);
    // for (int i = 0; i < nr; i++, (*prowidx)++)
    //   SET_VECTOR_ELT(out, i, Rf_eval(cl, rho));
    // UNPROTECT(1);