/***************************************************************************
 Public methods:
 SEXP rowProds(SEXP x, SEXP dim, SEXP rows, SEXP cols, SEXP naRm, SEXP hasNA, SEXP byRow, SEXP method, SEXP useNames)

 Authors: Henrik Bengtsson

 Copyright Henrik Bengtsson, 2017
 **************************************************************************/
#include <Rdefines.h>
#include "000.types.h"
#include "rowProds_lowlevel.h"
#include "naming.h"

SEXP rowProds(SEXP x, SEXP dim, SEXP rows, SEXP cols, SEXP naRm, SEXP hasNA, SEXP byRow, SEXP method, SEXP useNames) {
  int narm, hasna, byrow, meth, usenames;
  SEXP ans;
  R_xlen_t nrow, ncol;

  /* Coercion moved down to C */
  PROTECT(dim = coerceVector(dim, INTSXP));

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL | R_TYPE_LGL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  /* Argument 'naRm': */
  narm = asLogicalNoNA(naRm, "na.rm");

  /* Argument 'hasNA': */
  hasna = asLogicalNoNA(hasNA, "hasNA");

  /* Argument 'rows' and 'cols': */
  R_xlen_t nrows, ncols;
  int rowsHasNA;
  int colsHasNA;
  R_xlen_t *crows = validateIndicesCheckNA(rows, nrow, 0, &nrows, &rowsHasNA);
  R_xlen_t *ccols = validateIndicesCheckNA(cols, ncol, 0, &ncols, &colsHasNA);

  /* Argument 'byRow': */
  byrow = asLogical(byRow);

  /* Argument 'method': 0 = "direct", 1 = "expSumLog" */
  meth = asInteger(method);

  /* R allocate a double vector of the desired length */
  PROTECT(ans = allocVector(REALSXP, byrow ? nrows : ncols));

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    rowProds_dbl(REAL(x), nrow, ncol, crows, nrows, rowsHasNA, ccols, ncols, colsHasNA, narm, hasna, byrow, meth, REAL(ans));
  } else if (isInteger(x) || isLogical(x)) {
    rowProds_int(INTEGER(x), nrow, ncol, crows, nrows, rowsHasNA, ccols, ncols, colsHasNA, narm, hasna, byrow, meth, REAL(ans));
  }

  /* Argument 'useNames': */
  usenames = asLogicalNoNA(useNames, "useNames");

  if (usenames) {
    SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
    if (dimnames != R_NilValue) {
      if (byrow) {
        SEXP namesVec = VECTOR_ELT(dimnames, 0);
        if (namesVec != R_NilValue) {
          setNames(ans, namesVec, nrows, crows);
        }
      } else {
        SEXP namesVec = VECTOR_ELT(dimnames, 1);
        if (namesVec != R_NilValue) {
          setNames(ans, namesVec, ncols, ccols);
        }
      }
    }
  }

  UNPROTECT(2);

  return(ans);
}
