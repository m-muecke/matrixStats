/***************************************************************************
 Public methods:
 SEXP colWhichRanges(SEXP x, ...)

 Authors: Henrik Bengtsson.
 **************************************************************************/
#include <Rdefines.h>
#include "000.types.h"
#include "colWhichRanges_lowlevel.h"
#include "naming.h"

SEXP colWhichRanges(SEXP x, SEXP dim, SEXP rows, SEXP cols, SEXP what, SEXP naRm, SEXP hasNA, SEXP useNames) {
  SEXP ans = NILSXP;
  int what2, narm, hasna, usenames;
  R_xlen_t nrow, ncol;

  /* Coercion moved down to C */
  PROTECT(dim = coerceVector(dim, INTSXP));

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  /* Argument 'what': */
  if (length(what) != 1)
    error("Argument 'what' must be a single number");
  if (!isNumeric(what))
    error("Argument 'what' must be a numeric number");
  what2 = asInteger(what);
  if (what2 < 0 || what2 > 1)
    error("Invalid value of 'what': %d", what2);

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

  /* Argument 'useNames': */
  usenames = asLogicalNoNA(useNames, "useNames");

  PROTECT(ans = allocVector(INTSXP, ncols));
  if (isReal(x)) {
    colWhichRanges_dbl(REAL(x), nrow, ncol, crows, nrows, rowsHasNA, ccols, ncols, colsHasNA, what2, narm, hasna, INTEGER(ans));
  } else if (isInteger(x)) {
    colWhichRanges_int(INTEGER(x), nrow, ncol, crows, nrows, rowsHasNA, ccols, ncols, colsHasNA, what2, narm, hasna, INTEGER(ans));
  }

  if (usenames) {
    SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
    if (dimnames != R_NilValue) {
      SEXP namesVec = VECTOR_ELT(dimnames, 1);
      if (namesVec != R_NilValue) {
        setNames(ans, namesVec, ncols, ccols);
      }
    }
  }

  UNPROTECT(2); /* ans, dim */

  return(ans);
} // colWhichRanges()
