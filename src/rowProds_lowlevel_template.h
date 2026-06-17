/***********************************************************************
 TEMPLATE:
  void rowProds_<int|dbl>(ARGUMENTS_LIST)

 ARGUMENTS_LIST:
  X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA, R_xlen_t *cols, R_xlen_t ncols, int colsHasNA, int narm, int hasna, int byrow, int method, double *ans

 Arguments:
   The following macros ("arguments") should be defined for the
   template to work as intended.

  - METHOD: the name of the resulting function
  - X_TYPE: 'i' or 'r'

 The product of each row (byrow == TRUE) or column (byrow == FALSE) is
 calculated using one of two methods:
  - method == 0 ("direct"):   the elements are multiplied directly.
  - method == 1 ("expSumLog"): the product is calculated via the
    logarithmic transform sum(log(abs(x))), treating negative values
    specially, cf. productExpSumLog().

 Copyright: Henrik Bengtsson, 2017
 ***********************************************************************/
#include <R_ext/Memory.h>
#include <Rdefines.h>
#include <Rmath.h>
#include "000.types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, X_ISNAN)
 */
#include "000.templates-types.h"

/* NOTE: Like rowSums2(), the elements are always iterated through
 * in-order column-major no matter the flag byrow. State for each
 * output element is therefore accumulated across the iteration.
 */
void CONCAT_MACROS(rowProds, X_C_SIGNATURE)(X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol,
                  R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA,
                  R_xlen_t *cols, R_xlen_t ncols, int colsHasNA,
                  int narm, int hasna, int byrow, int method, double *ans) {
  R_xlen_t ii, jj, kk, idx, nout;
  R_xlen_t colOffset;
  X_C_TYPE value;
  int nocols, norows;
  /* "direct" method state */
  LDOUBLE *prod = NULL;
  /* "expSumLog" method state */
  LDOUBLE *logSum = NULL, t;
  int *isneg = NULL, *hasZero = NULL, *isna = NULL;

  /* If there are no missing values, don't try to remove them. */
  if (hasna == FALSE)
    narm = FALSE;

  if (cols == NULL) { nocols = 1; } else { nocols = 0; }
  if (rows == NULL) { norows = 1; } else { norows = 0; }

  /* Number of output elements; state is indexed by the output position. */
  nout = byrow ? nrows : ncols;
  if (nout == 0) return;

  if (method == 0) {
    prod = LDOUBLE_ALLOC(nout);
    for (kk = 0; kk < nout; kk++) prod[kk] = 1.0;
  } else {
    logSum = LDOUBLE_ALLOC(nout);
    isneg = (int *) R_alloc(nout, sizeof(int));
    hasZero = (int *) R_alloc(nout, sizeof(int));
    isna = (int *) R_alloc(nout, sizeof(int));
    for (kk = 0; kk < nout; kk++) {
      logSum[kk] = 0.0;
      isneg[kk] = 0;
      hasZero[kk] = 0;
      isna[kk] = 0;
    }
  }

  for (jj = 0; jj < ncols; jj++) {
    if (nocols) {
      colOffset = jj * nrow;
    } else if (!colsHasNA) {
      colOffset = cols[jj] * nrow;
    } else {
      colOffset = R_INDEX_OP(cols[jj], *, nrow, 1, 1);
    }
    for (ii = 0; ii < nrows; ii++) {
      if (!colsHasNA && norows) {
        /* In this special case, we can eliminate the
           possibility of having NA indices */
        idx = colOffset + ii;
        value = x[idx];
      } else if (!colsHasNA && !rowsHasNA && !norows) {
        idx = colOffset + rows[ii];
        value = x[idx];
      } else {
        if (norows) {
          idx = R_INDEX_OP(colOffset, +, ii, 1, 1);
        } else {
          idx = R_INDEX_OP(colOffset, +, rows[ii], 1, 1);
        }
        value = R_INDEX_GET(x, idx, X_NA, 1);
      }

      /* The output element this value contributes to. */
      kk = byrow ? ii : jj;

      /* Drop missing values? */
      if (narm && X_ISNAN(value)) continue;

      if (method == 0) {
        /* "direct": multiply directly, letting NA/NaN propagate. */
        if (X_ISNAN(value)) {
          prod[kk] = NA_REAL;
        } else {
          prod[kk] *= (LDOUBLE) value;
        }
      } else {
        /* "expSumLog": accumulate sum(log(abs(x))). */
        if (isna[kk]) continue;
        t = (LDOUBLE) value;
#if X_TYPE == 'i'
        if (X_ISNAN(value)) {
          isna[kk] = 1;
          continue;
        } else if (t < 0) {
          isneg[kk] = !isneg[kk];
          t = -t;
        } else if (t == 0) {
          hasZero[kk] = 1;
        }
#elif X_TYPE == 'r'
        if (t < 0) {
          isneg[kk] = !isneg[kk];
          t = -t;
        }
#endif
        logSum[kk] += log(t);
      }
    } /* for (ii ...) */

    R_CHECK_USER_INTERRUPT(jj);
  } /* for (jj ...) */

  /* Finalize the output. */
  for (kk = 0; kk < nout; kk++) {
    LDOUBLE y;
    if (method == 0) {
      y = prod[kk];
      /* For consistency with productExpSumLog() and integers, NaN
         (from either NA or NaN inputs) is reported as NA. */
      if (ISNAN(y)) {
        y = NA_REAL;
      } else if (y > DBL_MAX) {
        y = R_PosInf;
      } else if (y < -DBL_MAX) {
        y = R_NegInf;
      }
    } else {
      if (isna[kk]) {
        y = NA_REAL;
      } else {
        y = logSum[kk];
        if (ISNAN(y)) {
          /* The information on an NA value is lost when calculating
             log(abs(NA)); for consistency we return NA in all cases. */
          y = NA_REAL;
        } else if (hasZero[kk]) {
          /* No NA in this product and it contains a zero. */
          y = 0;
        } else {
          y = exp(y);
          if (isneg[kk]) y = -y;
          if (y > DBL_MAX) {
            y = R_PosInf;
          } else if (y < -DBL_MAX) {
            y = R_NegInf;
          }
        }
      }
    }
    ans[kk] = (double) y;
  }
}
