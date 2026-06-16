/***********************************************************************
 TEMPLATE:
  void rowWhichRanges_<int|dbl>(ARGUMENTS_LIST)

 ARGUMENTS_LIST:
  X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA, R_xlen_t *cols, R_xlen_t ncols, int colsHasNA, int what, int narm, int hasna, int *ans

 Arguments:
   The following macros ("arguments") should be defined for the
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i' or 'r'

 The result 'ans' is an integer vector of length 'nrows' holding, for each
 row, the (1-based) column index of the first minimum (what == 0) or maximum
 (what == 1) value.  In case of ties the lowest such index is returned.  The
 index is NA_INTEGER when the row has no values to compare, i.e. when there
 are no columns, when narm is true and all values are missing, or when narm
 is false and at least one value is missing.  The index always refers to the
 columns of the original matrix, i.e. cols[jj] + 1 when 'cols' subsetting is
 used.

 Authors:
  Henrik Bengtsson.
 ***********************************************************************/
#include <R_ext/Memory.h>
#include "000.types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C)
 */
#include "000.templates-types.h"


void CONCAT_MACROS(rowWhichRanges, X_C_SIGNATURE)(X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol,
                        R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA,
                        R_xlen_t *cols, R_xlen_t ncols, int colsHasNA,
                        int what, int narm, int hasna, int *ans) {
  R_xlen_t ii, jj;
  R_xlen_t colBegin, idx, thisCol;
  X_C_TYPE value, *best;
  int *skip;

  /* If there are no missing values, don't try to remove them. */
  if (hasna == FALSE)
    narm = FALSE;

  best = (X_C_TYPE *) R_alloc(nrows, sizeof(X_C_TYPE));
  for (ii=0; ii < nrows; ii++) ans[ii] = NA_INTEGER;

  if (hasna) {
    skip = (int *) R_alloc(nrows, sizeof(int));
    for (ii=0; ii < nrows; ii++) skip[ii] = 0;

    for (jj=0; jj < ncols; jj++) {
      thisCol = (cols == NULL) ? jj : cols[jj];
      colBegin = R_INDEX_OP(thisCol, *, nrow, colsHasNA, 0);

      for (ii=0; ii < nrows; ii++) {
        if (skip[ii]) continue;

        idx = R_INDEX_OP(colBegin, +, ((rows == NULL) ? (ii) : rows[ii]), colsHasNA, rowsHasNA);
        value = R_INDEX_GET(x, idx, X_NA, colsHasNA || rowsHasNA);

        if (X_ISNAN(value)) {
          /* A missing value: with narm == FALSE the whole row is NA. */
          if (!narm) {
            ans[ii] = NA_INTEGER;
            skip[ii] = 1;
          }
        } else if (ans[ii] == NA_INTEGER) {
          best[ii] = value;
          ans[ii] = (int)(thisCol + 1);
        } else if ((what == 0) ? (value < best[ii]) : (value > best[ii])) {
          best[ii] = value;
          ans[ii] = (int)(thisCol + 1);
        }
      }
    } /* for (jj ...) */
  } else {
    /* No missing values */
    if (ncols > 0) {
      thisCol = (cols == NULL) ? 0 : cols[0];
      colBegin = thisCol * nrow;
      for (ii=0; ii < nrows; ii++) {
        best[ii] = x[((rows == NULL) ? (ii) : rows[ii]) + colBegin];
        ans[ii] = (int)(thisCol + 1);
      }

      for (jj=1; jj < ncols; jj++) {
        thisCol = (cols == NULL) ? jj : cols[jj];
        colBegin = thisCol * nrow;
        for (ii=0; ii < nrows; ii++) {
          value = x[((rows == NULL) ? (ii) : rows[ii]) + colBegin];
          if ((what == 0) ? (value < best[ii]) : (value > best[ii])) {
            best[ii] = value;
            ans[ii] = (int)(thisCol + 1);
          }
        }
      }
    }
  } /* if (hasna) */
}
