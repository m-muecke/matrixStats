/***********************************************************************
 TEMPLATE:
  void colWhichRanges_<int|dbl>(ARGUMENTS_LIST)

 ARGUMENTS_LIST:
  X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA, R_xlen_t *cols, R_xlen_t ncols, int colsHasNA, int what, int narm, int hasna, int *ans

 Arguments:
   The following macros ("arguments") should be defined for the
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i' or 'r'

 The result 'ans' is an integer vector of length 'ncols' holding, for each
 column, the (1-based) row index of the first minimum (what == 0) or maximum
 (what == 1) value.  See rowWhichRanges_lowlevel_template.h for the exact
 semantics regarding ties, missing values and subsetting.

 Authors:
  Henrik Bengtsson.
 ***********************************************************************/
#include <R_ext/Memory.h>
#include "000.types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C)
 */
#include "000.templates-types.h"


void CONCAT_MACROS(colWhichRanges, X_C_SIGNATURE)(X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol,
                        R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA,
                        R_xlen_t *cols, R_xlen_t ncols, int colsHasNA,
                        int what, int narm, int hasna, int *ans) {
  R_xlen_t ii, jj;
  R_xlen_t colBegin, idx, thisRow;
  X_C_TYPE value, best;
  int counted, isna, norows, nocols;

  if (cols == NULL) { nocols = 1; } else { nocols = 0; }
  if (rows == NULL) { norows = 1; } else { norows = 0; }

  /* If there are no missing values, don't try to remove them. */
  if (hasna == FALSE)
    narm = FALSE;

  for (jj=0; jj < ncols; jj++) {
    if (nocols) {
      colBegin = jj * nrow;
    } else {
      R_xlen_t colsElement = cols[jj];
      if (!colsHasNA || colsElement != NA_R_XLEN_T) {
        colBegin = colsElement * nrow;
      } else {
        colBegin = NA_R_XLEN_T;
      }
    }

    best = X_NA;
    counted = 0;
    isna = 0;
    ans[jj] = NA_INTEGER;

    for (ii=0; ii < nrows; ii++) {
      if (norows) {
        thisRow = ii;
        if (!colsHasNA || colBegin != NA_R_XLEN_T) {
          value = x[colBegin + ii];
        } else {
          value = X_NA;
        }
      } else {
        thisRow = rows[ii];
        if (!rowsHasNA && !colsHasNA) {
          value = x[colBegin + rows[ii]];
        } else {
          idx = R_INDEX_OP(colBegin, +, (rows[ii]), 1, 1);
          value = R_INDEX_GET(x, idx, X_NA, 1);
        }
      }

      if (X_ISNAN(value)) {
        /* A missing value: with narm == FALSE the whole column is NA. */
        if (!narm) {
          isna = 1;
          break;
        }
      } else if (!counted) {
        best = value;
        ans[jj] = (int)(thisRow + 1);
        counted = 1;
      } else if ((what == 0) ? (value < best) : (value > best)) {
        best = value;
        ans[jj] = (int)(thisRow + 1);
      }
    } /* for (ii ...) */

    if (isna) ans[jj] = NA_INTEGER;
  } /* for (jj ...) */
}
