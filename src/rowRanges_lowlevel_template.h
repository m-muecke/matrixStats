/***********************************************************************
 TEMPLATE:
  void rowRanges_<int|dbl>(ARGUMENTS_LIST)

 ARGUMENTS_LIST:
  X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA, R_xlen_t *cols, R_xlen_t ncols, int colsHasNA, int what, int narm, int hasna, X_C_TYPE *ans, int *is_counted

 Arguments:
   The following macros ("arguments") should be defined for the
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i' or 'r'
  - ANS_TYPE: 'i' or 'r'

 Authors:
  Henrik Bengtsson.

 Copyright: Henrik Bengtsson, 2014
 ***********************************************************************/
#include <R_ext/Memory.h>
#include "000.types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C)
    ANS_TYPE => (ANS_SXP, ANS_NA, ANS_C_TYPE, ANS_IN_C)
 */
#include "000.templates-types.h"


void CONCAT_MACROS(rowRanges, X_C_SIGNATURE)(X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol,
                        R_xlen_t *rows, R_xlen_t nrows, int rowsHasNA,
                        R_xlen_t *cols, R_xlen_t ncols, int colsHasNA,
                        int what, int narm, int hasna, X_C_TYPE *ans, int *is_counted) {
  R_xlen_t ii, jj;
  R_xlen_t colBegin, idx;
  X_C_TYPE value, *mins = NULL, *maxs = NULL;
  int *skip = NULL;
  int use_fast = 0;

  /* Rprintf("(nrow,ncol)=(%d,%d), what=%d\n", nrow, ncol, what); */

  /* If there are no missing values, don't try to remove them. */
  if (hasna == FALSE)
    narm = FALSE;

  if (hasna) {
    skip = (int *) R_alloc(nrows, sizeof(int));
    for (ii=0; ii < nrows; ii++) {
      is_counted[ii] = 0;
      skip[ii] = 0;
    }

    /* Fast path for the common case of no subsetting and no NA indices.
       Only used for doubles: here NA/NaN are part of the data (not the
       indices), so we can stream the matrix column-by-column using simple,
       branchless min/max updates that the compiler can auto-vectorise.
       Integers keep the generic path below (the NA_INTEGER sentinel would
       break a branchless comparison). */
#if X_TYPE == 'r'
    use_fast = (cols == NULL && rows == NULL && !colsHasNA && !rowsHasNA);
#endif

    /* Missing values */
    if (what == 0) {
      /* rowMins() */
      mins = ans;

      if (use_fast) {
#if X_TYPE == 'r'
        for (ii=0; ii < nrows; ii++) mins[ii] = R_PosInf;
        if (narm) {
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              v = X_ISNAN(v) ? R_PosInf : v;   /* ignore NA/NaN */
              mins[ii] = v < mins[ii] ? v : mins[ii];
            }
          }
        } else {
          /* na.rm = FALSE: NA/NaN must propagate.  Compute the min over the
             non-missing values branchlessly, flag which rows contained a
             missing value, then resolve only those (rare) rows exactly. */
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              mins[ii] = v < mins[ii] ? v : mins[ii];
              skip[ii] |= X_ISNAN(v);
            }
          }
          for (ii=0; ii < nrows; ii++) {
            if (skip[ii]) {
              mins[ii] = R_NaN;
              for (jj=0; jj < ncols; jj++) {
                if (X_ISNA(x[jj * nrow + ii])) { mins[ii] = X_NA; break; }
              }
            }
          }
        }
#endif
      } else {
        for (jj=0; jj < ncols; jj++) {
          colBegin = R_INDEX_OP(((cols == NULL) ? (jj) : cols[jj]), *, nrow, colsHasNA, 0);

          for (ii=0; ii < nrows; ii++) {
            if (!narm && skip[ii]) continue;

            idx = R_INDEX_OP(colBegin, +, ((rows == NULL) ? (ii) : rows[ii]), colsHasNA, rowsHasNA);
            value = R_INDEX_GET(x, idx, X_NA, colsHasNA || rowsHasNA);

            if (X_ISNAN(value)) {
              if (!narm) {
                mins[ii] = value;
                is_counted[ii] = 1;
                /* Early stopping? */
#if X_TYPE == 'i'
                skip[ii] = 1;
#elif X_TYPE == 'r'
                if (X_ISNA(value)) skip[ii] = 1;
#endif
              }
            } else if (!is_counted[ii]) {
              mins[ii] = value;
              is_counted[ii] = 1;
            } else if (value < mins[ii]) {
              mins[ii] = value;
            }
          }
        } /* for (jj ...) */

#if X_TYPE == 'r'
        /* Handle zero non-missing values */
        for (ii=0; ii < nrows; ii++) {
          if (!is_counted[ii]) {
            mins[ii] = R_PosInf;
          }
        }
#endif
      }
    } else if (what == 1) {
      /* rowMaxs() */
      maxs = ans;

      if (use_fast) {
#if X_TYPE == 'r'
        for (ii=0; ii < nrows; ii++) maxs[ii] = R_NegInf;
        if (narm) {
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              v = X_ISNAN(v) ? R_NegInf : v;   /* ignore NA/NaN */
              maxs[ii] = v > maxs[ii] ? v : maxs[ii];
            }
          }
        } else {
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              maxs[ii] = v > maxs[ii] ? v : maxs[ii];
              skip[ii] |= X_ISNAN(v);
            }
          }
          for (ii=0; ii < nrows; ii++) {
            if (skip[ii]) {
              maxs[ii] = R_NaN;
              for (jj=0; jj < ncols; jj++) {
                if (X_ISNA(x[jj * nrow + ii])) { maxs[ii] = X_NA; break; }
              }
            }
          }
        }
#endif
      } else {
        for (jj=0; jj < ncols; jj++) {
          colBegin = R_INDEX_OP(((cols == NULL) ? (jj) : cols[jj]), *, nrow, colsHasNA, 0);

          for (ii=0; ii < nrows; ii++) {
            if (!narm && skip[ii]) continue;

            idx = R_INDEX_OP(colBegin, +, ((rows == NULL) ? (ii) : rows[ii]), colsHasNA, rowsHasNA);
            value = R_INDEX_GET(x, idx, X_NA, colsHasNA || rowsHasNA);

            if (X_ISNAN(value)) {
              if (!narm) {
                maxs[ii] = value;
                is_counted[ii] = 1;
                /* Early stopping? */
#if X_TYPE == 'i'
                skip[ii] = 1;
#elif X_TYPE == 'r'
                if (X_ISNA(value)) skip[ii] = 1;
#endif
              }
            } else if (!is_counted[ii]) {
              maxs[ii] = value;
              is_counted[ii] = 1;
            } else if (value > maxs[ii]) {
              maxs[ii] = value;
            }
          }
        } /* for (jj ...) */

#if X_TYPE == 'r'
        /* Handle zero non-missing values */
        for (ii=0; ii < nrows; ii++) {
          if (!is_counted[ii]) {
            maxs[ii] = R_NegInf;
          }
        }
#endif
      }
    } else if (what == 2) {
      /* rowRanges() */
      mins = ans;
      maxs = &ans[nrows];

      if (use_fast) {
#if X_TYPE == 'r'
        for (ii=0; ii < nrows; ii++) {
          mins[ii] = R_PosInf;
          maxs[ii] = R_NegInf;
        }
        if (narm) {
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              X_C_TYPE vmin = X_ISNAN(v) ? R_PosInf : v;   /* ignore NA/NaN */
              X_C_TYPE vmax = X_ISNAN(v) ? R_NegInf : v;
              mins[ii] = vmin < mins[ii] ? vmin : mins[ii];
              maxs[ii] = vmax > maxs[ii] ? vmax : maxs[ii];
            }
          }
        } else {
          for (jj=0; jj < ncols; jj++) {
            colBegin = jj * nrow;
            for (ii=0; ii < nrows; ii++) {
              X_C_TYPE v = x[colBegin + ii];
              mins[ii] = v < mins[ii] ? v : mins[ii];
              maxs[ii] = v > maxs[ii] ? v : maxs[ii];
              skip[ii] |= X_ISNAN(v);
            }
          }
          for (ii=0; ii < nrows; ii++) {
            if (skip[ii]) {
              X_C_TYPE res = R_NaN;
              for (jj=0; jj < ncols; jj++) {
                if (X_ISNA(x[jj * nrow + ii])) { res = X_NA; break; }
              }
              mins[ii] = res;
              maxs[ii] = res;
            }
          }
        }
#endif
      } else {
        for (jj=0; jj < ncols; jj++) {
          colBegin = R_INDEX_OP(((cols == NULL) ? (jj) : cols[jj]), *, nrow, colsHasNA, 0);

          for (ii=0; ii < nrows; ii++) {
            if (!narm && skip[ii]) continue;

            idx = R_INDEX_OP(colBegin, +, ((rows == NULL) ? (ii) : rows[ii]), colsHasNA, rowsHasNA);
            value = R_INDEX_GET(x, idx, X_NA, colsHasNA || rowsHasNA);

            if (X_ISNAN(value)) {
              if (!narm) {
                mins[ii] = value;
                maxs[ii] = value;
                is_counted[ii] = 1;
                /* Early stopping? */
#if X_TYPE == 'i'
                skip[ii] = 1;
#elif X_TYPE == 'r'
                if (X_ISNA(value)) skip[ii] = 1;
#endif
              }
            } else if (!is_counted[ii]) {
              mins[ii] = value;
              maxs[ii] = value;
              is_counted[ii] = 1;
            } else if (value < mins[ii]) {
              mins[ii] = value;
            } else if (value > maxs[ii]) {
              maxs[ii] = value;
            }
          }
        } /* for (jj ...) */

#if X_TYPE == 'r'
        /* Handle zero non-missing values */
        for (ii=0; ii < nrows; ii++) {
          if (!is_counted[ii]) {
            mins[ii] = R_PosInf;
            maxs[ii] = R_NegInf;
          }
        }
#endif
      }
    } /* if (what ...) */
  } else {
    /* No missing values */
    if (what == 0) {
      /* rowMins() */
      mins = ans;

      /* Initiate results */
      for (ii=0; ii < nrows; ii++) {
        mins[ii] = x[ii];
      }

      for (jj=1; jj < ncols; jj++) {
        colBegin = ((cols == NULL) ? (jj) : cols[jj]) * nrow;
        for (ii=0; ii < nrows; ii++) {
          value = x[((rows == NULL) ? (ii) : rows[ii])+colBegin];
          mins[ii] = value < mins[ii] ? value : mins[ii];
        }
      }
    } else if (what == 1) {
      /* rowMax() */
      maxs = ans;

      /* Initiate results */
      for (ii=0; ii < nrows; ii++) {
        maxs[ii] = x[ii];
      }

      for (jj=1; jj < ncols; jj++) {
        colBegin = ((cols == NULL) ? (jj) : cols[jj]) * nrow;
        for (ii=0; ii < nrows; ii++) {
          value = x[((rows == NULL) ? (ii) : rows[ii])+colBegin];
          maxs[ii] = value > maxs[ii] ? value : maxs[ii];
        }
      }
    } else if (what == 2) {
      /* rowRanges()*/
      mins = ans;
      maxs = &ans[nrows];

      /* Initiate results */
      for (ii=0; ii < nrows; ii++) {
        mins[ii] = x[ii];
        maxs[ii] = x[ii];
      }

      for (jj=1; jj < ncols; jj++) {
        colBegin = ((cols == NULL) ? (jj) : cols[jj]) * nrow;
        for (ii=0; ii < nrows; ii++) {
          value = x[((rows == NULL) ? (ii) : rows[ii])+colBegin];
          mins[ii] = value < mins[ii] ? value : mins[ii];
          maxs[ii] = value > maxs[ii] ? value : maxs[ii];
        }
      }
    } /* if (what ...) */
  } /* if (narm) */
}


/***************************************************************************
 HISTORY:
 2015-06-07 [DJ]
  o Supported subsetted computation.
 2014-11-16 [HB]
  o Created.
 **************************************************************************/
