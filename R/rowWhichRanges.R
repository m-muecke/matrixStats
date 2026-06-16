#' Gets the index of the minimum or maximum in each row (column) of a matrix
#'
#' Gets the index of the minimum or maximum value in each row (column) of a
#' matrix.
#'
#' @inheritParams rowAlls
#'
#' @return Returns an \code{\link[base]{integer}} \code{\link[base]{vector}} of
#' length N (K), where N (K) is the number of rows (columns) of the input
#' matrix.  Each element is the (1-based) column (row) index of the first
#' minimum or maximum value of the corresponding row (column).  In case of
#' ties, the lowest index is returned.  The index is
#' \code{\link[base:NA]{NA_integer_}} when there is no value to compare, i.e.
#' when the row (column) has no elements, when \code{na.rm = TRUE} and all
#' values are missing, or when \code{na.rm = FALSE} and at least one value is
#' missing.  When \code{cols} (\code{rows}) is specified, the returned index
#' refers to the columns (rows) of the original matrix \code{x}.
#'
#' @example incl/rowWhichRanges.R
#'
#' @author Henrik Bengtsson
#'
#' @seealso \code{\link{rowRanges}}() and \code{\link[base]{which.min}}().
#'
#' @keywords array iteration robust univar
#'
#' @export
rowWhichMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                         dim. = dim(x), ..., useNames = TRUE) {
  .Call(C_rowWhichRanges, x, dim., rows, cols, 0L, na.rm, TRUE, useNames)
}


#' @rdname rowWhichMins
#' @export
rowWhichMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                         dim. = dim(x), ..., useNames = TRUE) {
  .Call(C_rowWhichRanges, x, dim., rows, cols, 1L, na.rm, TRUE, useNames)
}


#' @rdname rowWhichMins
#' @export
colWhichMins <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                         dim. = dim(x), ..., useNames = TRUE) {
  .Call(C_colWhichRanges, x, dim., rows, cols, 0L, na.rm, TRUE, useNames)
}


#' @rdname rowWhichMins
#' @export
colWhichMaxs <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                         dim. = dim(x), ..., useNames = TRUE) {
  .Call(C_colWhichRanges, x, dim., rows, cols, 1L, na.rm, TRUE, useNames)
}
