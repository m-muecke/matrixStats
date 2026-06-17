#' Calculates the product for each row (column) in a matrix
#'
#' Calculates the product for each row (column) in a matrix.
#'
#' If \code{method = "expSumLog"}, then then \code{\link{product}}() function is
#' used, which calculates the product via the logarithmic transform (treating
#' negative values specially).  This improves the precision and lowers the risk
#' for numeric overflow.  If \code{method = "direct"}, the direct product is
#' calculated via the \code{\link[base]{prod}}() function.
#'
#' @inheritParams rowAlls
#' @inheritParams rowDiffs
#'
#' @param method A \code{\link[base]{character}} string specifying how each
#' product is calculated.
#'
#' @return Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' length N (K).
#'
#' @section Missing values:
#' Note, if \code{method = "expSumLog"}, \code{na.rm = FALSE}, and \code{x}
#' contains missing values (\code{\link[base]{NA}} or
#' \code{\link[base:is.finite]{NaN}}), then the calculated value is also
#' missing value.  Note that it depends on platform whether
#' \code{\link[base:is.finite]{NaN}} or \code{\link[base]{NA}} is returned
#' when an \code{\link[base:is.finite]{NaN}} exists, cf.
#' \code{\link[base]{is.nan}}().
#'
#' @author Henrik Bengtsson
#'
#' @keywords array iteration robust univar
#' @export
rowProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                     method = c("direct", "expSumLog"), ..., useNames = TRUE) {
  # Argument 'x':
  if (!is.matrix(x)) defunctShouldBeMatrix(x)
  
  # Apply subset
  if (!is.null(rows) && !is.null(cols)) x <- x[rows, cols, drop = FALSE]
  else if (!is.null(rows)) x <- x[rows, , drop = FALSE]
  else if (!is.null(cols)) x <- x[, cols, drop = FALSE]

  # Argument 'method':
  method <- method[1L]

  has_nas <- TRUE
  .Call(C_rowProds, x, dim(x), NULL, NULL, na.rm, has_nas, TRUE,
        productMethodCode(method), useNames)
}


#' @rdname rowProds
#' @export
colProds <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                     method = c("direct", "expSumLog"), ..., useNames = TRUE) {
  # Argument 'x':
  if (!is.matrix(x)) defunctShouldBeMatrix(x)

  # Apply subset
  if (!is.null(rows) && !is.null(cols)) x <- x[rows, cols, drop = FALSE]
  else if (!is.null(rows)) x <- x[rows, , drop = FALSE]
  else if (!is.null(cols)) x <- x[, cols, drop = FALSE]

  # Argument 'method':
  method <- method[1L]

  has_nas <- TRUE
  .Call(C_rowProds, x, dim(x), NULL, NULL, na.rm, has_nas, FALSE,
        productMethodCode(method), useNames)
}

# Maps argument 'method' to the integer code understood by C_rowProds:
#   0 = "direct", 1 = "expSumLog"
productMethodCode <- function(method) {
  if (method == "direct") {
    0L
  } else if (method == "expSumLog") {
    1L
  } else {
    stop(sprintf("Unknown value of argument '%s': %s", "method", method))
  }
}
