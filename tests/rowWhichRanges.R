library("matrixStats")

## R reference implementations matching the documented semantics:
## ties -> lowest index; NA index when there is nothing to compare, i.e.
## empty input, na.rm = TRUE with all values missing, or na.rm = FALSE with
## any value missing.
rowWhichMins_R <- function(x, na.rm = FALSE, useNames = TRUE) {
  res <- apply(x, MARGIN = 1L, FUN = function(v) {
    if (!na.rm && anyNA(v)) return(NA_integer_)
    keep <- which(!is.na(v))
    if (length(keep) == 0L) return(NA_integer_)
    keep[which.min(v[keep])]
  })
  res <- as.integer(res)
  if (useNames) names(res) <- rownames(x)
  res
} # rowWhichMins_R()

rowWhichMaxs_R <- function(x, na.rm = FALSE, useNames = TRUE) {
  res <- apply(x, MARGIN = 1L, FUN = function(v) {
    if (!na.rm && anyNA(v)) return(NA_integer_)
    keep <- which(!is.na(v))
    if (length(keep) == 0L) return(NA_integer_)
    keep[which.max(v[keep])]
  })
  res <- as.integer(res)
  if (useNames) names(res) <- rownames(x)
  res
} # rowWhichMaxs_R()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# With and without some NAs, over integer and double, row and column
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
for (mode in c("integer", "double")) {
  for (add_na in c(FALSE, TRUE)) {
    x <- matrix(c(3, 1, 2, 1, 1, 5, 7, 4, 6, 8, 2, 8),
                nrow = 4L, ncol = 3L)
    if (add_na) {
      x[2L, 2L] <- NA
      x[3L, ] <- NA
    }
    storage.mode(x) <- mode

    for (na.rm in c(FALSE, TRUE)) {
      # rows
      stopifnot(identical(rowWhichMins(x, na.rm = na.rm),
                          rowWhichMins_R(x, na.rm = na.rm)))
      stopifnot(identical(rowWhichMaxs(x, na.rm = na.rm),
                          rowWhichMaxs_R(x, na.rm = na.rm)))
      # columns mirror rows of the transpose
      stopifnot(identical(colWhichMins(x, na.rm = na.rm),
                          rowWhichMins_R(t(x), na.rm = na.rm)))
      stopifnot(identical(colWhichMaxs(x, na.rm = na.rm),
                          rowWhichMaxs_R(t(x), na.rm = na.rm)))
    }
  }
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Documented example (issue #176)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
m <- rbind(A = c(11, 7, 15), B = c(-2, 9, 9), C = c(9, NaN, 0), D = c(NA, NaN, NA))
stopifnot(identical(unname(rowWhichMaxs(m)), c(3L, 2L, NA, NA)))
stopifnot(identical(unname(rowWhichMaxs(m, na.rm = TRUE)), c(3L, 2L, 1L, NA)))
stopifnot(identical(unname(rowWhichMaxs(m[, 0])), rep(NA_integer_, 4L)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Ties return the lowest index
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopifnot(identical(unname(rowWhichMins(rbind(c(2, 2, 2)))), 1L))
stopifnot(identical(unname(rowWhichMaxs(rbind(c(2, 2, 2)))), 1L))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Names
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stopifnot(identical(names(rowWhichMins(m)), rownames(m)))
stopifnot(is.null(names(rowWhichMins(m, useNames = FALSE))))
colnames(m) <- c("a", "b", "c")
stopifnot(identical(names(colWhichMaxs(m)), colnames(m)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting: returned index refers to the original columns/rows
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- matrix(c(5, 2, 9, 1, 4, 8, 3, 7), nrow = 2L, ncol = 4L)
cols <- c(2L, 4L)
exp <- apply(x[, cols, drop = FALSE], MARGIN = 1L,
             FUN = function(v) cols[which.min(v)])
stopifnot(identical(unname(rowWhichMins(x, cols = cols)), as.integer(exp)))

rows <- c(1L)
exp <- apply(x[rows, , drop = FALSE], MARGIN = 2L,
             FUN = function(v) rows[which.max(v)])
stopifnot(identical(unname(colWhichMaxs(x, rows = rows)), as.integer(exp)))
