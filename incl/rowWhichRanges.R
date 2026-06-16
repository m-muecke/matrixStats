x <- matrix(c(3, 1, 2, 1, 1, 5, 7, 4, 6), nrow = 3, ncol = 3, byrow = TRUE)
# Index of the minimum/maximum in each row
print(rowWhichMins(x))   # ties -> lowest index: row 2 -> 1
print(rowWhichMaxs(x))

# Index of the minimum/maximum in each column
print(colWhichMins(x))
print(colWhichMaxs(x))

# Missing values
x[2, 1] <- NA
print(rowWhichMaxs(x))             # NA for the row with a missing value
print(rowWhichMaxs(x, na.rm = TRUE))
