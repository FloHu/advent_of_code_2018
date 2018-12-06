# Task 1:
v <- readLines("input_day05.txt")
v <- unlist(strsplit(v, ""))
all(v %in% letters | v %in% LETTERS)
v2 <- c(v[-1], "")
v2 <- ifelse(v2 %in% letters, toupper(v2), tolower(v2))

get_n_units <- function(v, v2) {
  continue <- TRUE
  while (continue) {
    excl <- which(v == v2)
    diffs <- diff(excl)
    # this if is necessary because v[-integer(0)] of a vector is also integer(0)
    if ((length(diffs) > 0) & (any(diffs == 1))) {
      excl <- excl[-which(diffs == 1)]
    }
    excl <- c(excl, excl+1)
    v <- v[-excl]
    v2 <- c(v[-1], "")
    v2 <- ifelse(v2 %in% letters, toupper(v2), tolower(v2))
    # cat("v is now ", v, "\n")
    if (!any(v == v2)) continue <- FALSE
  }
  return(length(v))
}

cat("Day 5, task 1:", get_n_units(v, v2), "units")

# Task 2:
lens <- numeric(length = length(letters))
names(lens) <- letters

lens <- sapply(names(lens), function(x) {
  v <- v[!v == x]
  v <- v[!v == toupper(x)]
  v2 <- c(v[-1], "")
  v2 <- ifelse(v2 %in% letters, toupper(v2), tolower(v2))
  return(get_n_units(v, v2))
})

cat("Day 5, task 2:", min(sort(lens)), "\n")
