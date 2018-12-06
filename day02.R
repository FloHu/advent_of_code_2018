# task 1
v <- readLines("input_day02.txt")

split_strs <- lapply(v, function(x) {
  unlist(stringr::str_split(pattern = "", string = x))
})

sorted_strs <- lapply(split_strs, sort)

rles <- lapply(sorted_strs, rle)

twos <- sapply(rles, function(x) any(x$lengths == 2))
threes <- sapply(rles, function(x) any(x$lengths == 3))

checksum <- sum(twos) * sum(threes)
cat("Checksum: ", checksum, "\n")

# task 2
# this function takes a pair of character vectors and returns true if the
# pair differs by just one position:
is_pair <- function(x, y) {
  return(sum(x != y) == 1)
}

pairs <- data.frame(gtools::combinations(length(v), 2))
names(pairs) <- c('inda', 'indb')
pairs$a <- split_strs[pairs$inda]
pairs$b <- split_strs[pairs$indb]
pairs$is_pair <- mapply(is_pair, pairs$a, pairs$b)

# get letters that are the same:
a <- pairs$a[[which(pairs$is_pair)]]
b <- pairs$b[[which(pairs$is_pair)]]
same_letters <- a[a == b]
cat("The same letters are: ", paste0(same_letters, collapse = ""), "\n")

