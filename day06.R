(day6 <- read.table("input_day06.txt", sep = ",", 
  col.names = c('r', 'c')))
day6$loc <- do.call(paste0, expand.grid(letters, letters))[1:nrow(day6)]

m <- matrix("", nrow = max(day6$r), ncol = max(day6$c))
m[cbind(day6$r, day6$c)] <- day6$loc

day6_m <- as.matrix(day6[, c("r", "c")])
rownames(day6_m) <- day6$loc

# to make task 2 easier, see below
task1_fun <- function(d) {
  d <- sort(d[1, ][-1])
  if (d[1] != d[2]) names(d)[1] else "."
}

populate_m <- function(m, day6_m, FUN) {
  # m = matrix (as a skeleton)
  # loc = data frame with locations + names
  all_pos <- as.matrix(expand.grid(seq_len(nrow(m)), seq_len(ncol(m))))
  for (r in seq_len(nrow(all_pos))) {
    cur_pos <- all_pos[r, , drop = FALSE]
    d <- as.matrix(dist(rbind(cur_pos, day6_m), method = "manhattan"))
    # gives a vector with manhattan distances to all other positions
    m[cur_pos] <- FUN(d)
  }
  return(m)
}

m_pop <- populate_m(m, day6_m, task1_fun)

peripherals <- unique(c(m_pop[1, ], m_pop[nrow(m_pop), ], 
  m_pop[, 1], m_pop[, ncol(m_pop)]))

my_rle <- rle(sort(m_pop))
result <- data.frame(loc = my_rle$values, area = my_rle$lengths, 
  stringsAsFactors = FALSE)
result$in_peripherals <- result$loc %in% peripherals
result <- result[order(result$area, decreasing = TRUE), ]

cat("Day 6, task 1:", max(result$area[!result$in_peripherals]))

# task 2
task2_fun <- function(d) {
  sum(d[1, ][-1])
}

m2 <- matrix(0, nrow = max(day6$r), ncol = max(day6$c))
m_pop2 <- populate_m(m2, day6_m, FUN = task2_fun)

cat("Day 6, task 2:", sum(m_pop2 < 10000))

