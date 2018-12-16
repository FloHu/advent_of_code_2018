v <- c(2, 3, 
        0, 3, 10, 11, 12, 
        1, 1, 
          0, 1, 99, 
          2, 
        1, 1, 2) # should be 138
bigtree <- readLines("input_day08.txt")
bigtree <- as.numeric(unlist(strsplit(bigtree, split = " ")))

treecutter <- function(v) {
  nchild <- v[1]
  nmeta <- v[2]
  branch <- v[-c(1, 2)]
  total_sum <- 0

  while (nchild > 0) {
    nchild <- nchild - 1
    subtree <- treecutter(branch)
    branch <- subtree$branch
    # numeric(0) + 3 is numeric(0)!!!
    total_sum <- sum(c(total_sum, subtree$total_sum))
  }
  
  total_sum <- total_sum + sum(branch[seq_len(nmeta)])
  branch <- branch[-seq_len(nmeta)]
  return(list(total_sum = total_sum, branch = branch))
}

treecutter(v)
res <- treecutter(bigtree)

cat("Day 8, task 1:", res$total_sum, "\n")

v <- c(2, 3, 
        0, 3, 10, 11, 12, 
        1, 1, 
          0, 1, 99, 
          2, 
        1, 1, 2) # should be 138
# Task 2:
treecutter2 <- function(v) {
  nchild <- v[1]
  i <- nchild
  nmeta <- v[2]
  branch <- v[-c(1, 2)]
  total_sum <- 0
  childsums <- numeric()
  
  while (i > 0) {
    i <- i - 1
    subtree <- treecutter2(branch)
    branch <- subtree$branch
    total_sum <- sum(c(total_sum, subtree$total_sum))
    childsums <- c(childsums, subtree$childsums)
  }

  metadata <- branch[seq_len(nmeta)]
  total_sum <- total_sum + sum(metadata)
  if (nchild == 0) {
    childsums <- sum(metadata)
  } else {
    childsums <- childsums[metadata]
    childsums[is.na(childsums)] <- 0
    childsums <- sum(childsums)
  }
  branch <- branch[-seq_len(nmeta)]
  
  return(list(total_sum = total_sum, branch = branch, childsums = childsums, 
    metadata = metadata))
}

# treecutter2(v)
cat("Day 8, task 2:", treecutter2(bigtree)$childsums, "\n")

