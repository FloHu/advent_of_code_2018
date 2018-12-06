(day6 <- read.table("input_day06.txt", sep = ",", 
  col.names = c('r', 'c')))
day6$loc <- do.call(paste0, expand.grid(letters, letters))[1:nrow(day6)]
day6

# get coordinates of 'outer ring' of variables (will have infinite area)
# make again a maximum size matrix
# write function to calculate Manhattan distance of a pair of points
# go through each coordinate and 'write down' the closest location
# then destroy matrix and sort lengths

# might be able to work with a distance matrix:
dist(data.frame(a = c(1, 1), b = c(1, 6)), method = "manhattan")
