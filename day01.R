#!/usr/local/bin/Rscript

v <- as.numeric(readLines("input_day01.txt"))
cat("Day 1, task 1: ", sum(v), "\n")

many_vs <- rep(v, 10000)
cat("Day 1, task 2: ", cumsum(many_vs)[min(which(duplicated(cumsum(many_vs))))], "\n")

