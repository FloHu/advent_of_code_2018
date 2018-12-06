library(lubridate)
library(tidyverse)

day4 <- tibble(input = readLines("input_day04.txt"))
day4$unparsed <- str_extract(day4$input, "^\\[.+\\]")
day4$event <- str_extract(day4$input, "(begins shift)|(falls asleep)|(wakes up)")
day4$date_and_time <- ymd_hm(day4$unparsed)
day4 <- arrange(day4, date_and_time)
head(day4)

on_duty <- character(length = nrow(day4))
my_pattern <- "Guard #\\d{1,}"
for (r in seq_len(nrow(day4))) {
  grepped <- str_extract(day4$input[r], pattern = my_pattern)
  if (!is.na(grepped)) {
    guard <- grepped
  }
  on_duty[r] <- guard
}
day4$on_duty <- on_duty
head(day4)

all_guards <- unique(day4$on_duty)

day4_s <- day4 %>%
  group_by(on_duty) %>%
  summarise(
    sleep_from = list(date_and_time[which(event == "falls asleep")]), 
    sleep_to = list(date_and_time[which(event == "wakes up")])
    )

day4_s$sleep_intervls <- map2(day4_s$sleep_to, day4_s$sleep_from, 
  function(.x, .y) {
    return(.x - .y)
  })

day4_s$total_sleep <- map2_dbl(day4_s$sleep_to, day4_s$sleep_from, 
  function(.x, .y) {
    return(sum(.x - .y))
  })

day4_s$sleep_rngs <- map2(day4_s$sleep_to, day4_s$sleep_from, 
  function(.x, .y) {
    mins_to <- minute(.x)-1
    mins_from <- minute(.y)
    return(mapply(FUN = `:`, mins_from, mins_to))
  })

day4_s$most_common_minute <- 
  lapply(day4_s$sleep_rngs, function(x) {
    if (length(x) == 0) return(list(0, 0))
    xrle <- rle(sort(unlist(x)))
    most_common <- xrle$values[which.max(xrle$lengths)]
    how_often <- max(xrle$lengths)
    return(list(most_common = most_common, how_often = how_often))
})

day4_s$how_often <- sapply(day4_s$most_common_minute, `[[`, 2)
day4_s$most_common_minute <- sapply(day4_s$most_common_minute, `[[`, 1)

day4_s <- arrange(day4_s, desc(total_sleep))
day4_s
cat("The answer to task 1 is ", 3167*45, "\n")
day4_s <- arrange(day4_s, desc(how_often))
day4_s
cat("The answer to task 1 is ", 179*30, "\n")

