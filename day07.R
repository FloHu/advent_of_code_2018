# example input from puzzle for testing purposes:
l <- list(
  'A' = c('C'), 
  'B' = c('A'), 
  'C' = character(), 
  'D' = c('A'), 
  'E' = c('B', 'F', 'D'), 
  'F' = c('C')
)

# Task 1:
instructions <- readLines("input_day07.txt")
instructions <- stringr::str_match(instructions, 
  pattern = "^Step ([A-Z]).+([A-Z]) can begin\\.")
# vector of pairs, where the name is the step and the value is 
# the condition for the step
pairs <- instructions[, 2]
names(pairs) <- instructions[, 3]

l <- list()
for (step in unique(names(pairs))) {
  l[[step]] <- unname(pairs[names(pairs) == step])
}

# need to add steps without a condition
nocond <- unique(unlist(l)[!unlist(l) %in% names(l)])
l[nocond] <- list(character())
# sort alphabetically
l <- l[order(names(l))]

path <- character()

find_unlocked <- function(l) {
  unlocked <- which(sapply(l, function(x) length(x) == 0))
  return(unlocked)
}

update_conds <- function(l, path) {
  l <- plyr::llply(l, function(x) {
    x <- x[!x %in% path]
    return(x)
  })
  return(l)
}

# for task 2:
l.bak <- l
path.bak <- path

while (TRUE) {
  unlocked <- find_unlocked(l)
  next_step <- names(l)[[min(unlocked)]]
  path <- c(path, next_step)
  l <- l[names(l) != next_step]
  l <- update_conds(l, path)
  if (length(l) == 0) break
}

cat("Day 7, task 1:", paste(path, collapse = ""), "\n")

# Task 2:
l <- l.bak
path <- path.bak
total_time <- 0
idle <- 5
queue <- numeric()

# the routine above gets more complicated:
# first, add a time attribute to each list element:
add_time_attributes <- function(l) {
  l <- purrr::imap(l, function(el, name) {
    attr(el, "duration") <- 60 + which(LETTERS == name)
    return(el)
  })
  return(l)
}

# (1) find unlocked steps
# (2) fill up the queue in the right order until no workers left/no 
#     steps possible
# (3) process the queue: 
#     a) subtract 1st time and add to total_time
#     b) remove all steps with time = 0, add them to path, update idle workers
# (4) update the list
# (5) go to 1

find_unlocked2 <- function(l, idle) {
  # modify so that it doesn't return more than idle workers
  unlocked <- which(sapply(l, function(x) length(x) == 0))
  if (length(unlocked) > idle) unlocked <- unlocked[seq_len(idle)]
  return(unlocked)
}

while (TRUE) {
  # annoyingly, attributes are usually lost and since I don't want to create 
  # my own S3 class I'll just readd them ...
  l <- add_time_attributes(l)
  # find unlocked steps: max amount = amount of idle workers
  unlocked <- find_unlocked2(l, idle)
  # update number of available workers
  idle <- idle - length(unlocked)
  # define queue, sort it and retrieve durations:
  if (length(unlocked) > 0) {
    queue <- c(queue, sapply(l[unlocked], function(x) attr(x, "duration")))
  }
  # update list already now!
  l <- l[!(names(l) %in% names(queue))]
  # put into right order
  queue <- queue[order(queue)]
  # process the queue:
  time_passed <- min(queue)
  total_time <- total_time + time_passed
  queue <- queue - time_passed
  # get all steps in the queue with length 0, update queue, free workers
  next_steps <- names(queue[queue == 0])
  queue <- queue[queue != 0]
  # free workers 
  idle <- idle + length(next_steps)
  path <- c(path, next_steps)
  l <- update_conds(l, path)
  if (length(l) == 0) break
}

# 1672 is wrong - too high




l <- list(
  a = 'b', 
  a = 'c', 
  b = 'd', 
  c = 'd', 
  d = ''
)

make_path <- function(key, l) {
  if (key %in% names(l)) {
    cat("Found", key, "\n")
    if (sum(names(l) %in% key) > 1) {
      # go through all list elements with that name, take 1st one alphab.
      opts <- vector('character')
      l2 <- l
      while (!is.null(l2[[key]])) {
        opts <- c(opts, l2[[key]])
        l2[[key]] <- NULL
      }
      make_path(sort(opts)[1], l)
    } else {
      if (exists("string", where = parent.frame())) {
        string <- c(get("string", envir = parent.frame()), key)
      } else {
        string <- key
      }
      cat("String:", string, "\n")
      make_path(l[[key]], l)
    }
  }
}

make_path('b', l)
make_path('a', l)

recur_eval_list <- function(arg) {
  # replace by tryCatch()
  if (is.name(arg[[1]])) {
    string <- 
      if (exists("string", where = parent.frame())) {
        string <- paste0(get("string", envir = parent.frame()), 
          names(arg), collapse = "")
      } else {
        string <- names(arg)
      }
    cat('String:', string, '\n')
    
    r <- eval(arg[[1]])
    recur_eval_list(r)
  } else {
    cat('- finished - \n')
  }
}

recur_eval_list(b)


f <- function() {
  avar <- 'hello'
  g()
}

g <- function() {
  # if (exists('avar', envir = parent.frame())) { # works!
  if (exists('avar', where = parent.frame()))
    cat('found avar!\n')
}


f()

recur_eval_list(b)


