library(stringr)
library(tibble)
library(purrr)

# TASK 1 ---------------------------------------
claims <- readLines("input_day03.txt")
pattern <- "#\\d{1,} @ (\\d{1,}),(\\d{1,}): (\\d{1,})x(\\d{1,})"
claims <- lapply(claims, function(x) {
                     str_match(x, pattern)
})

claims <- do.call(rbind, claims)
claims <- data.frame(claims, stringsAsFactors = FALSE)
claims[2:5] <- lapply(claims[2:5], as.numeric)
names(claims) <- c("input", "left_offset", "top_offset", "width", "height")
claims <- as_tibble(claims)

largest_width <- max(claims$left_offset + claims$width)
largest_height <- max(claims$top_offset + claims$height)

# add a list column containing a matrix with the largest dimensions
claims$matrices <- rep(list(matrix(F, nrow = largest_height, ncol = largest_width)), nrow(claims))

make_index_matrices <- function(left_offset, top_offset, width, height) {
    # inputs should all have length 1
    # the following creates vectors containing row and column 
    # indices and then uses expand.grid to create a matrix that 
    # can be used to subset from a larger matrix a submatrix with the 
    # positions and dimensions given by the arguments to this function
    rows <- c((top_offset + 1):(top_offset + height))
    cols <- c((left_offset + 1):(left_offset + width))
    m <- as.matrix(expand.grid(rows, cols))
    return(m)
}

claims$indexmats <- pmap(claims[, c("left_offset", "top_offset", "width", "height")], make_index_matrices)
claims$matrices <- map2(claims$matrices, claims$indexmats, function(m, i) {
                            m[i] <- TRUE
                            return(m)
})

overlaps_per_field <- reduce(claims$matrices, `+`)
overlaps <- sum(overlaps_per_field > 1)

cat("Number of overlapping fields: ", sum(overlaps), "\n")

# TASK 2 -----------------------------

row_inds <- seq(nrow(claims))
for (r in row_inds) {
    cat("Currently checking row ", r, "\n")
    current_mat <- claims$matrices[[r]]
    others <- row_inds[-r]
    for (s in others) {
        other_mat <- claims$matrices[[s]]
        if (any(current_mat & other_mat)) {
            break
        }
    }
    if (s == max(others)) {
        cat("Success! - ID is ", claims$input[r], "\n")
        break
    }
}

# try alternative solution using plotting 
# we need a data frame with row - col - sum_of_vals - ID
# the first 3 are easy - reduce with `+`
# to get the id one needs to replace the logical matrices with character 
# matrices that are pasted together
library(tidyverse)
claims2 <- select(claims, input, matrices)
claims2$input <- str_extract(claims2$input, "^#\\d{1,4}")

ID_matrix <- character(length = 999000)

for (i in (1:nrow(claims2))) {
  cat("Processing row ", i, "\n")
  m <- claims2$matrices[[i]]
  id <- claims2$input[[i]]
  m2 <- character(length = 999000)
  m2[as.vector(m)] <- id
  ID_matrix <- paste0(ID_matrix, m2)
}

ID_matrix <- matrix(ID_matrix, nrow = 1000, ncol = 999)

# all_vals <- reduce(claims2$matrices, `+`)
all_vals <- overlaps_per_field
rm(overlaps_per_field)
#all_ids <- reduce(claims2$ID_matrices, paste, sep = "")
#all_ids <- matrix(all_ids, nrow = 1000, ncol = 999)

all_vals_m <- reshape2::melt(all_vals)
all_ids_m <- reshape2::melt(ID_matrix)
all_ids_m$value <- as.character(all_ids_m$value)

p <- 
  ggplot(all_vals_m, aes(x = Var1, y = Var2, fill = value)) + 
  geom_raster() + 
  geom_text(data = all_ids_m, aes(x = Var1, y = Var2, label = value), 
    inherit.aes = FALSE, size = 0.2, alpha = 0.2)
ggsave(p, filename = "Day3.pdf", width = 32, height = 30)



