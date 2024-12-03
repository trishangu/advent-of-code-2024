input <- readLines("day-3/day-3-input.txt", warn = FALSE)
input <- str_c(input[1], input[2], input[3], input[4], input[5], input[6])

library(stringr)

donts <- stringr::str_locate_all(input, "don't\\(\\)")
dos <- stringr::str_locate_all(input, "do\\(\\)")

donts <- unlist(donts)
donts <- donts[1:ceiling(length(donts)/2)]

dos <- unlist(dos)
dos <- dos[1:ceiling(length(dos)/2)]

input_vec <- strsplit(input, "")[[1]]
pos_vector <- rep(TRUE, length(input_vec))

# Apply toggling based on wrong and right positions
for (i in seq_along(input_vec)) {
  if (i %in% donts) {
    pos_vector[i:length(pos_vector)] <- FALSE
  }
  if (i %in% dos) {
    pos_vector[i:length(pos_vector)] <- TRUE
  }
}

matches <- str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)")[[1]]

match_positions <- str_locate_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)")[[1]]

# Check which matches fall within TRUE positions
filtered_matches <- matches[sapply(1:nrow(match_positions), function(i) {
  start <- match_positions[i, "start"]
  end <- match_positions[i, "end"]
  any(pos_vector[start:end])
})]


correct <- str_remove_all(filtered_matches, "mul")
digits <- gsub("[()]", "", correct)

digits <- do.call(rbind, stringr::str_split(digits, ","))

digits <- data.frame(first = as.numeric(digits[, 1]), second = as.numeric(digits[, 2]))

digits <- digits |>
  dplyr::mutate(mul = first * second)

ans <- sum(digits$mul)
print(ans)
