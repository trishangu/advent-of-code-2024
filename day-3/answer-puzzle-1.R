input <- readLines("day-3/day-3-input.txt", warn = FALSE)

library(stringr)

correct <- stringr::str_extract_all(input, pattern = "mul\\(\\d{1,3},\\d{1,3}\\)")
correct <- unlist(correct)

correct <- str_remove_all(correct, "mul")
digits <- gsub("[()]", "", correct)

digits <- do.call(rbind, stringr::str_split(digits, ","))

digits <- data.frame(first = as.numeric(digits[, 1]), second = as.numeric(digits[, 2]))

digits <- digits |>
  dplyr::mutate(mul = first * second)

ans <- sum(digits$mul)
print(ans)
