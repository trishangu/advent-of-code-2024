input <- readLines("day-4/day-4-input.txt")

counter = 0

for(i in 1:length(input)){
  pos <- stringr::str_locate_all(input[i], "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

for(i in 1:length(input)){
  reverse <- stringi::stri_reverse(input[i])
  pos <- stringr::str_locate_all(reverse, "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

char_list <- strsplit(input, "")

max_length <- max(sapply(char_list, length))

# Pad shorter lines with NA and transpose
result <- do.call(cbind, lapply(char_list, function(x) {
  length(x) <- max_length  # Pad with NA
  return(x)
}))

ver_input <- apply(result, 1, function(row) {
  paste(na.omit(row), collapse = "")
})

for(i in 1:length(ver_input)){
  pos <- stringr::str_locate_all(ver_input[i], "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

for(i in 1:length(ver_input)){
  reverse <- stringi::stri_reverse(ver_input[i])
  pos <- stringr::str_locate_all(reverse, "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

d <- row(result) - col(result)
diagonal <- split(result, d)

max_length <- max(sapply(diagonal, length))

# Pad shorter lines with NA and transpose
result_diag <- do.call(rbind, lapply(diagonal, function(x) {
  length(x) <- max_length
  return(x)
}))

diagonal <- apply(result_diag, 1, function(row) {
  paste(na.omit(row), collapse = "")
})

for(i in 1:length(diagonal)){
  pos <- stringr::str_locate_all(diagonal[i], "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

for(i in 1:length(diagonal)){
  reverse <- stringi::stri_reverse(diagonal[i])
  pos <- stringr::str_locate_all(reverse, "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

rotate <- function(x) t(apply(x, 2, rev))

tra_result <- rotate(result)
d <- row(tra_result) - col(tra_result)
tra_diagonal <- split(tra_result, d)

max_length <- max(sapply(tra_diagonal, length))

# Pad shorter lines with NA and transpose
tra_result_diag <- do.call(rbind, lapply(tra_diagonal, function(x) {
  length(x) <- max_length
  return(x)
}))

tra_diagonal <- apply(tra_result_diag, 1, function(row) {
  paste(na.omit(row), collapse = "")
})

for(i in 1:length(tra_diagonal)){
  pos <- stringr::str_locate_all(tra_diagonal[i], "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}

for(i in 1:length(tra_diagonal)){
  reverse <- stringi::stri_reverse(tra_diagonal[i])
  pos <- stringr::str_locate_all(reverse, "XMAS")
  pos <- unlist(pos)
  counter <- counter + length(pos)/2
}
