input <- readLines("day-4/day-4-input.txt")

char_list <- strsplit(input, "")

max_length <- max(sapply(char_list, length))

# Pad shorter lines with NA and transpose
result <- do.call(cbind, lapply(char_list, function(x) {
  length(x) <- max_length  # Pad with NA
  return(x)
}))

counter = 0
for (i in 2:(nrow(result)-1)) {
  for (j in 2:(ncol(result)-1)) {
    if (result[i,j] == "A") {
      if (result[i-1, j-1] == "S" & result[i+1, j+1] == "M" & result[i+1,j-1] == "M" & result[i-1,j+1] == "S") {
        counter = counter + 1
      }
      if (result[i-1, j-1] == "M" & result[i+1, j+1] == "S" & result[i+1,j-1] == "S" & result[i-1,j+1] == "M") {
        counter = counter + 1
      }
      if (result[i-1, j-1] == "S" & result[i+1, j+1] == "M" & result[i+1,j-1] == "S" & result[i-1,j+1] == "M") {
        counter = counter + 1
      }
      if (result[i-1, j-1] == "M" & result[i+1, j+1] == "S" & result[i+1,j-1] == "M" & result[i-1,j+1] == "S") {
        counter = counter + 1
      }
    }
  }
}
