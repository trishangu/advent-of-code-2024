library(dplyr)

in_order <- read.table(file = "day-5/day-5-order.txt", sep = "|")
input <- read.table(file = "day-5/day-5-input.txt",fill = TRUE, header = FALSE)

char_list <- strsplit(input$V1, ",")

max_length <- max(sapply(char_list, length))

# Pad shorter lines with NA and transpose
result <- do.call(rbind, lapply(char_list, function(x) {
  length(x) <- max_length  # Pad with NA
  return(x)
}))

input <- as.data.frame(result, stringsAsFactors = FALSE)

in_order <- in_order |>
  rename(First = V1,
         second = V2)

summation = 0
for(i in 1:nrow(input)) {
  now_input <- unlist(as.vector(input[i,]))
  now_input <- na.omit(now_input)
  current = 1
  for (j in 1:(length(now_input)-1)) {
    num <- as.numeric(now_input[j])
    check_num <- now_input[(j+1):length(now_input)]
    now_order <- in_order |>
      dplyr::filter(First == num)
    if(!all(check_num %in% now_order$second)) {
      current = 0
      break
    }
  }
  if(current == 1) {
    cat("row", i, "\n")
    middle_number <- as.numeric(now_input[(length(now_input) + 1) / 2])
    summation = summation + middle_number
  }
}
