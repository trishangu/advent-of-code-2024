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

not_correct <- data.frame(matrix(nrow = 0, ncol = 23))
for(i in 1:nrow(input)) {
  now_input_raw <- unlist(as.vector(input[i,]))
  now_input <- na.omit(now_input_raw)
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
  if(current == 0) {
    not_correct <- rbind(not_correct, now_input_raw)
  }
}

summation <- 0
for(i in 1:nrow(not_correct)) {
  now_input <- unlist(as.vector(not_correct[i,]))
  now_input <- na.omit(now_input)
  
  j <- 1
  while (j < length(now_input)) {
    num <- as.numeric(now_input[j])
    now_order <- in_order |>
      dplyr::filter(First == num)
    for(k in (j+1):length(now_input)) {
      again <- 0
      next_num <- as.numeric(now_input[k])
      if(!(next_num %in% now_order$second)) {
        temp <- now_input[k]
        now_input[k] <- now_input[j]
        now_input[j] <- temp
        again <- 1
        break
      }
    }
    if(again == 0){
      j <- j + 1
    }
  }
  middle_number <- as.numeric(now_input[(length(now_input) + 1) / 2])
  summation = summation + middle_number
}

summation
