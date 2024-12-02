input <- read.table("day-2/day-2-input.txt", header = FALSE, fill = TRUE, row.names = NULL)

safe <- 0
for (i in 1:nrow(input)) {
  cleaned_row <- as.numeric(na.omit(as.numeric(input[i,])))
  difference <- diff(cleaned_row)
  if (all(abs(difference) > 0) & all(abs(difference) < 4) & 
      (all(diff(cleaned_row) > 0) | all(diff(cleaned_row) < 0))) {
    safe <- safe + 1
  } else {
    counter = 0
    current_sign <- names(which.max(table(sign(difference))))
    num_times <- (abs(difference) > 3 | 
                    abs(difference) < 1 | 
                    sign(difference) != as.numeric(current_sign)
    )
    one_true <- sum(num_times) > 0
    if (one_true == TRUE) {
      pos_true <- which(num_times)[1]
      new_cleaned_row <- cleaned_row[-(pos_true+1)]
      new_difference <- diff(new_cleaned_row)
      if (all(abs(new_difference) > 0) & all(abs(new_difference) < 4) & 
          (all(diff(new_cleaned_row) > 0) | all(diff(new_cleaned_row) < 0))) {
        safe <- safe + 1
        counter <- 1
      }
    }
    if (one_true == TRUE & counter != 1) {
      pos_true <- which(num_times)[1]
      new_cleaned_row <- cleaned_row[-(pos_true)]
      new_difference <- diff(new_cleaned_row)
      if (all(abs(new_difference) > 0) & all(abs(new_difference) < 4) & 
          (all(diff(new_cleaned_row) > 0) | all(diff(new_cleaned_row) < 0))) {
        safe <- safe + 1
      }
    }
  }
}
