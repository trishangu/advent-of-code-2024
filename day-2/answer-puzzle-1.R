input <- read.table("day-2/day-2-input.txt", header = FALSE, fill = TRUE, row.names = NULL)

safe <- 0
for (i in 1:nrow(input)) {
  cleaned_row <- as.numeric(na.omit(as.numeric(input[i,])))
  difference <- diff(cleaned_row)
  first_last <- cleaned_row[1] - cleaned_row[length(cleaned_row)]
  if(first_last > 0 & all(difference < 0) & all(abs(difference) > 0) & all(abs(difference) < 4)) {
    safe <- safe + 1
  } else if(first_last < 0 & all(difference > 0) & all(abs(difference) > 0) & all(abs(difference) < 4)) {
    safe <- safe + 1
  }
}
