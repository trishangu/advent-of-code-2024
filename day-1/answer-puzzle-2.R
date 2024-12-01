input <- read.table("day-1/day-1-input.txt")

column_1 <- sort(input$V1)
column_2 <- sort(input$V2)

tot_sim_score <- 0
for (i in 1:length(column_1)) {
  num_times <- sum(column_2 == column_1[i])
  sim_score <- column_1[i] * num_times
  tot_sim_score <- tot_sim_score + sim_score
}

cat(tot_sim_score)
