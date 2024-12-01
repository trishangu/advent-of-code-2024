input <- read.table("day-1/day-1-input.txt")

column_1 <- sort(input$V1)
column_2 <- sort(input$V2)
difference <- abs(column_1 - column_2)
sum(difference)
