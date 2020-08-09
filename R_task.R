install.packages("tidyverse")
library(dplyr)

Modifier <-function(input.data,
                    min=0,
                    max=90,
                    step=10,
                    threshold=NULL,
                    dates.column.name="Date",
                    cut.points=c(NA,NA),
                    sep=";",
                    dec=","){
  
  colnames(input.data)[1] <- dates.column.name
  input.data[[dates.column.name]] <- as.Date(input.data[[dates.column.name]])
  variants.count <- (max-min)/step + 1
  variables.colnames <- colnames(input.data)[-(1:1)]
  output.data <- input.data %>%
    filter(.[[dates.column.name]] > cut.points[1] & .[[dates.column.name]] < cut.points[2])
  for (variant in variants.count){
    s <- 0
    for (variable in variables.colnames){
      s <- s + step  
    }
  }
  return(output.data)
  
}


main.tab <- read.csv2("D:\\R_data\\R_dev_task_project\\R_dev_task\\R_dev_task.csv")

c.points <- c("2017-02-01","2017-04-30")
result <- Modifier(input.data=main.tab, dates.column.name="Datyy",  cut.points=c.points)

