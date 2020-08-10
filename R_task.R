install.packages("tidyverse")
library(dplyr)
library(stringr)

Modifier <-function(input.data,
                    min = 0,
                    max = 90,
                    step = 10,
                    threshold = NULL,
                    date.name = "Date",
                    cut.points = c(NA,NA),
                    sep = ";",
                    dec = ","){
  
  #Sprawdzenie brakujacych  danych---------------------------------------------------------------------------
  var.names <- colnames(input.data)[-(1:1)]
  input.data[var.names] <- sapply(input.data[var.names], as.numeric)
  if (!identical(which(is.na(input.data)), integer(0))) 
    stop("plik zrodlowy zawiera brakujace dane")
  
  #Formatowanie --------------------------------------------------------------------------------------
  colnames(input.data)[1] <- date.name
  input.data[[date.name]] <- as.Date(input.data[[date.name]])  

  #Filtrowanie----------------------------------------------------------------------------------------
  if (is.na(cut.points[1]))
    cut.points[1] <- min(input.data[[date.name]])
  if (is.na(cut.points[2]))
    cut.points[2] <- max(input.data[[date.name]])
  output.data <- input.data %>%
    dplyr::filter(.[[date.name]] >= cut.points[1] & .[[date.name]] <= cut.points[2])
 
  #Dodawanie wariantow--------------------------------------------------------------------------------
  variants.count <- (max-min)/step + 1  
  for (variable in var.names){
    s <- 0
    for (variant in c(1:variants.count)){
      new.column.name <- paste(substr(variable, 1, 2), str_pad(s, 2, pad = "0"), substr(variable, 3, 10), sep = "")
      output.data[new.column.name] <- c(stats::filter(output.data[variable], c((s/100)), method = "recursive")) #poz. na liscie wspolczynnikow = rzad opoznienia
      s <- s + step  
    }
  }
  
  #Zamiana niskich wartosci---------------------------------------------------------------------------
  ifelse(is.null(threshold),
    threshold <-  0.01 * median(unlist(output.data[-(1:1)])),
    output.data[output.data < threshold] <- 0)
  
  #Ustawienie separatora i znaku dziesietnego i Export------------------------------------------------
  output.data <- output.data[,!(colnames(output.data) %in% var.names)]
  output.data <- format(output.data, decimal.mark = dec)
  write.table(output.data, file = "output.csv", sep = sep, row.names = FALSE)
  return(output.data)
}


main.tab <- read.csv2("D:\\R_data\\R_dev_task_project\\R_dev_task\\R_dev_task.csv")
c.points <- c("2017-02-06","2017-04-30")
result <- Modifier(input.data=main.tab, min = 0, max = 50, step = 25, threshold = 0.8, cut.points = c.points, date.name = "Daty", dec = ",", sep = "/")




