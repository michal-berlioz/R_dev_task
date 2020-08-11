install.packages("tidyverse")
library(dplyr)
library(stringr)

Modifier <- function(input.data,
                     min = 0,
                     max = 90,
                     step = 10,
                     threshold = NULL,
                     date.name = "Date",
                     cut.points = c(NA,NA),
                     sep = ";",
                     dec = ","){
  
  #Sprawdzenie brakujacych danych------------------------------------------------------
  var.names <- colnames(input.data)[-(1:1)]
  input.data[var.names] <- sapply(input.data[var.names], as.numeric)
  if (!identical(which(is.na(input.data)), integer(0))) 
    stop("plik zrodlowy zawiera brakujace dane")
  
  #Formatowanie daty--------------------------------------------------------------------
  colnames(input.data)[1] <- date.name
  input.data[[date.name]] <- as.Date(input.data[[date.name]])  
  
  #Filtrowanie--------------------------------------------------------------------------
  if (is.na(cut.points[1]))
    cut.points[1] <- min(input.data[[date.name]])
  if (is.na(cut.points[2]))
    cut.points[2] <- max(input.data[[date.name]])
  output.data <- input.data %>%
    dplyr::filter(.[[date.name]] >= cut.points[1] & .[[date.name]] <= cut.points[2])
  
  #Dodawanie wariantow------------------------------------------------------------------
  variants.count <- (max-min)/step + 1  
  for (variable in var.names){
    s <- 0
    for (variant in c(1:variants.count)){
      new.var.name <- paste(substr(variable, 1, 2), str_pad(s, 2, pad = "0"),
                            substr(variable, 3, 10), sep = "")
      output.data[new.var.name] <- c(stats::filter(output.data[variable], 
                                                   c((s/100)), method = "recursive")) 
      #poz. na liscie wspolczynnikow = rzad opoznienia
      s <- s + step  
    }
  }
  
  #Zamiana niskich wartosci-------------------------------------------------------------
  output.data <- output.data[,!(colnames(output.data) %in% var.names)]
  num.columns <- sapply(output.data, is.numeric)
  if (is.null(threshold))
    threshold <- 0.01 * median(unlist(output.data[num.columns]))
  output.data[output.data < threshold] <- 0
  
  
  #Formatowanie separatorow-------------------------------------------------------------
  output.data[num.columns] <- lapply(output.data[num.columns], round, 2)
  output.data[num.columns] <- apply(output.data[num.columns],
                                    2, function(x) formatC(x, format = "f",
                                                           drop0trailing = TRUE,
                                                           decimal.mark = dec))
  
  #Export------------------------------------------------------------------------------
  write.table(output.data, file = "output.csv", sep = sep, row.names = FALSE)
  print(output.data)
  return(output.data)
}

main.tab <- read.csv2("R_dev_task.csv")
c.points <- c("2017-02-06","2017-05-28")
result <- Modifier(input.data = main.tab,
                   min = 0,
                   max = 50,
                   step = 25,
                   threshold = 0.8,
                   cut.points = c.points,
                   date.name = "Daty",
                   dec = ".",
                   sep = "/")


