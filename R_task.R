install.packages("tidyverse")
library(dplyr)
library(stringr)

Modifier <-function(input.data,
                    min=0,
                    max=90,
                    step=10,
                    threshold=NULL,
                    dates.column.name="Date",
                    cut.points=c(NA,NA),
                    sep=";",
                    dec=","){
  
  #Sprawdzenie braku danych---------------------------------------------------------------------------
  variables.colnames <- colnames(input.data)[-(1:1)]
  input.data[variables.colnames] <- sapply(input.data[variables.colnames], as.numeric)
  if (!identical(which(is.na(input.data)), integer(0))) 
    stop("plik zrodlowy zawiera brakujace dane")
  
  #Formatowanie --------------------------------------------------------------------------------------
  colnames(input.data)[1] <- dates.column.name
  input.data[[dates.column.name]] <- as.Date(input.data[[dates.column.name]])  

  #Filtrowanie----------------------------------------------------------------------------------------
  output.data <- input.data %>%
    dplyr::filter(.[[dates.column.name]] >= cut.points[1] & .[[dates.column.name]] <= cut.points[2])
 
  #Dodawanie wariantow--------------------------------------------------------------------------------
  variants.count <- (max-min)/step + 1  
  for (variable in variables.colnames){
    s <- 0
    for (variant in c(1:variants.count)){
      new.column.name <- paste(substr(variable, 1, 2), str_pad(s, 2, pad="0"), substr(variable, 3, 10), sep="")
      output.data[new.column.name] <- c(stats::filter(output.data[variable], c((s/100)), method="recursive")) #poz. na liscie wspolczynnikow = rzad opoznienia
      s <- s + step  
    }
  }
  
  output.data <- output.data[,!(colnames(output.data) %in% variables.colnames)]
  output.data <- format(output.data, decimal.mark=dec)
  write.table(output.data, file="output.csv", sep=sep, row.names=FALSE)
  return(output.data)
  
}

#koniec--------------------------------------------------------------------------------------------------------


main.tab <- read.csv2("D:\\R_data\\R_dev_task_project\\R_dev_task\\R_dev_task.csv")
c.points <- c("2017-02-06","2017-04-30")
result <- Modifier(input.data=main.tab, dates.column.name="Datyy",  cut.points=c("2017-02-06","2017-04-30"), dec=".", sep="/")

debug(Modifier)
Modifier(input.data=main.tab, dates.column.name="Datyy",  cut.points=c.points, dec=",", sep="/")




main.tab


main.tab$KN_XXX_30[6] <- "   "


