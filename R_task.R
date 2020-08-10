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
  if (is.na(cut.points[1]))
    cut.points[1] <- min(input.data[[dates.column.name]])
  if (is.na(cut.points[2]))
    cut.points[2] <- max(input.data[[dates.column.name]])
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
  
  #Zamiana niskich wartosci---------------------------------------------------------------------------
  if (is.na(threshold))
    threshold = 2
  output.data[output.data < threshold] <- 0
  
  
  #Ustawienie separatora i znaku dziesietnego i Export
  output.data <- output.data[,!(colnames(output.data) %in% variables.colnames)]
  # output.data <- format(output.data, decimal.mark=dec, scientific=FALSE)
  write.table(output.data, file="output.csv", sep=sep, row.names=FALSE)
  as.numeric(sub(".", dec, output.data[-(1:1)], fixed=TRUE))
  return(output.data)
  
}

#koniec--------------------------------------------------------------------------------------------------------


main.tab <- read.csv2("D:\\R_data\\R_dev_task_project\\R_dev_task\\R_dev_task.csv")
c.points <- c("2017-02-06","2017-04-30")
result <- Modifier(input.data=main.tab, threshold=1, dates.column.name="Datyy", dec=",", sep="/")

debug(Modifier)
Modifier(input.data=main.tab, threshold=1 , dates.column.name="Datyy",  cut.points=c.points, dec=",", sep="/")
undebug(Modifier)


main.tab
main.tab[["Date"]] <- as.Date(main.tab[["Date"]])  
min(main.tab[["Date"]])

main.tab$KN_XXX_30[6] <- "   "

result[7,2] + 1

result[result < 20] <- 0

str(result)

as.numeric(result$KN00_XXX_30)

as.numeric(result[-(1:1)])

is.list(result$KN00_XXX_30)
is.character(result$KN00_XXX_30)
