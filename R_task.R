modifier <-function(input.data,
                    min=0,
                    max=90,
                    step=10,
                    threshold=NULL,
                    dates_col_name="Date",
                    cut_points=c(NA,NA),
                    sep=";",
                    dec=","){
  
  variantsCount <- (max-min)/step
  variablesColnames <- colnames(input.data)[-(1:1)]
  
  for (v in variantsCount){
    for (n in variablesColnames){
      
    }
  }
  return(input.data)
  
}


main_tab <- read.csv2("D:\\R_data\\R_dev_task.csv")

modifier(input.data = main_tab)
length(colnames(main_tab))
colnames(main_tab)[-(1:1)]
 