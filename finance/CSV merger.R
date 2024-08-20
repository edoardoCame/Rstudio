library(quantmod)
library(dplyr)
library(readr)


setwd("~/Desktop/merge data")


csv_filesList <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)


csv_filesRead <- lapply(X = csv_filesList, FUN = read_delim, delim = ";", escape_double = FALSE, col_names = FALSE, 
                                               trim_ws = TRUE)



#Let's concatenate all dataframes (imported from CSV files) with rbind in
#a chronological order:

merged_data <- do.call(rbind, csv_filesRead)[,1:5]
colnames(merged_data) <- c("Time","Open", "High", "Low", "Close") #rename the df with OHLC



#I transform the time-stamp into a POSIXlt
merged_data$Time <- as.POSIXlt(merged_data$Time, format = "%Y-%m-%d %H:%M:%S")

#I create a returns column using ROC function from TTR package
merged_data$Returns <- ROC(merged_data$Close)







chartSeries(merged_data)
