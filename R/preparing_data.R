# initilizing the the packages --------------------------------------------


# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install()
# 
# BiocManager::install("rhdf5")


# read the files ----------------------------------------------------------

library(rhdf5)
library(dplyr)
library(stringr)
library(purrr)
library(xts)
dir <- "/Users/pooya/Desktop/M1 Data"
setwd(dir)

a <- rhdf5::h5ls("m1_data.mat", recursive = FALSE)
x <- h5read("m1_data.mat", "/m1_data")
cl <- x$close_m1_ser
hi <- x$high_m1_ser
lo <- x$low_m1_ser
op <- x$open_m1_ser
vol <- x$volume_m1_ser
x <- NULL
idx_daily <- R.matlab::readMat("m1_index_2D.mat")$m1.index.2D

stock_names <- R.matlab::readMat("Str_Data_2012.mat")$Stock.Names %>% unlist() %>% 
  str_split("_") %>% map_chr(1)

dates <- R.matlab::readMat("Str_Data_2012.mat")$Date.of.Stocks
days_num <- length(dates)

timerange1 <- "20160106 0900/20160106 1230"
daily_times <- format(timeBasedSeq(timerange1), "%H:%M:%S")
seq_times <- rep(daily_times, times = days_num)
seq_dates <- rep(dates, each = length(daily_times))

n <- nrow(cl)
for (i in 1:n) {
  close <- cl[i, ]
  high <- hi[i, ]
  low <- lo[i, ]
  open <- op[i, ]
  volume <- vol[i, ]
  data <- data.frame(date = seq_dates, time = seq_times, close = close, high = high, low = low, open = open, volume = volume)
  file_name <- paste0("ticker/", stock_names[i],".csv")
  write.csv(data, file = file_name, row.names = F, quote = F)
}





