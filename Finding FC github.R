#Uploading the libraries
library(openxlsx)
library(WriteXLS)

#Cleaning the enviroment
rm(list=ls())

#reading the bigdata sheet
data <- read.xlsx("data.xlsx", rowNames = TRUE)
data <- t(data)

## read the excel with the statistics (anova, mann-whitney or test t), 
#so we only use the p-value significant features
statistic <- read.xlsx("statistics.xlsx")
features <- unique(statistic$.y.)
data.filter <- data[match(features,rownames(data)),]

## read the excel with the lables of each sample
class <- read.xlsx("lables.xlsx", sheet=2)
rownames(class) <- class$ID

### average cases of the groups
less_median <- subset(class, class$`groups` == "less_median")
less_median <- rownames(less_median)
avg.less_median <- apply(data[,less_median], 1, mean)

above_median <- subset(class, class$`groups` == "above_median")
above_median <- rownames(above_median)
avg.above_median <- apply(data.filter[,above_median], 1, mean)



fc <- data.frame(avg.less_median, avg.above_median)


################## IF THE DATA IS NOT IN LOG 2 ####################
# calculate fc
fc$fc_less_median.above_median <- avg.less_median / avg.above_median
WriteXLS(fc, "FC.xlsx", row.names = TRUE)

#put the FC in Log2FoldChange
fc$log2fc <- log2(fc$fc_less_median.above_median)
WriteXLS(fc, "log2_FC.xlsx", row.names = TRUE)

### create the FC filter to above FC 1.5
features_filter <- subset(fc, fc$log2fc <= -0.48 |  fc$log2fc >= 0.48)
WriteXLS(features_filter, "Log2_FC_filter.xlsx", row.names = TRUE)

############### IF DATA IS ALREADY IN LOG 2 #######################
# calculate fc
fc$fc_less_median.above_median <- (avg.less_median - avg.above_median)
WriteXLS(fc, "FC.xlsx", row.names = TRUE)

#put the FC in Log2FoldChange
fc$log2fc <- log2(fc$fc_less_median.above_median)
WriteXLS(fc, "log2_FC.xlsx", row.names = TRUE)

### create the FC filter to above FC 1.5
features_filter <- subset(fc, fc$log2fc <= -0.48 |  fc$log2fc >= 0.48)
WriteXLS(features_filter, "Log2_FC_filter.xlsx", row.names = TRUE)