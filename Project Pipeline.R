#install and load packages
library(caTools)
library(dplyr)


#set a working directory
setwd("C://Users//Haochen Zou//Desktop//R")
getwd()


#load dataset 1 of the multiple datasets
dataset1 = read.table("moscow_real_estate_sale.csv",header=F, sep=",")
dataset1


#load dataset 2 of the multiple datasets
dataset2 = read.table("moscow_real_estate_sale_2.csv",header=F, sep=",")
dataset2


#calculate the length of the big data
lengthOfBigData = nrow(dataset1) - 1 + nrow(dataset2) - 1
lengthOfBigData


#separate the dataset1 into three equal subsets
set.seed(20220731)
index1 = sample.split(dataset1$V1, SplitRatio = 1/3)
dataset1Subset1 = subset(dataset1, index1 == TRUE)
dataset1RemainSubset = subset(dataset1, index1 == FALSE)
index2 = sample.split(dataset1RemainSubset$V1, SplitRatio = 1/2)
dataset1Subset2 = subset(dataset1RemainSubset, index2 == TRUE)
dataset1Subset3 = subset(dataset1RemainSubset, index2 == FALSE)


#separate the dataset2 into three euqal subsets
index3 = sample.split(dataset2$V1, SplitRatio = 1/3)
dataset2Subset1 = subset(dataset2, index3 == TRUE)
dataset2RemainSubset = subset(dataset2, index3 == FALSE)
index4 = sample.split(dataset2RemainSubset$V1, SplitRatio = 1/2)
dataset2Subset2 = subset(dataset2RemainSubset, index4 == TRUE)
dataset2Subset3 = subset(dataset2RemainSubset, index4 == FALSE)


#calculate the number of distinct data elements in the two datasets
dataset1Distinct = dataset1 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, .keep_all = T)
dataset2Distinct = dataset2 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, .keep_all = T)
numberOfDistinctDataElement = nrow(dataset1Distinct) + nrow(dataset2Distinct)


#calculate the number of distinct data elements in the two datasets in T1
dataset1DistinctT1 = dataset1Subset1 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, .keep_all = T)
dataset2DistinctT1 = dataset2Subset1 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, .keep_all = T)
numberOfDistinctDataElementT1 = nrow(dataset1DistinctT1) + nrow(dataset2DistinctT1)


#calculate the number of distinct data elements in the two datasets in T2
dataset1DistinctT2 = dataset1Subset2 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, .keep_all = T)
dataset2DistinctT2 = dataset2Subset2 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, .keep_all = T)
numberOfDistinctDataElementT2 = nrow(dataset1DistinctT2) + nrow(dataset2DistinctT2)


#calculate the number of distinct data elements in the two datasets in T3
dataset1DistinctT3 = dataset1Subset3 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, .keep_all = T)
dataset2DistinctT3 = dataset2Subset3 %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, .keep_all = T)
numberOfDistinctDataElementT3 = nrow(dataset1DistinctT3) + nrow(dataset2DistinctT3)


#count the value of dataset 1 Pj in T1, T2, and T3
dataset1PjT1 = nrow(dataset1Subset1) -nrow(dataset1DistinctT1)
dataset1PjT2 = nrow(dataset1Subset2) -nrow(dataset1DistinctT2)
dataset1PjT3 = nrow(dataset1Subset3) -nrow(dataset1DistinctT3)


#count the value of dataset 2 Pj in T1, T2, and T3
dataset2PjT1 = nrow(dataset2Subset1) -nrow(dataset2DistinctT1)
dataset2PjT2 = nrow(dataset2Subset2) -nrow(dataset2DistinctT2)
dataset2PjT3 = nrow(dataset2Subset3) -nrow(dataset2DistinctT3)


#count the number of dataset 1 records with no null values in T1, T2, and T3
dataset1NoNullT1 = nrow(dataset1Subset1) - sum(is.na(dataset1Subset1$V1)) - sum(is.na(dataset1Subset1$V2)) - sum(is.na(dataset1Subset1$V3)) - sum(is.na(dataset1Subset1$V4)) - sum(is.na(dataset1Subset1$V5)) - sum(is.na(dataset1Subset1$V6)) - sum(is.na(dataset1Subset1$V7)) - sum(is.na(dataset1Subset1$V8)) - sum(is.na(dataset1Subset1$V9)) - sum(is.na(dataset1Subset1$V10)) - sum(is.na(dataset1Subset1$V11)) - sum(is.na(dataset1Subset1$V12)) - sum(is.na(dataset1Subset1$V13)) - sum(is.na(dataset1Subset1$V14))
dataset1NoNullT2 = nrow(dataset1Subset2) - sum(is.na(dataset1Subset2$V1)) - sum(is.na(dataset1Subset2$V2)) - sum(is.na(dataset1Subset2$V3)) - sum(is.na(dataset1Subset2$V4)) - sum(is.na(dataset1Subset2$V5)) - sum(is.na(dataset1Subset2$V6)) - sum(is.na(dataset1Subset2$V7)) - sum(is.na(dataset1Subset2$V8)) - sum(is.na(dataset1Subset2$V9)) - sum(is.na(dataset1Subset2$V10)) - sum(is.na(dataset1Subset2$V11)) - sum(is.na(dataset1Subset2$V12)) - sum(is.na(dataset1Subset2$V13)) - sum(is.na(dataset1Subset2$V14))
dataset1NoNullT3 = nrow(dataset1Subset3) - sum(is.na(dataset1Subset3$V1)) - sum(is.na(dataset1Subset3$V2)) - sum(is.na(dataset1Subset3$V3)) - sum(is.na(dataset1Subset3$V4)) - sum(is.na(dataset1Subset3$V5)) - sum(is.na(dataset1Subset3$V6)) - sum(is.na(dataset1Subset3$V7)) - sum(is.na(dataset1Subset3$V8)) - sum(is.na(dataset1Subset3$V9)) - sum(is.na(dataset1Subset3$V10)) - sum(is.na(dataset1Subset3$V11)) - sum(is.na(dataset1Subset3$V12)) - sum(is.na(dataset1Subset3$V13)) - sum(is.na(dataset1Subset3$V14))


#count the number of dataset 2 records with no null values in T1, T2, and T3
dataset2NoNullT1 = nrow(dataset2Subset1) - sum(is.na(dataset2Subset1$V1)) - sum(is.na(dataset2Subset1$V2)) - sum(is.na(dataset2Subset1$V3)) - sum(is.na(dataset2Subset1$V4)) - sum(is.na(dataset2Subset1$V5)) - sum(is.na(dataset2Subset1$V6)) - sum(is.na(dataset2Subset1$V7)) - sum(is.na(dataset2Subset1$V8)) - sum(is.na(dataset2Subset1$V9)) - sum(is.na(dataset2Subset1$V10)) - sum(is.na(dataset2Subset1$V11)) - sum(is.na(dataset2Subset1$V12)) - sum(is.na(dataset2Subset1$V13)) - sum(is.na(dataset2Subset1$V14)) - sum(is.na(dataset2Subset1$V15))
dataset2NoNullT2 = nrow(dataset2Subset2) - sum(is.na(dataset2Subset2$V1)) - sum(is.na(dataset2Subset2$V2)) - sum(is.na(dataset2Subset2$V3)) - sum(is.na(dataset2Subset2$V4)) - sum(is.na(dataset2Subset2$V5)) - sum(is.na(dataset2Subset2$V6)) - sum(is.na(dataset2Subset2$V7)) - sum(is.na(dataset2Subset2$V8)) - sum(is.na(dataset2Subset2$V9)) - sum(is.na(dataset2Subset2$V10)) - sum(is.na(dataset2Subset2$V11)) - sum(is.na(dataset2Subset2$V12)) - sum(is.na(dataset2Subset2$V13)) - sum(is.na(dataset2Subset2$V14)) - sum(is.na(dataset2Subset2$V15))
dataset2NoNullT3 = nrow(dataset2Subset3) - sum(is.na(dataset2Subset3$V1)) - sum(is.na(dataset2Subset3$V2)) - sum(is.na(dataset2Subset3$V3)) - sum(is.na(dataset2Subset3$V4)) - sum(is.na(dataset2Subset3$V5)) - sum(is.na(dataset2Subset3$V6)) - sum(is.na(dataset2Subset3$V7)) - sum(is.na(dataset2Subset3$V8)) - sum(is.na(dataset2Subset3$V9)) - sum(is.na(dataset2Subset3$V10)) - sum(is.na(dataset2Subset3$V11)) - sum(is.na(dataset2Subset3$V12)) - sum(is.na(dataset2Subset3$V13)) - sum(is.na(dataset2Subset3$V14)) - sum(is.na(dataset2Subset3$V15))


#summarize the number of records with no null values in multiple datasets in T1, T2, and T3
recordNoNullT1 = dataset1NoNullT1 + dataset2NoNullT1
recordNoNullT2 = dataset1NoNullT2 + dataset2NoNullT2
recordNoNullT3 = dataset1NoNullT3 + dataset2NoNullT3


#dataset preparation for box plot
dataset1ForBoxPlotT1 = dataset1Subset1
dataset1ForBoxPlotT1 = dataset1ForBoxPlotT1[-1,]
dataset2ForBoxPlotT2 = dataset2Subset2
dataset2ForBoxPlotT2 = dataset2ForBoxPlotT2[-1,]


#consider the house price as the main value
dataset1NumericT1 = as.numeric(unlist(dataset1ForBoxPlotT1$V3))
dataset1NumericT2 = as.numeric(unlist(dataset1Subset2$V3))
dataset1NumericT3 = as.numeric(unlist(dataset1Subset3$V3))
dataset2NumericT1 = as.numeric(unlist(dataset2Subset1$V3))
dataset2NumericT2 = as.numeric(unlist(dataset2ForBoxPlotT2$V3))
dataset2NumericT3 = as.numeric(unlist(dataset2Subset3$V3))


#dataset 1 box plot analysis
boxplot.stats(dataset1NumericT1, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset1T1 = nrow(dataset1Subset1) - sum(dataset1NumericT1<20000) - sum(dataset1NumericT1>44445000)
boxplot.stats(dataset1NumericT2, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset1T2 = nrow(dataset1Subset2) - sum(dataset1NumericT2<45000) - sum(dataset1NumericT2>44426560)
boxplot.stats(dataset1NumericT3, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset1T3 = nrow(dataset1Subset3) - sum(dataset1NumericT3<10500) - sum(dataset1NumericT3>44079152)


#dataset 2 box plot analysis
boxplot.stats(dataset2NumericT1, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset2T1 = nrow(dataset2Subset1) - sum(dataset2NumericT1<1.71000) - sum(dataset2NumericT1>39.03820)
boxplot.stats(dataset2NumericT2, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset2T2 = nrow(dataset2Subset2) - sum(dataset2NumericT2<0.45000) - sum(dataset2NumericT2>39.17800)
boxplot.stats(dataset2NumericT3, coef=1.5, do.conf=TRUE, do.out=TRUE)
acceptableDataNumberDataset2T3 = nrow(dataset2Subset3) - sum(dataset2NumericT3<2.1000) - sum(dataset2NumericT3>38.70000)


#acceptable record sum in T1, T2, and T3
acceptableDataNumberT1 = acceptableDataNumberDataset1T1 + acceptableDataNumberDataset2T1
acceptableDataNumberT2 = acceptableDataNumberDataset1T2 + acceptableDataNumberDataset2T2
acceptableDataNumberT3 = acceptableDataNumberDataset1T3 + acceptableDataNumberDataset2T3


#length of big dataset in T1, T2, and T3
lengthOfBigDataT1 = nrow(dataset1Subset1) + nrow(dataset2Subset1) - 1
lengthOfBigDataT2 = nrow(dataset1Subset2) + nrow(dataset2Subset2) 
lengthOfBigDataT3 = nrow(dataset1Subset3) + nrow(dataset2Subset3) - 1


#data proprocess for the big data compliance analysis
dataset1ForComplianceT1 = dataset1Subset1
dataset1ForComplianceT1 = dataset1ForComplianceT1[-1,]
dataset2ForComplianceT2 = dataset2Subset2
dataset2ForComplianceT2 = dataset2ForComplianceT2[-1,]


#number of compliant data in dataset 1 in T1, T2, and T3
dataset1PriceT1 = as.numeric(unlist(dataset1ForComplianceT1[,c(3)]))
dataset1AreaT1 = as.numeric(unlist(dataset1ForComplianceT1[,c(12)]))
numberOfCompliantDataset1T1 = nrow(dataset1ForComplianceT1) - sum((dataset1PriceT1 / dataset1AreaT1) < 68578.83) - sum((dataset1PriceT1 / dataset1AreaT1) > 483941.18) 
dataset1PriceT2 = as.numeric(unlist(dataset1Subset2[,c(3)]))
dataset1AreaT2 = as.numeric(unlist(dataset1Subset2[,c(12)]))
numberOfCompliantDataset1T2 = nrow(dataset1Subset2) - sum((dataset1PriceT2 / dataset1AreaT2) < 68578.83) - sum((dataset1PriceT2 / dataset1AreaT2) > 483941.18) 
dataset1PriceT3 = as.numeric(unlist(dataset1Subset3[,c(3)])) 
dataset1AreaT3 = as.numeric(unlist(dataset1Subset3[,c(12)]))
numberOfCompliantDataset1T3 = nrow(dataset1Subset3) - sum((dataset1PriceT3 / dataset1AreaT3) < 68578.83) - sum((dataset1PriceT3 / dataset1AreaT3) > 483941.18) 


#number of compliant data in dataset 1 in T1, T2, and T3
dataset2PriceT1 = as.numeric(unlist(dataset2Subset1[,c(3)]))
dataset2AreaT1 = as.numeric(unlist(dataset2Subset1[,c(12)]))
numberOfCompliantDataset2T1 = nrow(dataset2Subset1) - sum((dataset2PriceT1 / dataset2AreaT1)*1000000 < 68578.83, na.rm = 1) - sum((dataset2PriceT1 / dataset2AreaT1) > 483941.18, na.rm = 1) 
dataset2PriceT2 = as.numeric(unlist(dataset2Subset2[,c(3)]))
dataset2AreaT2 = as.numeric(unlist(dataset2Subset2[,c(12)]))
numberOfCompliantDataset2T2 = nrow(dataset2Subset2) - sum((dataset2PriceT2 / dataset2AreaT2)*1000000 < 68578.83, na.rm = 1) - sum((dataset2PriceT2 / dataset2AreaT2) > 483941.18, na.rm = 1) 
dataset2PriceT3 = as.numeric(unlist(dataset2Subset3[,c(3)]))
dataset2AreaT3 = as.numeric(unlist(dataset2Subset3[,c(12)]))
numberOfCompliantDataset2T3 = nrow(dataset2Subset3) - sum((dataset2PriceT3 / dataset2AreaT3)*1000000 < 68578.83, na.rm = 1) - sum((dataset2PriceT3 / dataset2AreaT3) > 483941.18, na.rm = 1) 


#length of dataset1 in T1, T2, and T3
lengthOfDataset1T1 = nrow(dataset1Subset1)
lengthOfDataset1T2 = nrow(dataset1Subset2)
lengthOfDataset1T3 = nrow(dataset1Subset3)


#length of dataset2 in T1, T2, and T3
lengthOfDataset2T1 = nrow(dataset2Subset1)
lengthOfDataset2T2 = nrow(dataset2Subset2)
LengthOfDataset2T3 = nrow(dataset2Subset3)