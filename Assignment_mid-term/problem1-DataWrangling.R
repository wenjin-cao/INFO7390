

setwd("/Users/ling/Documents/INFO 7390 Data Science/Assignment/Mid-Term/problem1")
rawdata<- read.csv2("default of credit card clients.csv",header=T)

name = c("ID",         "LIMIT_BAL",  "SEX",         "EDUCATION" ,  "MARRIAGE",   "AGE", 
         "PAY_0",      "PAY_2",      "PAY_3",       "PAY_4",       "PAY_5",      "PAY_6", 
         "BILL_AMT1",  "BILL_AMT2",  "BILL_AMT3",   "BILL_AMT4",   "BILL_AMT5",  "BILL_AMT6",
         "PAY_AMT1",   "PAY_AMT2",   "PAY_AMT3",    "PAY_AMT4",    "PAY_AMT5",   "PAY_AMT6",
         "Y" )
colnames(rawdata) = name

rawdata = rawdata[-1,]
write.table(rawdata, "p1-ready-data.csv", sep="," ,row.name=F)

