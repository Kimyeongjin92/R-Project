getwd()

install.packages('xlsx')
library(xlsx)

news <- read.csv('NewsResult_20120101-20190710.csv')
str(news)
dim(news)
