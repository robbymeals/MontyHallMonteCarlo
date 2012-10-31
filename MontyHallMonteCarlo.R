#### Monty Hall Monte Carlo
#### Rob Mealey
library(ggplot2)
library(RColorBrewer)
library(reshape2)
setwd('/home/rmealey/Dropbox/ObscureAnalytics')
source('MontyMonteSource.R')

trialLengths <- c(3,10,100,1000,10000,100000)
for (n in trialLengths){montyMonte(n,7,5,5,5)}
montyMonteTilePlot(df100,3,11)
