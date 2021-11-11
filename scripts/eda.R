library(tidyverse) # data manipulation
library(DataExplorer) # for faster EDA plotting  
library(here) # allocate file
library(dplyr) # data manipulation
library(ggplot2) # data visualization

screentime <- read.csv(here('data','screentime.csv'))
sleep <- read.csv(here('data','sleepdata.csv'))
mood <- read.csv(here('data','mood.csv'))
sleep <- sleep[-c(12)]


# Screen time
introduce(screentime)

plot_missing(screentime)
plot_missing(sleep)
plot_missing(mood)

introduce(sleep)
introduce(mood)

data_list <- list(screentime, sleep, mood)
plot_str(data_list, type = "r")

plot_str()
missing  

plot_correlation(na.omit(screentime), maxcat = 5L)
