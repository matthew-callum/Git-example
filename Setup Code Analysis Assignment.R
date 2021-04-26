#First I install the relevant R Packages (only needs to be done once)
install.packages("vegan")
install.packages("ggplot2")
install.packages("tidyverse")

#Then I load the data sets into my R workspace (Needs to be done each time)
library(vegan)
library(ggplot2)
library(tidyverse)

#Then I add the custom code for the assignment
source("nes8010.R")

#First I need to bring in the two data sets into R
Pollen_Data <-read.csv("nafferton_pollen_altered.csv")
Farm_Data <-read.csv("nafferton_farm.csv")

#Check the data sets to make sure they entered correctly
summary(Farm_Data)
summary(Pollen_Data)
