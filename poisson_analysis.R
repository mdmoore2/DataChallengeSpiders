library(tidyverse)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#bring up the data as file-specific dataframes
cole <- read_csv('data/cole_arthropod_data_1946.csv')
mitchell <- read_csv('data/mitchell_weevil_egg_data_1975.csv')

#calculate the mean from a given column
MeanSpider <- mean(cole$C_count_of_boards_with_k_spiders)

#create a varaible that represents the poisson distribution
poisson_spiders <- dpois(cole$C_count_of_boards_with_k_spiders, MeanSpider, log=FALSE)

#adding dist to cole data frame
cole <- cbind(cole, poisson_spiders)

#plot the two plots on top of eachother using ggplot
spider_plot <- ggplot(cole) + 
  aes(x=k_number_of_arthropods, y=C_count_of_boards_with_k_spiders) + 
  geom_point() +
  theme_minimal() +
  xlab('Number seen') +
  ylab('Occurences')




