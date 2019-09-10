library(tidyverse)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#function to correct occurence data to give proper average of bug occerence
poison_mle<- function(occurence_data, weight_data){
  lambda = 0
  for (i in 1:length(occurence_data)){
    lambda <- lambda + occurence_data[i]*weight_data[i]
  }
  lambda <- lambda/ sum(occurence_data)
}
#bring up the data as file-specific dataframes
cole <- read_csv('data/cole_arthropod_data_1946.csv')
mitchell <- read_csv('data/mitchell_weevil_egg_data_1975.csv')

#calculate the mean from a given column
MeanSpider <- poison_mle(cole$k_number_of_arthropods, cole$C_count_of_boards_with_k_spiders)

#create a varaible that represents the poisson distribution
poisson_spiders <- dpois(cole$k_number_of_arthropods, MeanSpider, log=FALSE)

#adding dist to cole data frame
cole <- cbind(cole, poisson_spiders)

#plot the two plots on top of eachother using ggplot
spider_plot <- ggplot(cole) + 
  aes(x=k_number_of_arthropods, y=C_count_of_boards_with_k_spiders/sum(C_count_of_boards_with_k_spiders)) + 
  geom_point(colour='blue') +
  theme_minimal() +
  xlab('Number seen') +
  ylab('Probability')

spider_plot <- spider_plot + 
  geom_point(y=cole$poisson_spiders, x=cole$k_number_of_arthropods, colour='red')
  
show(spider_plot)


