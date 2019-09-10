library(tidyverse)
library(ggplot2)
library(RMKdiscrete)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#bring up the data as file-specific dataframes
cole <- read_csv('data/cole_arthropod_data_1946.csv')
mitchell <- read_csv('data/mitchell_weevil_egg_data_1975.csv')

#function to correct occurence data to give proper average of bug occerence
poison_mle<- function(occurence_data, weight_data){
  lambda = 0
  for (i in 1:length(occurence_data)){
    lambda <- lambda + occurence_data[i]*weight_data[i]
  }
  lambda <- lambda/ sum(weight_data)
}

plot_poisson_comp <- function(full_data,occurence_data,weight_data){
  lambda <- poison_mle(occurence_data, weight_data) #calculate poisson_mle
  poisson_data <- dpois(occurence_data, lambda, log=FALSE) #create a varaible that represents the poisson distribution
  
  #plot the two plots on top of eachother using ggplot
  plot_data <- ggplot(full_data) + 
    aes(x=occurence_data, y=weight_data/sum(weight_data)) + 
    geom_point(colour='blue') +
    theme_minimal() +
    xlab('Number seen') +
    ylab('Probability')
  
  plot_data <- plot_data + 
    geom_point(y=poisson_data, x=occurence_data, colour='red')
  show(plot_data)
}

plot_poisson_comp(cole,cole$k_number_of_arthropods, cole$C_count_of_boards_with_k_spiders)
plot_poisson_comp(cole,cole$k_number_of_arthropods, cole$C_count_of_boards_with_k_sowbugs)
plot_poisson_comp(mitchell,mitchell$k_number_of_eggs,mitchell$C_count_of_beans_with_k_eggs)


