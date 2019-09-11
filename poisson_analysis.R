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

plot_poisson_comp <- function(full_data,occurence_data,weight_data,animal){
  lambda <-  poison_mle(occurence_data, weight_data)
  
  if (animal == 'spider'){
    lambda_1 <-  lambda
    lambda_2 <-  0
  } else if (animal == 'snowbug'){
    lambda_1 <-  (1-0.53214)*lambda
    lambda_2 <-  0.53214
  } else if (animal == 'weavill'){
    repulsion = -.15
    lambda_1 <-  (1-repulsion)*lambda
    lambda_2 <-  repulsion
  }
  lgp_data <- 
  poisson_data <- dpois(occurence_data, lambda, log=FALSE) #create a varaible that represents the poisson distribution
  lagrange_data <- dLGP(occurence_data,lambda_1,lambda_2)
  #plot the two plots on top of eachother using ggplot
  plot_data <- ggplot(full_data) + 
    aes(x=occurence_data, y=weight_data/sum(weight_data)) + 
    geom_point(colour='blue') +
    theme_minimal() +
    xlab('Number seen') +
    ylab('Probability')
  
  plot_data <- plot_data + 
    geom_point(y=poisson_data, x=occurence_data, colour='red')
  plot_data <- plot_data + 
    geom_path(y=lagrange_data, x=occurence_data, colour='green')
  
  show(plot_data)
}

plot_poisson_comp(cole,cole$k_number_of_arthropods, cole$C_count_of_boards_with_k_spiders,"spider")
plot_poisson_comp(cole,cole$k_number_of_arthropods, cole$C_count_of_boards_with_k_sowbugs,"snowbug")
plot_poisson_comp(mitchell,mitchell$k_number_of_eggs,mitchell$C_count_of_beans_with_k_eggs,"weavill")


