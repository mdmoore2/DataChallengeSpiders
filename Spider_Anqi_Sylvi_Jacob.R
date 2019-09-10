arthropod <- read_csv("data/cole_arthropod_data_1946.csv")
weevil <- read_csv("data/mitchell_weevil_egg_data_1975.csv")

numSpider <-arthropod$C_count_of_boards_with_k_spiders * arthropod$ k_number_of_arthropods
lambda_spiders <- mean(numSpider)

numSowbug <-arthropod$C_count_of_boards_with_k_sowbugs * arthropod$ k_number_of_arthropods
lambda_sowbug <- mean(numSowbug)

numWeevil <-weevil$k_number_of_eggs * weevil$C_count_of_beans_with_k_eggs
lambda_weevil <- mean(numWeevil)

norm_number_spider <- arthropod$C_count_of_boards_with_k_spiders/sum(numSpider)
plot(arthropod$k_number_of_arthropods, norm_number_spider)

poisSpider <- dpois(arthropod$k_number_of_arthropods, lambda_spiders, log = FALSE)
plot(poisSpider, type = "l") 

spider <- cbind((0:17), numSpider, poisSpider, norm_number_spider)
spider <- as.data.frame(spider)
colnames(spider) <- ("num_arthropod", "numSpider", "poisSpider", "norm_number_spider")
ggplot(data = spider) + 
  geom_smooth(mapping = aes(x = num_arthropod, y = poisSpider))+
  geom_point(mapping = aes(x = num_arthropod, y = norm_number_spider))