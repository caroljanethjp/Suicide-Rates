#
# Este script  se construyó a partir del dataset obtenido en la Limpieza de datos
# y permitirá encontrar un foco : de los 10 países a nivel mundial
# con mayor tendencia creciente de suicidios  en los ultimos años(2010-2015).
# Y a partir de esto, se mostrará en que grupo de edades y género se concentra más este fenómeno,
# con el fin de aplicar una  prevención más delimitada.
#


########## Librerías ############

library(tidyverse)
library(broom)


########## Lectura ############

dt <- read_csv("data/master_clean.csv")


########## Obteniendo las tendencias de suicidios en cada país  ############
##########            desde 1985-2015         ############

country_year <- dt %>%
  group_by(country, year) %>%
  summarize(suicides = sum(suicides_no), 
            population = sum(population), 
            suicide_per_100k = (suicides / population) * 100000)
                

country_year_trends <- country_year %>%
  ungroup()%>%
  nest(-country)%>% 
  mutate(model = map(data, ~ lm(suicide_per_100k ~ year, data = .)),
         tidied = map(model, tidy))%>% 
  unnest(tidied)


# me quedo con aquellos valores más significativos de los modelos (p< 0.05)
country_year_sig_trends <- country_year_trends %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  filter(p.adjusted < .05) %>%
  arrange(estimate)


#Obtengo los 10 países con tendencia más creciente en todo el dataset
top10_increasing <- tail(country_year_sig_trends$country, 10)



########## Delimitando los 10 top países por grupo de edades y género ############

data_filtered <- dt %>%
  filter(country %in% top10_increasing)


#por género
top_10_gender <- data_filtered %>%
  filter(year >= 2010) %>%
  group_by(country, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 
  
  
#por edad
top_10_age <-  data_filtered %>%
  filter(year >= 2010) %>%
  group_by(country, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
  
#factorizando la columna edad 
data_filtered$age <- factor(data_filtered$age, 
                            ordered = T, 
                            levels = c("5-14",
                                       "15-24", 
                                       "25-34", 
                                       "35-54", 
                                       "55-74", 
                                       "75+"))



########## Visualizaciones ############

countries_top_10 <- country_year %>%
    filter(country %in% top10_increasing) %>%
    ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~ country) + 
    theme(legend.position = "none") + 
    ggtitle("Top 10 de países con tendencia más creciente")+
    labs(x = "Año", 
         y = "Suicidios por 100k")

countries_top_10

countries_top_10_gender <- ggplot(top_10_gender, aes(x = country, y = suicide_per_100k, fill = sex)) + 
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(title = "Porcentaje de suicidios por género", 
         subtitle = "en los 10 países top, 2010 - 2015", 
         x = "País", 
         y = "", 
         fill = "Género")


countries_top_10_age <- ggplot(top_10_age, aes(x = country, y = suicide_per_100k, fill = age)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Por edad", 
       subtitle = "2010 - 2015 ", 
       x = "País", 
       y = "Suicidios por 100k", 
       fill = "Edad")


grid.arrange(countries_top_10_gender, countries_top_10_age, nrow = 2)
