#
# Este script construido a partir del dataset limpio, 
# mostrará la relación entre las variables de interés , lo cual  se usará para hallar el modelo de regresión
# Es así que este script se ejecuta antes del script Modelo.R
#




########## Librerías ############
library(tidyverse)
library(data.table)
library(broom)


########## Lectura ############
dt= read_csv("data/master_clean.csv")



########## Relación entre GDP y suicidios(por 100k) ############

#Se hallo la media  GDP  de cada país y  da un solo valor (punto) por cada país. 

media_gdp_pais= dt %>%
                group_by(country, continent) %>%
                summarize(meanGDP= mean(as.numeric(gdp_per_capita)),
                          suicidios_por_100k= sum(as.numeric(suicides_no))/ sum(as.numeric(population))* 100000 )

ggplot(media_gdp_pais, aes(x= meanGDP, y= suicidios_por_100k, col= factor(continent))) + 
geom_point()+
theme_fivethirtyeight()+
scale_fill_viridis(discrete=TRUE) +
theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
guides(fill=F,xlab=F)+
ggtitle("Relación entre GDP(per capita) y Suicidios por 100k ")+
labs(x = "GDP(per capita)", 
     y = "Suicidios por 100k",
     col = "Continent") +
scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000))




########## Valores outliers ############

#Se observan valores extremos que podrían influenciar negativamente en el modelo,  
#por lo tanto se usará la distancia Cook,  para excluir aquellos países con un valor superior a 4/n.
#siendo n el numero de observaciones

modelo1 <- lm(suicidios_por_100k ~ meanGDP, data = media_gdp_pais)

mod_sin_outliers <- modelo1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% 
  inner_join(media_gdp_pais, by = c("suicidios_por_100k", "meanGDP")) %>%
  select(country, continent, meanGDP , suicidios_por_100k)


#Quedaron 88 paises de los 93
#Guardando nuestro modelo preliminar(sin outliers) en un archivo separado .csv
fwrite(mod_sin_outliers,"data/mod_sin_outliers.csv")





