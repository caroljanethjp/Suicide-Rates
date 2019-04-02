#
# Este script se construyó a partir del dataset obtenido en la Limpieza de datos
# Se explorarán lo datos y se hará gráficas para entender mejor el conjunto de datos
#



#librerías

library(tidyverse)

# Para visualizacion
library(grid)
library(gridExtra)
library(ggridges)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(viridis)

########## Lectura ############

dt <- read_csv("data/master_clean.csv")


########## Explorando los datos ############

#Formato factor
dt$age <- factor(dt$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))

# Nuestro dt
str(dt)
summary(dt)
dim(dt)
       


########## Manipulando los Datos ############

### Evolución mundial de suicidios , desde  1985 hasta 2015 , genera p1
suicidios_mundial = dt %>%
            group_by(year)%>%
            summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
  
  

### Evolución mundial de suicidios por continente , genera p2
suicidios_continente = dt %>%
                       group_by(year, continent)%>%
                       summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)


### Suicidios globales por edad, genera p3
suicidios_por_edad = dt %>%
  group_by(age)%>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
#Cambiando a factor la edad para poder pintarla por colores
suicidios_por_edad$age <- factor(suicidios_por_edad$age, 
                                 ordered = T, levels = c("5-14",
                                                         "15-24", 
                                                         "25-34", 
                                                         "35-54", 
                                                         "55-74", 
                                                         "75+"))






########## Visualización ############


p1= ggplot(suicidios_mundial, aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "darkgoldenrod1 ", size = 1) + 
  geom_point(col = "darkgoldenrod1", size = 2) + 
  scale_fill_viridis(discrete=TRUE) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
  guides(fill=F,xlab=F)+
  ggtitle("Suicidios globales por Años")+
  labs(y = "Suicidios por 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))



p2= ggplot(suicidios_continente, aes(x = year, y = suicides_per_100k, col= factor(continent))) + 
  facet_grid(continent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  scale_fill_viridis(discrete=TRUE) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
  guides(fill=F,xlab=F)+
  ggtitle("Evolución por Continentes")+
  labs(y = "Suicidios por 100k") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) 
  


p3= ggplot(suicidios_por_edad, aes(x= age, y= suicide_per_100k, fill= age )) +
  geom_col()+
  scale_fill_viridis(discrete=TRUE) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
  guides(fill=F,xlab=F)+
  ggtitle("Total de Suicidios por edades")+
  labs(y = "Suicidios por 100k") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,30,1), minor_breaks = F )


grid.arrange(p1,arrangeGrob(p2,p3, ncol=2),  ncol=1 )



#pero como va ido cambiando la tendencia de acuerdo a la edad?
evolucion_por_edad = dt %>%
                    group_by(year, age)%>%
                    summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
                    ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
                    facet_grid(age ~ ., scales = "free_y") + 
                    geom_line() + 
                    geom_point() + 
                    scale_fill_viridis(discrete=TRUE) +
                    theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
                    guides(fill=F,xlab=F)+
                    ggtitle("Evolución por edades")+
                    labs(y = "Suicidios por 100k") +
                    theme(legend.position = "none", title = element_text(size = 10)) + 
                    scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) 

grid.arrange(p3, evolucion_por_edad, ncol = 2)





