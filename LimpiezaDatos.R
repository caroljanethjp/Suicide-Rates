#
# Este script limpiará el dataset, para que pueda ser usado después.
# Lo guardaremos en un archivo .csv
#



#librerías
library(tidyverse) # general
library(countrycode) # continentes
library(data.table)



########### Lectura ##############
  
dt <- read_csv("data/master.csv") 

dim(dt)
summary(dt)
str(dt)



########## Limpieza de datos ##############

#El 70% de HDI son valores faltantes, por eso se elimina del análisis
# sum(is.na(dt$`HDI for year`))
dt <- dt %>% 
  select(-c(`HDI for year`, `suicides/100k pop`)) 


#Renombrando las variables 
dt <- dt %>% 
      rename(gdp_for_year = `gdp_for_year ($)`, 
            gdp_per_capita = `gdp_per_capita ($)`, 
            country_year = `country-year`) %>%
      as.data.frame()


  
#Filtrando información incompleta
  
#Por cada grupo country_year debe haber 12 observaciones (edad=6 * sexo=2)  
# dt %>% 
#   group_by(country_year) %>%
#   count() %>%
#   filter(n != 12) 
  
#El año 2016 tiene observaciones sólo de  16 paises(de 100 en total) pero  incompleta , por eso sale del análisis  
dt <- dt %>%
  filter(year != 2016) 
#Ya no usaremos variable country_year
dt <- dt %>%  
      select(- country_year)


#Fijamos un mínimo años: Nos quedamos solo con los paises que tienen más de 3 años de observaciones

minimo <- dt %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)
dt <- dt %>%
  filter(!(country %in% head(minimo$country, 7)))


#Organizando el DF
dt$age <- gsub(" years", "", dt$age)
dt$sex <- ifelse(dt$sex == "male", "Male", "Female")


#Obteniendo la variable continente
dt$continent <- countrycode(sourcevar = dt$country,
                              origin = "country.name",
                              destination = "continent")


dt <- as_tibble(dt)


# Guardando el dataset limpio en el actual directorio de trabajo
fwrite(dt,"data/master_clean.csv")

