#
# Obtención del modelo de regresión  lineal, sin outliers
# Se muestra el grafico de la  ecuación del modelo
#



########## Modelo de Regresión ############

mod_sin_outliers <- read_csv("data/mod_sin_outliers.csv")

modelo_fin <- lm(suicidios_por_100k ~ meanGDP, data = mod_sin_outliers)

summary(modelo_fin)




########## Visualización   ############
ggplot(mod_sin_outliers, aes(x = meanGDP, y = suicidios_por_100k, col = factor(continent))) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_fill_viridis(discrete=TRUE) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9),axis.title.x = element_blank())+
  guides(fill=F,xlab=F)+
  ggtitle("Modelo GDP(per capita) vs. Suicidios ")+
  labs( y = "Suicidios por 100k") +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000))+
  theme(legend.position = "none")
  

  
########## Conlusiones  ############

#1) Siendo p = 0.0288 < 0.05, nos muestra que sí existe una relación positiva 
#   entre la riqueza y los suicidios.
#2) Sin embargo R cuadrado(0.054) tendiendo más a cero, 
#   confirma que  aunque están relacionadas , ésta relación es débil.
#3) La ecuación del modelo viene dada: 
#   Suicides(por 100k)= 8.77 + (1.115e-04)*GDP(per capita)
# Lo que nos dice que para que se produzca un suicidio adicional(por 100 k de la población)
# tiene que darse un incremento del GDP(per capita) del $8,968 



