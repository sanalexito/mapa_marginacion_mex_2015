library(rgdal)
library(ggplot2)
library(foreign)
library(ggspatial)
capa_estados <-readOGR("Dir/conjunto_de_datos", layer= "areas_geoestadisticas_estatales")

#Cargo la base de marginación y la limpio. 
marginacion <- read.csv("Dir/Base_Indice_de_marginacion_estatal_90-15.csv")
marginacion$AÑO<-as.numeric(as.character(marginacion$AÑO))  

marginacion$f<-ifelse(!marginacion$NOM_ENT%in%"Nacional",1,0)
mar1<-marginacion[marginacion$f%in%1,c("AÑO","CVE_ENT","NOM_ENT","IM")]
mar1$id<-mar1$CVE_ENT
mar1$IM<-as.numeric(as.character(mar1$IM))
mar1<-mar1[mar1$AÑO%in%2015,]
summary(mean(mar1$IM))

#Paso el objeto SPDF a data.frame con `fortify()`, una función de ggplot2 con métodos para convertir objetos diversos a data.frame.

capa_estados_df <- fortify(capa_estados, region="CVE_ENT") #region="CVE_ENT", el agrupamiento de los polígonos.     
capa_estados_df$id<-as.numeric(as.character(capa_estados_df$id))
capa_estados_df_mediaIM <- merge(capa_estados_df, mar1[,c("id","NOM_ENT","mediaIM")], by="id") #Uno las medias con los polígonos por la variable clave: id.


ggplot(mar1, aes(map_id = id)) + 
  geom_map(aes(fill = IM), map = capa_estados_df) + 
  expand_limits(x = capa_estados_df$long, y = capa_estados_df$lat) +
  scale_fill_continuous(low="green", high="red") +                           #Cambio la escala contínua de colores.
  labs(title="Media del Índice de Marginación por Estados 2015",   #Las anotaciones con la sintaxis usual de ggplot2::
       subtitle="Ponderado por población", 
       caption="Elaboración propia. \n Datos geográficos: INEGI 2016. \n Datos de estadísticos: CONAPO 2015") +
  geom_errorbarh(aes(x=1.5e6, xmin=1.5e6-1e5, xmax=1.5e6+1e5, y=1e6), height=5e4) + #Uso una barra de error horizontal para la escala
  annotate("text", x= 1.5e6, y=1.0e6+8e4, label="200km", size=2) +           #Nota de la escala +- 100000 metros=200km. 
  theme(legend.position = c(0.8, 0.7)) +                                     #Ubico la leyenda dentro del gráfico en una zona vacia.
  annotation_north_arrow(location='tr')+                                     #norte
  theme_void()                                                            #Elimina escalas, marcas de coordenadas, etc.

