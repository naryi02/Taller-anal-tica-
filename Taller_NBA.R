#PROBABILIDAD Y ESTADÍSTICA 
#ESTADÍSTICA DESCRIPTIVA
#install.packages("ggplot2") #descargar librería
#install.packages("psych")
#install.packages("tidyverse")
library(tidyverse)
library(psych)
library(ggplot2) #cargar librería
datos=read.csv("NBA.csv") #Cargar los datos y guardarlos con el nombre "datos"
datos=na.omit(datos) #Sobreescribir el archivo datos por uno que elimina filas con NA's
str(datos) #Ver la estructura de cada variable
datos$Pos=as.factor(datos$Pos) #Sobrescribir las entradas como factores, es decir formaremos categorias 
datos$Tm=as.factor(datos$Tm) 

table(datos$Tm)
write.table(table(datos$Tm), file = "tablaequipos.txt", sep = ",", quote = FALSE,row.names=F)

#Grafico número de jugadores po posición 

ggplot(datos, aes(x=reorder(Pos, Pos, function(x)-length(x))))+
  geom_bar(fill=heat.colors(8))+
  geom_text(stat="count", aes(label=..count..), vjust=-1)+
  ggtitle("Número de jugadores por posición")+theme(plot.title=element_text(hjust=0.5))+
  labs(x="Posición", y="Número de jugadores")

describe(datos)
describeBy(datos~Pos)


write.table(lapply(datos, mean), file = "tablamedias.txt", sep = ",", quote = FALSE,row.names=F)


# 1 HISTOGRAMA TIROS DE CAMPO EFECTIVOS
ggplot(datos, aes(x=eFG.))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma para tiros de campo efectivos para jugadores NBA")+
  labs(x="Tiros de campo efectivos (eFG)", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=eFG., y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=eFG., y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma para tiros de campo efectivos para jugadores NBA")+
  labs(x="Tiros de campo efectivos (eFG)", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 2 HISTOGRAMA PUNTOS ANOTADOS
ggplot(datos, aes(x=PTS))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma para total de puntos anotados por jugador")+
  labs(x="Total puntos anotados por jugador", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=PTS , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=PTS , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma para total de puntos anotados por jugador")+
  labs(x="Total puntos anotados por jugador", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 3 HISTOGRAMA MINUTOS JUGADOS

ggplot(datos, aes(x=MP))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma para total minutos jugados para jugadores NBA")+
  labs(x="Total minutos jugados", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=MP , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=MP , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma para total minutos jugados para jugadores NBA")+
  labs(x="Total minutos jugados", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 4 HISTOGRAMA EDAD

ggplot(datos, aes(x=Age))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma de la edad en años para jugadores NBA")+
  labs(x="Edad en años", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=Age , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=Age , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma de la edad en años para jugadores NBA")+
  labs(x="Edad en años", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 5 HISTOGRAMA TOTAL REBOTES 
ggplot(datos, aes(x=TRB))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma total rebotes para jugadores NBA")+
  labs(x="Total rebotes", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=TRB , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=TRB , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma total rebotes para jugadores NBA")+
  labs(x="Total rebotes", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 6 HISTOGRAMA TOTAL NMERO DE ASISTENCIAS
ggplot(datos, aes(x=AST))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma número total asistencias para jugadores NBA")+
  labs(x="Total asistencias", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=AST , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=AST , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma número total asistencias para jugadores NBA")+
  labs(x="Total asistencias", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

# 7 HISTOGRAMA TOTAL NÚMERO DE BOLAS ROBADAS
ggplot(datos, aes(x=STL))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  ggtitle("Histograma número total balones robados por jugador")+
  labs(x="Total balones robados por jugador", y="Frecuencia")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

ggplot(datos, aes(x=STL , y=..density..))+
  geom_histogram(bins=ceiling(sqrt(length(datos$eFG.))), fill="deeppink2", col="white")+
  geom_density(aes(x=STL , y=..density..), colour="black", size=0.8)+
  ggtitle("Histograma número total balones robados por jugador")+
  labs(x="Total balones robados por jugador", y="Densidad")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))

#BOX VAIRBLES POR POSICIÓN 

#Q Q PLOT  eFG

qqnorm(datos$eFG.)
qqline(datos$eFG., col="steelblue", lwd=2)
shapiro.test(datos$eFG.)

cv=function(x){
  sd(x)/mean(x)*100}

cv(datos$STL)

ggplot(datos, aes(x=eFG., y=Pos))+
  geom_boxplot(fill=rainbow(8))+
  ggtitle("Diagrama de caja para tiros de campo efectivo según posición")+
  labs(y="Tiros de campo efectivos")+
  coord_flip()+theme(plot.title = element_text(hjust=0.5))

ggplot(datos, aes(x=STL, y=Pos))+
  geom_boxplot(fill=rainbow(8))+
  ggtitle("Diagrama de caja para robos de balon según posición")+
  labs(y="Robos de balon")+
  coord_flip()+theme(plot.title = element_text(hjust=0.5))

ggplot(datos, aes(x=TRB, y=Pos))+
  geom_boxplot(fill=rainbow(8))+
  ggtitle("Diagrama de caja para rebotes según posición")+
  labs(y="Rebotes")+
  coord_flip()+theme(plot.title = element_text(hjust=0.5))

ggplot(datos, aes(x=PTS, y=Pos))+
  geom_boxplot(fill=rainbow(8))+
  ggtitle("Diagrama de caja para puntos anotados según posición")+
  labs(y="Tiros de campo efectivos")+
  coord_flip()+theme(plot.title = element_text(hjust=0.5))

datoscuant<-datos%>%
select(PTS, TRB, AST, STL, eFG., MP, Age) #Selecciona las variables y crea otro data frame

Tabla_descriptiva=describe(datoscuant, IQR=T) #Tabla descriptiva para las variables cuantitativas

Tabla_descriptiva<-Tabla_descriptiva%>% #Crear columna con CV (coeficiente de variación) en tabla descrptiva
  mutate(CV=sd/mean*100) 

Tabla_descriptiva


corPlot(datoscuant, main="Diagrama de correlación")# Diagrama de correlación

ggplot(datoscuant, aes(x=PTS, y=AST))+
  geom_point(fill="blue")+
  ggtitle("Diagrama de correlación puntos y asistencias")+
  labs(x="Puntos anotados", y="Asistencias")
