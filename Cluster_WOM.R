library(data.table) #Librería para llamar DataFrame
library(dplyr)
library(modeest)

setwd("C:/Users/Rodrigo/Desktop/WOM") #Se reubica la carga de archiivos

#### CARGA Y PREPARACIÓN DE LA BASE DE DATOS ####
df = fread("test_social_network.txt") #Se carga la base de datos en formato DF
df = df[, c(2,3,4,5,6,7,8,9)] #Se seleccionan las variables de utilidad para el modelo
colnames(df)[6]<-"ROAMING"
colnames(df)[1]<-"FECHA"

View(df)

#### TIPOS DE VARIABLES ####
class(df$ID_DWH_DIA)
class(df$V4)
class(df$V5)
class(df$OPERADOR)
class(df$ROAMING)
class(df$Q_LLAMADAS)
class(df$F_DURACION)


#### EXTRAE NÚMEROS Y TRANSFORMA LA VARIABLE OPERADOR EN NUMÉRICA ####
df$OPERADOR <- substr(df$OPERADOR, start=10, stop=11)
class(df$OPERADOR)
df$OPERADOR <- as.numeric(df$OPERADOR)
class(df$OPERADOR)
head(df$OPERADOR)

#### EXTRAE NÚMEROS Y TRANSFORMA LA VARIABLE ROAMING EN NUMÉRICA ####
df$ROAMING <- substr(df$ROAMING, start=6, stop=6)
class(df$ROAMING)
df$ROAMING <- as.numeric(df$ROAMING)
class(df$ROAMING)
head(df$ROAMING)

#### EXTRAE NÚMEROS Y TRANSFORMA LA VARIABLE ID_DWH_DIA EN NUMÉRICA ####
df$FECHA <- substr(df$FECHA, start=7, stop=8)
class(df$FECHA)
df$FECHA <- as.numeric(df$FECHA)
class(df$FECHA)
head(df$FECHA)

#### CARACTERÍSTICAS DE LA BASE DE DATOS ####
dim(df) #Dimensión del DF de 1.999.936
head(df) #6 primeros
#View(df)
object.size(df)
#str(df)


#### ANÁLISIS PRELIMINAR DE ESTUDIO DE VARIABLES ####

table(df$SENTIDO) #   E 1067500 - S 932436 
table(df$ROAMING) #   ROAM 1 1591971 - ROAM 2 301906 - ROAM 3  105918 - ROAM 4  141 
table(df$Q_LLAMADAS)
table(df$OPERADOR)



#### SE TRANSFORMA LA VARIABLE DE LLAMADAS ENTRANTES, SALIENTES Y LAS  ####
####  DE CADA ID POR SOLO DOS VARIABLES DE LLAMADA EMITIDA Y RECIBIDA ####
EMITIDAS_V4 <- subset(df, SENTIDO != "E") #Excluimos las llamadas entrantes de Nro abonado
#head(EMITIDAS_V4)
dim(EMITIDAS_V4) #Dimensión

B1 = EMITIDAS_V4[, c(1,2,3,4,5,6,7,8)]
head(B1)
class(B1$V4)

EMITIDAS_V5 <- subset(df, SENTIDO != "S") #Excluimos las llamadas entrantes de DNA-DNB
#head(EMITIDAS_V5)
dim(EMITIDAS_V5)

B2 = EMITIDAS_V5[, c(1,2,4,3,5,6,7,8)] #Intercambiando las columnas para que las salientes queden primero
head(B2)
class(B2$V5)

colnames(B1)[3]<-"SALIENTE" #Se le asignan nombres iguales a las columnas de ambas ta las para poder cruzarlas
colnames(B1)[4]<-"ENTRANTE"

colnames(B2)[3]<-"SALIENTE"
colnames(B2)[4]<-"ENTRANTE"


CLUS = rbind(B1, B2) #Se agregan las llamadas entrantes y salientes de las variables Abonado y DNA_DNB
head(CLUS)

CLUS = CLUS[, c(1,3,4,5,6,7,8)] #Se elimina la variable Sentido ya que ha sido reemplazada su utilidad
dim(CLUS)
head(CLUS)


#### TABLAS DE FRECUENCIAS DE LLAMADAS ####

## SALIENTES ##
FREQ_SALIENTES <- table(CLUS$SALIENTE) #Se crea una tabla de frecuencias para calcular las repeticiones en llamadas salientes
#dim(FREQ_SALIENTES)
#head(FREQ_SALIENTES)
#View(FREQ_SALIENTES)
FREQ_SALIENTES <- as.data.frame(FREQ_SALIENTES)
colnames(FREQ_SALIENTES)[1]<-"ID_UNICO"
colnames(FREQ_SALIENTES)[2]<-"FREQ"
class(FREQ_SALIENTES$ID_UNICO)
class(FREQ_SALIENTES$FREQ)
#FREQ_SALIENTES <- data.frame(t(FREQ_SALIENTES[-1])) #Trasposición del vector de datos
#FREQ_SALIENTES = FREQ_SALIENTES[, c(2,3)]
dim(FREQ_SALIENTES)
head(FREQ_SALIENTES)

#### Ya que la función TABLE genera columnas de tipo FACTOR, ####
#### primero se transforman en tipo CARACTER y luego en NUMÉRICO ####
class(FREQ_SALIENTES$ID_UNICO)
FREQ_SALIENTES$ID_UNICO <- as.character(FREQ_SALIENTES$ID_UNICO) 
class(FREQ_SALIENTES$ID_UNICO)
FREQ_SALIENTES$ID_UNICO <- as.numeric(FREQ_SALIENTES$ID_UNICO)
head(FREQ_SALIENTES)
#View(FREQ_SALIENTES)

class(FREQ_SALIENTES$FREQ)
FREQ_SALIENTES$FREQ <- as.numeric(FREQ_SALIENTES$FREQ)
class(FREQ_SALIENTES$freq)
head(FREQ_SALIENTES)


## ENTRANTES ##
CLUS$ENTRANTE
head(CLUS)
FREQ_ENTRANTES <- table(CLUS$ENTRANTE) #Se crea una tabla de frecuencias para calcular las repeticiones en llamadas salientes

FREQ_ENTRANTES <- as.data.frame(FREQ_ENTRANTES)
colnames(FREQ_ENTRANTES)[1]<-"ID_UNICO"
colnames(FREQ_ENTRANTES)[2]<-"FREQ"
class(FREQ_ENTRANTES$ID_UNICO)
class(FREQ_ENTRANTES$FREQ)

dim(FREQ_ENTRANTES)
head(FREQ_ENTRANTES)

class(FREQ_ENTRANTES$ID_UNICO)
FREQ_ENTRANTES$ID_UNICO <- as.character(FREQ_ENTRANTES$ID_UNICO) 
class(FREQ_ENTRANTES$ID_UNICO)
FREQ_ENTRANTES$ID_UNICO <- as.numeric(FREQ_ENTRANTES$ID_UNICO)
head(FREQ_ENTRANTES)
#View(FREQ_ENTRANTES)

class(FREQ_ENTRANTES$FREQ)
FREQ_ENTRANTES$FREQ <- as.numeric(FREQ_ENTRANTES$FREQ)
class(FREQ_ENTRANTES$freq)
head(FREQ_ENTRANTES)


#### QUITAR DUPLICADOS Y DEJAR ID DE LLAMADAS COMO ÚNICOS ####

ID_SALIENTES <- CLUS[!duplicated(SALIENTE),] #Se exporta a una nueva tabla, la columna de llamadas salientes sin duplicados
dim(ID_SALIENTES)
ID_SALIENTES = ID_SALIENTES[, c(2)]
colnames(ID_SALIENTES)[1]<-"ID_UNICO"
#View(ID_SALIENTES)
class(ID_SALIENTES$ID_UNICO)

ID_ENTRANTES <- CLUS[!duplicated(ENTRANTE),] #Se exporta a una nueva tabla, la columna de llamadas entrantes sin duplicados
dim(ID_ENTRANTES)
ID_ENTRANTES = ID_ENTRANTES[, c(3)]
colnames(ID_ENTRANTES)[1]<-"ID_UNICO"
#View(ID_ENTRANTES)
class(ID_ENTRANTES$ID_UNICO)


ID_UNICO = rbind(ID_SALIENTES, ID_ENTRANTES) #Nueva tabla con columna de valores de ID único
#View(ID_UNICO)
dim(ID_UNICO) # La nueva dimensión de clientes únicos (sin repetición) es de 1.558.141
ID_UNICO <- ID_UNICO[!duplicated(ID_UNICO),] #Se vuelve a quitar duplicados por los que puedan estar en ambas tablas
dim(ID_UNICO)

ID_UNICO$ID_UNICO <- as.numeric(ID_UNICO$ID_UNICO)

head(FREQ_ENTRANTES)
head(FREQ_SALIENTES)

NEW_DF <- merge(ID_UNICO,FREQ_SALIENTES, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con LLAMADAS SALIENTES
NEW_DF_2 <- merge(NEW_DF,FREQ_ENTRANTES, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con LLAMADAS ENTRANTES

dim(NEW_DF_2)

colnames(NEW_DF_2)[2]<-"SALIENTES"
colnames(NEW_DF_2)[3]<-"ENTRANTES"

head(NEW_DF_2)


#### CORROBORACIÓN DE FORMATO DE LAS VARIABLES PARA CRUCES DE DATOS ####

class(CLUS$FECHA)
class(CLUS$SALIENTE)
class(CLUS$ENTRANTE)
class(CLUS$OPERADOR)
class(CLUS$ROAMING)
class(CLUS$Q_LLAMADAS)
class(CLUS$F_DURACION)

CLUS$SALIENTE <- as.numeric(CLUS$SALIENTE)
CLUS$ENTRANTE <- as.numeric(CLUS$ENTRANTE)
CLUS$Q_LLAMADAS <- as.numeric(CLUS$Q_LLAMADAS)
CLUS$F_DURACION <- as.numeric(CLUS$F_DURACION)

class(CLUS$SALIENTE)
class(CLUS$ENTRANTE)
class(CLUS$Q_LLAMADAS)
class(CLUS$F_DURACION)

head(CLUS)
head(NEW_DF_2)


#### Suma y cruce de las variables Q_LLAMADAS Y F_DURACIÓN ####

#### SALIENTES ####

SALIENTE_SUM = CLUS[, c(2,6,7)]
head(SALIENTE_SUM)

SALIENTE_SUM_Q <- aggregate(SALIENTE_SUM$Q_LLAMADAS, by=list(SALIENTE=SALIENTE_SUM$SALIENTE), FUN=sum)
colnames(SALIENTE_SUM_Q)[1]<-"ID_UNICO"
head(SALIENTE_SUM_Q)
SALIENTE_SUM_F <- aggregate(SALIENTE_SUM$F_DURACION, by=list(SALIENTE=SALIENTE_SUM$SALIENTE), FUN=sum)
colnames(SALIENTE_SUM_F)[1]<-"ID_UNICO"
head(SALIENTE_SUM_F)

NEW_DF_3 <- merge(NEW_DF_2,SALIENTE_SUM_Q, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES
NEW_DF_4 <- merge(NEW_DF_3,SALIENTE_SUM_F, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES

colnames(NEW_DF_4)[4]<-"Q_LLAMADAS_SAL"
colnames(NEW_DF_4)[5]<-"F_DURACION_SAL"
head(NEW_DF_4)

#### ENTRANTES ####

ENTRANTE_SUM = CLUS[, c(3,6,7)]
head(ENTRANTE_SUM)

ENTRANTE_SUM_Q <- aggregate(ENTRANTE_SUM$Q_LLAMADAS, by=list(ENTRANTE=ENTRANTE_SUM$ENTRANTE), FUN=sum)
colnames(ENTRANTE_SUM_Q)[1]<-"ID_UNICO"
head(ENTRANTE_SUM_Q)
ENTRANTE_SUM_F <- aggregate(ENTRANTE_SUM$F_DURACION, by=list(ENTRANTE=ENTRANTE_SUM$ENTRANTE), FUN=sum)
colnames(ENTRANTE_SUM_F)[1]<-"ID_UNICO"
head(ENTRANTE_SUM_F)

NEW_DF_5 <- merge(NEW_DF_4,ENTRANTE_SUM_Q, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES
NEW_DF_6 <- merge(NEW_DF_5,ENTRANTE_SUM_F, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES

colnames(NEW_DF_6)[6]<-"Q_LLAMADAS_ENT"
colnames(NEW_DF_6)[7]<-"F_DURACION_ENT"
head(NEW_DF_6)


#### PARA CRUCE DE LLAMADAS SALIENTES ####

CLUS_SAL = CLUS[, c(2,4,5)]
colnames(CLUS_SAL)[1]<-"ID_UNICO"
head(CLUS_SAL)
NEW_DF_7 <- merge(NEW_DF_6,CLUS_SAL, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES
head(NEW_DF_7)

colnames(NEW_DF_7)[8]<-"OPERADOR_SAL"
colnames(NEW_DF_7)[9]<-"ROAMING_SAL"


class(NEW_DF_7$Q_LLAMADAS_SAL)
class(NEW_DF_7$F_DURACION_SAL)
class(NEW_DF_7$OPERADOR_SAL)
class(NEW_DF_7$ROAMING_SAL)

head(NEW_DF_7)
dim(NEW_DF_7)
NEW_DF_8 <- NEW_DF_7[!duplicated(ID_UNICO),]
dim(NEW_DF_7)
head(NEW_DF_7)

#### PARA CRUCE DE LLAMADAS ENTRANTES ####

CLUS_ENT = CLUS[, c(2,4,5)]
colnames(CLUS_ENT)[1]<-"ID_UNICO"
head(CLUS_ENT)
NEW_DF_9 <- merge(NEW_DF_8,CLUS_ENT, by = "ID_UNICO", all.x = T) #Cruce ID ÚNICOS con Variables de LLAMADAS ENTRANTES
head(NEW_DF_9)

colnames(NEW_DF_9)[10]<-"OPERADOR_ENT"
colnames(NEW_DF_9)[11]<-"ROAMING_ENT"

class(NEW_DF_9$Q_LLAMADAS_ENT)
class(NEW_DF_9$F_DURACION_ENT)
class(NEW_DF_9$OPERADOR_ENT)
class(NEW_DF_9$ROAMING_ENT)

head(NEW_DF_9)
dim(NEW_DF_9)
NEW_DF_10 <- NEW_DF_9[!duplicated(ID_UNICO),]
dim(NEW_DF_10)
head(NEW_DF_10)

#### TRANSFORMAR NANs EN CEROS ####

NEW_DF_10[is.na(NEW_DF_10)] <- 0
dim(NEW_DF_10)
head(NEW_DF_10)
#View(NEW_DF_10)


#### CLUSTERIZACIÓN ####

kmeans <- kmeans(NEW_DF_10, 5, iter.max = 10, nstart = 10)

NEW_DF_10$cluster <- kmeans$cluster
head(NEW_DF_10)

write.table(NEW_DF_10, "Cluster_WOM.csv", row.names = F, sep=";")

#View(NEW_DF_10)
dim(NEW_DF_10)
head(NEW_DF_10)
object.size(NEW_DF_10)
str(NEW_DF_10)
table(NEW_DF_10$cluster)


#### ESTADÍSTICOS DE IDENTIFICACIÓN DE GRUPOS ####

Grupo_1 <- subset(NEW_DF_10, cluster != 1)
Grupo_2 <- subset(NEW_DF_10, cluster != 2)
Grupo_3 <- subset(NEW_DF_10, cluster != 3)
Grupo_4 <- subset(NEW_DF_10, cluster != 4)
Grupo_5 <- subset(NEW_DF_10, cluster != 5)

#### MAlgunas medidas de tendencia central  ####

summary(Grupo_1)
min(Grupo_1) 
max(Grupo_1) 
range(Grupo_1) 
mean(Grupo_1$Q_LLAMADAS_SAL)  
median(Grupo_1$Q_LLAMADAS_SAL) 
length(Grupo_1$Q_LLAMADAS_SAL) 
sd(Grupo_1$Q_LLAMADAS_SAL)  
var(Grupo_1$Q_LLAMADAS_SAL)
cov(Grupo_1$Q_LLAMADAS_SAL,Q_LLAMADAS_ENT) 
cor(Q_LLAMADAS_ENT) 
quantile(Grupo_1$Q_LLAMADAS_ENT, 0.25)
quantile(Grupo_1$Q_LLAMADAS_ENT, 0.75)  
IQR(Grupo_1$Q_LLAMADAS_ENT)
sort(Grupo_1$Q_LLAMADAS_ENT)
table(Grupo_1$Q_LLAMADAS_ENT)





