

## EXPERIMENTOS DESARROLLO  


getwd()
setwd("C:/Users/Admin/Desktop/tesina/Resultados/2018/Junio/Experimento 1a completo/")

Datos_E1a <- read.table("E1AAVER2.txt", header=TRUE, sep = "\t", fill=T, fileEncoding = "latin1", dec= ",")
clases_E1a <- sapply(Datos_E1a, class)
clases_E1a

# Cambiar clase de columnas 

Datos_E1a[,2:3] <- lapply(Datos_E1a[,2:3], as.factor)
clases_E1a <- sapply(Datos_E1a, class)
clases_E1a

#FUNCION TAPPLY

tapply(Datos_E1a$Ps2, Datos_E1a$Tratamiento, mean)
aggregate(Datos_E1a$Ps2, Datos_E1a$Tratamiento, mean)

#Ps2
Ps2_mean <-with (Datos_E1a, aggregate(Datos_E1a$Ps2, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

#FvFM
Fvfm_mean <-with (Datos_E1a, aggregate(Datos_E1a$Fvfm, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))     

#Area colonia
Area_Colonia_mean <-with (Datos_E1a, aggregate(Datos_E1a$Area_colonia, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

#area total
Area_Total_mean <-with (Datos_E1a, aggregate(Datos_E1a$Area_riz, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

#Diferencia entre areas

Diferencia_Area_mean <-with (Datos_E1a, aggregate(Datos_E1a$Diferencia, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

## Tengo que calcular la media de los ds que ya calcul? para cada una de las repeticiones 
# DS de FvFM
DS_Fvfm_mean <-with (Datos_E1a, aggregate(Datos_E1a$DS_Fvfm, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))     

#DS Ps2
DS_Ps2_mean <-with (Datos_E1a, aggregate(Datos_E1a$DS_Ps2, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

# DS de Area colonia 

DS_Area_Colonia_mean <-with (Datos_E1a, aggregate(Datos_E1a$DS_AC, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

#area total

DS_Area_Total_mean <-with (Datos_E1a, aggregate(Datos_E1a$DS_Area_riz, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))

#Diferencia entre areas

DS_Diferencia_Area_mean <-with (Datos_E1a, aggregate(Datos_E1a$DS_dif, list("Tratamiento"=Datos_E1a$Tratamiento, "Tiempo"=Datos_E1a$Tiempo), FUN = mean))




#CREAR UN DATA FRAME LOS CON TODOS LAS aggregate DE MEDIAS 

Dataframe_de_medias <- data.frame(PS2= Ps2_mean, FvFm=Fvfm_mean, AColonia= Area_Colonia_mean, ATotal= Area_Total_mean, DAreas= Diferencia_Area_mean, DSfvfm= DS_Fvfm_mean, DSAcolonia= DS_Area_Colonia_mean,DSAreaT= DS_Area_Total_mean,DSDif= DS_Diferencia_Area_mean, DS_Ps2_mean= DS_Ps2_mean) 

View(Dataframe_de_medias)



#GRAFICAR 


install.packages("ggplot2")
library(ggplot2)


#Desde el data frame con repeticiones

Grafico1 <- ggplot(Datos_E1a, aes(x = Tiempo, y = Ps2, group= Tratamiento, color = Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  ylab("Ps2")+ xlab("Dias")


View(Datos_E1a)
Grafico1

#Dsde el data frame de medidas de resumen 

View(Dataframe_de_medias)

#PS2

Grafico2 <- ggplot(Dataframe_de_medias, aes(x = Dataframe_de_medias$DAreas.Tiempo, y = PS2.x, group= PS2.Tratamiento, color = PS2.Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  labs(title="Estado del fotosistema 2", y="PS2", x = "Dias")+
  geom_errorbar(aes(ymin = PS2.x - DS_Ps2_mean.x, ymax= PS2.x + DS_Ps2_mean.x), 
                width=.1, position=position_dodge(0.1))+
  labs(colour= "Tratamientos")+
  scale_x_continuous(breaks = c(1, 3, 7, 14),labels = c("1", "3", "7", "14"))


Grafico2


#Grafico FVFM


Grafico3 <- ggplot(Dataframe_de_medias, aes(x = Dataframe_de_medias$DAreas.Tiempo, y = FvFm.x, group= FvFm.Tratamiento, color = FvFm.Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  labs(title="Eficiencia fotosintetica", y="FvFm", x = "Dias")+
  geom_errorbar(aes(ymin = FvFm.x - DSfvfm.x, ymax= FvFm.x + DSfvfm.x), 
                width=.1, position=position_dodge(0.1))+
  labs(colour= "Tratamientos")+
  scale_x_continuous(breaks = c(1, 3, 7, 14),labels = c("1", "3", "7", "14"))

Grafico3


#Grafico area 

Grafico4 <- ggplot(Dataframe_de_medias, aes(x = Dataframe_de_medias$DAreas.Tiempo, y = AColonia.x, group= AColonia.Tratamiento, color = AColonia.Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  labs(title="?rea colonias", y="cm2", x = "Dias")+
  geom_errorbar(aes(ymin = AColonia.x - DSAcolonia.x, ymax= AColonia.x + DSAcolonia.x), 
                width=.1, position=position_dodge(0.1))+
  labs(colour= "Tratamientos")+
  scale_x_continuous(breaks = c(1, 3, 7, 14),labels = c("1", "3", "7", "14"))

Grafico4



#Grafico area total 

Dataframe_de_medias$DAreas.Tiempo <- as.numeric(as.character(Dataframe_de_medias$DAreas.Tiempo))

Grafico5 <- ggplot(Dataframe_de_medias, aes(x =Dataframe_de_medias$DAreas.Tiempo, y = ATotal.x, group= ATotal.Tratamiento, color = ATotal.Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  labs(title="?rea total colonias", y="cm2", x = "Dias")+
  geom_errorbar(aes(ymin = ATotal.x - DSAreaT.x, ymax= ATotal.x + DSAreaT.x), 
                width=.1, position=position_dodge(0.1))+
  labs(colour= "Tratamientos")+
  scale_x_continuous(breaks = c(1, 3, 7, 14),labels = c("1", "3", "7", "14"))


Grafico5



#Grafico diferencia rizoides/colonia 

Grafico6 <- ggplot(Dataframe_de_medias, aes(x = Dataframe_de_medias$DAreas.Tiempo, y = DAreas.x, group= DAreas.Tratamiento, color = DAreas.Tratamiento))+
  geom_point()+
  stat_smooth(se = F)+
  labs(title="Diferencia rizoides/colonia", y="cm2", x = "Dias")+
  geom_errorbar(aes(ymin = DAreas.x - DSDif.x, ymax= DAreas.x + DSDif.x), 
                width=.1, position=position_dodge(0.1))+
  labs(colour= "Tratamientos")+
  scale_x_continuous(breaks = c(1, 3, 7, 14),labels = c("1", "3", "7", "14"))

