
### EXPERIMENTOS BIOQUIMICOS 

library(ggplot2)

getwd()
setwd("C:/Users/Admin/Desktop/tesina/Resultados/2018/noviembre/bioquimicos/R/")

Tabla_2<- read.table("Tabla_2.txt", header=TRUE, sep = "\t", fill=T, fileEncoding = "latin1", dec= ".")

View(Tabla_2)

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# The errorbars overlapped, so use position_dodge to move them horizontally


## FRAP 


pd <- position_dodge(6) # move them .05 to the left and right
p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$FRAP_.umol.mg.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$FRAP_.umol.mg.-Tabla_2$DS_frap, ymax=Tabla_2$FRAP_.umol.mg.+Tabla_2$DS_frap), width=.1, 
    position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("FRAP umol/mg")+
  labs(title = "Potencial reductor f?rrico")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)



## AZUCARES SOLUBLES 

pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$AZUCARES_.mg.g.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$AZUCARES_.mg.g.-Tabla_2$DS_azucar, ymax=Tabla_2$AZUCARES_.mg.g.+Tabla_2$DS_azucar), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Azucares totales mg/g")+
  labs(title = "Azucares totales")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)


pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$AZUCARES_.mg.g.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$AZUCARES_.mg.g.-Tabla_2$DS_azucar, ymax=Tabla_2$AZUCARES_.mg.g.+Tabla_2$DS_azucar), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Az?cares totales mg/g")+
  labs(title = "Az?cares totales")

p + facet_wrap(~Tabla_2$condicion, nrow = 1)



## ALMIDON


pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$ALMIDON_.mg.g.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$ALMIDON_.mg.g.-Tabla_2$DS_almidon, ymax=Tabla_2$ALMIDON_.mg.g.+Tabla_2$DS_almidon), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Almid?n mg/g")+
  labs(title = "Almid?n")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)


## PROTEINAS 


pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$PROTE_.mg.g.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$PROTE_.mg.g.-Tabla_2$DS_prote, ymax=Tabla_2$PROTE_.mg.g.+Tabla_2$DS_prote), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Prote?nas mg/g")+
  labs(title = "Prote?nas")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)

## CLOROFILAS 


pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$Chl_.mg.mg.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$Chl_.mg.mg.-Tabla_2$Ds_Chl, ymax=Tabla_2$Chl_.mg.mg.+Tabla_2$Ds_Chl), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Clorofilas totales mg/mg")+
  labs(title = "Clorofilas totales")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)


## CAROTENOIDES

pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_2$Horas, y = Tabla_2$Carotenoides.mg.mg.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$Carotenoides.mg.mg.-Tabla_2$DS_carotenoides, ymax=Tabla_2$Carotenoides.mg.mg.+Tabla_2$DS_carotenoides), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Carotenoides y xant?filas mg/mg")+
  labs(title = "Carotenoides y xant?filas")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)



## AAlibres

Tabla_aa<- read.table("Tabla_aa.txt", header=TRUE, sep = "\t", fill=T, fileEncoding = "latin1", dec= ".")
View(Tabla_aa)

pd <- position_dodge(6) 

p <- ggplot(data = Tabla_2, 
            aes(x = Tabla_aa$Horas, y = Tabla_2$AA.libres_.mg.g.,
                group=Genotipos,
                shape=Genotipos,
                color=Genotipos)) +
  geom_line(position=pd, size= 1)+
  geom_errorbar(aes(ymin=Tabla_2$AA.libres_.mg.g.-Tabla_2$DS_aalib, ymax=Tabla_2$AA.libres_.mg.g.+Tabla_2$DS_aalib), width=.1, 
                position=pd)+
  geom_point(position=pd, size= 1.5)+
  scale_x_continuous("D?as", breaks = c(0, 24, 72, 168),labels = c("0", "1", "3", "7")) +
  scale_y_continuous("Amino?cidos libres mg/g")+
  labs(title = "Amino?cidos libres")


p + facet_wrap(~Tabla_2$condicion, nrow = 1)
