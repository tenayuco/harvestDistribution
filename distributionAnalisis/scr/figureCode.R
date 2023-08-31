### este va a ser el script para generar el output de las figuras finales

library(ggplot2)
library(dplyr)
library(tidyverse)
library(moveHMM)
library(knitr)
library(reshape2)
colorsGris <- c("black","#555555", "white")
groupColors3 <- c("#021128", "#fd9706", "#1b4a64" )
groupColors2 <- c("#021128", "#fd9706")
mycols3c <-c("#759580", "#1b4a64")

mycolsStates <-c("#1b4a64", "#fd9706" )

#these 2 colors contrast weel for color blind people





#DF_HARVEST_GAMMA <- read.csv("/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/data/datosParaFiguras.csv")
DF_HARVEST_GAMMA <- read.csv("../data/datosParaFiguras.csv")


#aqui invierto el 1 con el 2

DF_HARVEST_GAMMA$state <-  (-DF_HARVEST_GAMMA$state) + 3


DF_HARVEST_GAMMA <- DF_HARVEST_GAMMA %>%
  separate(ID, into = c("Finca", "IDREC"), sep = "_", remove = T)


DF_HARVEST_GAMMA_H <- DF_HARVEST_GAMMA%>%
  filter(Finca == "H")%>%
  group_by(IDREC) %>%
  mutate(ID_POR_FINCA = cur_group_id())

DF_HARVEST_GAMMA_I <- DF_HARVEST_GAMMA %>%
  filter(Finca == "I")%>%
  group_by(IDREC) %>%
  mutate(ID_POR_FINCA = cur_group_id())

DF_HARVEST_GAMMA <- rbind(DF_HARVEST_GAMMA_I, DF_HARVEST_GAMMA_H)  

rm(DF_HARVEST_GAMMA_I)
rm(DF_HARVEST_GAMMA_H)


#ahora estan umberados del 1 al 6 dentro de cada finca
#codesnames



FIG_MAP_GAMMA<- DF_HARVEST_GAMMA %>% 
  unite("Finca_ID", c(Finca, ID_POR_FINCA)) %>% 
  ggplot(aes(x= x, y = y)) +
  geom_path(aes(col= as.factor(state)), size= 1)+
  geom_point(size=1, aes(fill= "Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  scale_color_manual(values= mycolsStates)+
  facet_wrap(~Finca_ID, ncol = 3)+
  theme_bw()+
  theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  labs(x= "X", y= "Y", col= "State", fill= "")


#ggsave(FIG_MAP_GAMMA, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/mapHarvest_.png", height = 12, width = 10, device = "png")
ggsave(FIG_MAP_GAMMA, filename= "../output/finalFigures/mapHarvest_.png", height = 12, width = 10, device = "png")


#ggsave("../output/finalFigures/mapHarvest_.png", height = 8, width = 12, device = "png")

X= seq(1, 120, 1)


#este no es igual que en el otro codigo ,porque invert los estados    
DF_gamma_st1 <- data.frame("distribution"= c("gamma"), "state" = 1, "X"= X,  "PDF" = dgamma(X, shape= 3.62, rate = 0.845))
DF_gamma_st2 <- data.frame("distribution"= c("gamma"), "state" = 2, "X"= X,  "PDF" = dgamma(X, shape= 1.3, rate = 0.075))



DF_DISTRI <- rbind(DF_gamma_st1, DF_gamma_st2)
DF_DISTRI$logPDF <- log(DF_DISTRI$PDF)

DIS_PLOT<- ggplot() +
  geom_histogram(data= DF_HARVEST_GAMMA, size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       mycolsStates)+
  #theme(panel.background = element_blank())+
  theme_bw()+
  labs(x= "Step size (in m)", y= "Probability Density Function", col= "State")

#ggsave(DIS_PLOT, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/histoGAMMA.png", height = 6, width = 6, device = "png")

ggsave(DIS_PLOT, filename= "../output/finalFigures/histoGAMMA.png", height = 6, width = 6, device = "png")


##############################################################333


dataStates <- DF_HARVEST_GAMMA %>%
  group_by(Finca, ID_POR_FINCA, state)%>%
  summarise(numStates = sum(conteo)) 

dataStates <- dataStates %>%
  ungroup()%>%  #no entiendo que estaba agrupado, supongo que el ID con el state...
  complete(Finca, ID_POR_FINCA, state)%>%
  filter(state==2)


dataStates$numStates[is.na(dataStates$numStates)] <- 0 


dataTempTot <-  DF_HARVEST_GAMMA %>%
  group_by(Finca, ID_POR_FINCA) %>%
  summarise(totalStep = sum(conteo)) 

dataStates$totalStep <- dataTempTot$totalStep

dataStates$percentage_ST2 <- (dataStates$numStates/dataStates$totalStep)*100


HIST_ST1 <- dataStates %>%
  ggplot(col= "black", aes(x= Finca, y= percentage_ST2, 
                           ))+
  geom_boxplot(aes(fill= as.factor(Finca)))+ 
  geom_point(size= 3, aes(fill= as.factor(Finca)), shape= 21)+
  
  geom_segment(aes(x= 0, xend= 3, y=3.8, yend= 3.8), linetype= 2)+
  scale_fill_manual(values= c("#AAAAAA", "white"))+
  theme_bw()+
  labs(x= "Farm", y= "% of steps in State 2", fill= "Farm")

#ggsave(HIST_ST1, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/histoFARM.png", height = 6, width = 6, device = "png")
ggsave(HIST_ST1, filename= "../output/finalFigures/histoFARM.png", height = 6, width = 5, device = "png")


#############3prueba de medias

#dataStates_H <- dataStates %>% filter(Finca == "H")
#dataStates_I <- dataStates %>% filter(Finca == "I")

#varianza <- var.test(dataStates_H$percentage_ST2, dataStates_I$percentage_ST2)
#shapiro.test(dataStates_H$percentage_ST2)

#t.test(dataStates_I$percentage_ST2, dataStates_H$percentage_ST2)
