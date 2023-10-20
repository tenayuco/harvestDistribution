### este va a ser el script para generar el output de las figuras finales

library(patchwork)
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


#aqui invierto el 1 con el 2 por medio de una funcon linea

DF_HARVEST_GAMMA$state <-  (-DF_HARVEST_GAMMA$state) + 3

## Y agrego el nombre (long y short step)
#ver bien cuál quiero

#DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "1"] <- "1 (short steps)"
#DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "2"] <- "2 (long steps)"



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

#cambiams por E de ecolo
DF_HARVEST_GAMMA$Finca[DF_HARVEST_GAMMA$Finca== "H"] <- "C"
DF_HARVEST_GAMMA$Finca[DF_HARVEST_GAMMA$Finca== "I"] <- "E"

DF_HARVEST_RESUMEN <- DF_HARVEST_GAMMA %>%
  group_by(Finca, ID_POR_FINCA)%>%
  summarise(observation = sum(conteo)) %>%
  unite("Finca_ID", c(Finca, ID_POR_FINCA))



FIG_MAP_GAMMA<- DF_HARVEST_GAMMA %>% 
  unite("Finca_ID", c(Finca, ID_POR_FINCA)) %>% 
  ggplot(aes(x= x, y = y)) +
  geom_path(aes(col= as.factor(state), group = (Finca_ID)), size= 1)+
  geom_point(size=1, aes(fill= "Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  scale_color_manual(values= mycolsStates)+
  facet_wrap(~Finca_ID, ncol = 3)+
  geom_text(x = 140, y = 10, aes(label = observation), data = DF_HARVEST_RESUMEN)+
  geom_text(x = 125, y = 10, label= "N =")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(strip.background =element_rect(fill="white"))+
  #theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  labs(x= "X (in m)", y= "Y (in m)", col= "State", fill= "")


#ggsave(FIG_MAP_GAMMA, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/mapHarvest_.png", height = 12, width = 10, device = "png")
ggsave(FIG_MAP_GAMMA, filename= "../output/finalFigures/mapHarvest_.png", height = 14, width = 12, device = "png")


##########################333


binaryPlot <- DF_HARVEST_GAMMA %>%
  ggplot(aes(x= contador , y= ID_POR_FINCA, fill= as.factor(state)))+
  geom_tile(col= "black")+
  facet_wrap(~Finca, scales= "free_y", ncol= 1)+
  scale_fill_manual(values= c("#FFFFFF", "black"))+
  scale_y_continuous(breaks=seq(1,6,1))+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 20))+
  labs(x= "Steps", y= "ID", fill= "State", shape= "State")


###############################33333





X= seq(1, 120, 1)


#este no es igual que en el otro codigo ,porque invert los estados    
DF_gamma_st1 <- data.frame("distribution"= c("gamma"), "state" = 1, "X"= X,  "PDF" = dgamma(X, shape= 3.62, rate = 0.845))
DF_gamma_st2 <- data.frame("distribution"= c("gamma"), "state" = 2, "X"= X,  "PDF" = dgamma(X, shape= 1.3, rate = 0.075))



DF_DISTRI <- rbind(DF_gamma_st1, DF_gamma_st2)
DF_DISTRI$logPDF <- log(DF_DISTRI$PDF)


CAJADIST <- DF_HARVEST_GAMMA %>%
  ggplot()+
  geom_boxplot(aes(x= step), fill= "#EEEEEE")+ 
  theme_bw()+
  theme(text = element_text(size = 20))+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(x= "Step length (in m)", y= "")


DIS_PLOT<- ggplot() +
  geom_histogram(data= DF_HARVEST_GAMMA, size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       mycolsStates)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  labs(x= "Step length (in m)", y= "Frecuency", col= "State")

DIS_PLOT_COM <- DIS_PLOT + annotation_custom(ggplotGrob(CAJADIST), xmin = 15, xmax = 100, ymin = 0.1, ymax = 0.2)



#ggsave(DIS_PLOT, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/histoGAMMA.png", height = 6, width = 6, device = "png")



ggsave(DIS_PLOT_COM, filename= "../output/finalFigures/histoGAMMA.png", height = 6, width = 10, device = "png")





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
  theme(text = element_text(size = 20))+
  labs(x= "Farm", y= "Percentage of steps in state 2", fill= "Farm")

#ggsave(HIST_ST1, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/histoFARM.png", height = 6, width = 6, device = "png")
#ggsave(HIST_ST1, filename= "../output/finalFigures/histoFARM.png", height = 6, width = 5, device = "png")




FIG_STATES <- binaryPlot + HIST_ST1 + plot_layout(widths = c(2, 1))

#ggsave(FIG_STATES, filename= "../output/finalFigures/figStates_.png", height = 6, width = 12, device = "png")
ggsave(FIG_STATES, filename= "../output/finalFigures/figStates_.pdf", height = 6, width = 12, device = "pdf")


###esto para medidas por state

#dataState1 <- DF_HARVEST_GAMMA %>% filter(state ==1)
#> View(dataState1)
#> max(dataState1$step)
#[1] 13.41641
#> dataState2 <- DF_HARVEST_GAMMA %>% filter(state ==2)
#> max(dataState2$step)
#[1] 117.0171
#> min(dataState2$step)
#[1] 6.082763
#> min(dataState1$step)
#> dim(dataState1)[1]/dim(DF_HARVEST_GAMMA)[1]

#############3prueba de medias

#dataStates_H <- dataStates %>% filter(Finca == "H")
#dataStates_I <- dataStates %>% filter(Finca == "I")

#varianza <- var.test(dataStates_H$percentage_ST2, dataStates_I$percentage_ST2)
#shapiro.test(dataStates_H$percentage_ST2)

#t.test(dataStates_I$percentage_ST2, dataStates_H$percentage_ST2)

#####Para hacer conteo 


FIG_MAP_CONTEO<- DF_HARVEST_GAMMA %>% 
  unite("Finca_ID", c(Finca, ID_POR_FINCA)) %>% 
  ggplot(aes(x= x, y = y)) +
  geom_path(aes(col= as.factor(state)), size= 0.5)+
  geom_point(size=0.5, aes(fill= "Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  scale_color_manual(values= mycolsStates)+
  facet_wrap(~Finca_ID, ncol = 3)+
  geom_text(x = 140, y = 10, aes(label = observation), data = DF_HARVEST_RESUMEN)+
  geom_text(x = 125, y = 10, label= "N =")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(strip.background =element_rect(fill="white"))+
  scale_y_continuous(breaks = seq(0, 120, by = 10))+
  scale_x_continuous(breaks = seq(0, 150, by = 10))+
  theme(panel.grid.major = element_line(color = "red",
                                        size = 0.5,
                                        linetype = 1))+
  #theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  labs(x= "X (in m)", y= "Y (in m)", col= "State", fill= "")

ggsave(FIG_MAP_CONTEO, filename= "../output/finalFigures/mapCONTEO_.png", height = 14, width = 12, device = "png")

