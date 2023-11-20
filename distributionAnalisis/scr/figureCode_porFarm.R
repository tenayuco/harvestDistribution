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

mycolsStates <-c("#fd9706", "#1b4a64" )


#1. We first import our data base with the states assigned (from the 2 fincas analysis)

#DF_HARVEST_GAMMA_E <- read.csv("/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/data/analyzedData_figuresFINCA_E_.csv")
#DF_HARVEST_GAMMA_C <- read.csv("/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/data/analyzedData_figuresFINCA_C_.csv")


DF_HARVEST_GAMMA_E <- read.csv("../data/analyzedData_figuresFINCA_E_.csv")
DF_HARVEST_GAMMA_C <- read.csv("../data/analyzedData_figuresFINCA_C_.csv")


DF_HARVEST_GAMMA <- rbind(DF_HARVEST_GAMMA_E, DF_HARVEST_GAMMA_C)

rm(DF_HARVEST_GAMMA_C)
rm(DF_HARVEST_GAMMA_E)


DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "1"] <- "Collect"
DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "2"] <- "Search"

#We separte the ID to have more flexibilty
DF_HARVEST_GAMMA <- DF_HARVEST_GAMMA %>%
  separate(ID, into = c("farm", "IDREC"), sep = "_", remove = T)

#We change the name of the rfam
DF_HARVEST_GAMMA$farm[DF_HARVEST_GAMMA$farm== "E"] <- "O"

#this is made to know the number of steps per fram
DF_HARVEST_RESUMEN <- DF_HARVEST_GAMMA %>%
  group_by(farm, IDREC)%>%
  summarise(observation = sum(conteo)) %>%
  unite("farm_ID", c(farm, IDREC))

#we plot the states. It is important to notice that states do not represent the same
FIG_MAP_GAMMA<- DF_HARVEST_GAMMA %>% 
  unite("farm_ID", c(farm, IDREC)) %>% 
  ggplot(aes(x= x, y = y)) +
  geom_path(aes(col= as.factor(state), group = (farm_ID)), size= 1)+
  geom_point(size=1, aes(fill= "Visited Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  scale_color_manual(values= mycolsStates)+
  facet_wrap(~farm_ID, ncol = 3)+
  geom_text(x = 140, y = 10, aes(label = observation), data = DF_HARVEST_RESUMEN)+
  geom_text(x = 125, y = 10, label= "N =")+
  theme_bw()+
  theme(text = element_text(size = 15))+
  theme(strip.background =element_rect(fill="white"))+
  #theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  labs(x= "X (in m)", y= "Y (in m)", col= "State", fill= "")


#ggsave(FIG_MAP_GAMMA, filename= "/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/output/finalFigures/mapHarvest_porFarm.png", height = 14, width = 12.5, device = "png")
ggsave(FIG_MAP_GAMMA, filename= "../output/finalFigures/mapHarvest_porFarm.png", height = 14, width = 12.5, device = "png")

#this is to know the range and mean for each satae
DF_RESUMEN_STATE <- DF_HARVEST_GAMMA %>%
  group_by(farm, state) %>%
  summarise(meanDistance = mean(step), sdDistance = sd(step), minDistance = min(step), maxDistance = max(step), numStates = sum(conteo))

write.csv(DF_RESUMEN_STATE, file = "../data/df_resumen_perState.csv")

################################################################################33
#2. We plot the distributions


#DF_TOTAL_MIN <- read.csv("archivosTrabajandose/harvestDistribution/distributionAnalisis/output/DF_TOTAL_AIC_porFincas.csv")
DF_TOTAL_MIN <- read.csv("../output/DF_TOTAL_AIC_porFincas.csv")



DF_TOTAL_MIN_2 <- DF_TOTAL_MIN %>% 
  filter(model == "gamma")%>% 
  filter(states == 2)

DF_TOTAL_MIN_2$farm[DF_TOTAL_MIN_2$farm == "E"] <- "O"

#here we generate the distributions with the sobtained parameters 

##now!! it is important to recall that the program gives us the mean adn the sd fro garmma. To transform and pĺot we havve 
# shape α = µ2 /σ 2
# rate β = µ/σ 2
#scale λ = 1/β.

MIN_USADOS <- DF_TOTAL_MIN_2

MIN_USADOS$shape_st1 <- (MIN_USADOS$st1_par0*MIN_USADOS$st1_par0)/(MIN_USADOS$st1_par1* MIN_USADOS$st1_par1)
MIN_USADOS$shape_st2 <- (MIN_USADOS$st2_par0*MIN_USADOS$st2_par0)/(MIN_USADOS$st2_par1* MIN_USADOS$st2_par1)

MIN_USADOS$rate_st1 <- (MIN_USADOS$st1_par0)/(MIN_USADOS$st1_par1*MIN_USADOS$st1_par1)
MIN_USADOS$rate_st2 <- (MIN_USADOS$st2_par0)/(MIN_USADOS$st2_par1*MIN_USADOS$st2_par1)

MIN_USADOS <- MIN_USADOS %>%
  dplyr::select(farm, shape_st1, shape_st2, rate_st1, rate_st2)


MIN_USADOS_R <- MIN_USADOS %>% melt(id.vars = c("farm"), variable.name = "variable", value.name = "value")

MIN_USADOS_R <- MIN_USADOS_R %>% 
 separate(variable, into = c("parameter", "state"), sep = "_")

MIN_USADOS_R$state[MIN_USADOS_R$state == "st1"] <- "Collect"
MIN_USADOS_R$state[MIN_USADOS_R$state == "st2"] <- "Search"
#CVer tabla para valores)
X= seq(1, 120, 1)

DF_DISTRI <- data.frame("distribution"= c("gamma"), "farm" = "C", "state" = 0, "X"= 0,  "PDF" = 0)

for (f in MIN_USADOS_R$farm){
  for(st in MIN_USADOS_R$state){
    MIN_USADOS_R_T <- MIN_USADOS_R %>%
      filter((farm== f) & (state == st))
    DF_gamma <- data.frame("distribution"= c("gamma"), "farm" = f, "state" = st, "X"= X,  
                           "PDF" = dgamma(X, shape = MIN_USADOS_R_T$value[MIN_USADOS_R_T$parameter == "shape"], 
                                          rate = MIN_USADOS_R_T$value[MIN_USADOS_R_T$parameter == "rate"]))
    DF_DISTRI <- rbind(DF_DISTRI, DF_gamma)
  }
}

DF_DISTRI <- DF_DISTRI %>%
  filter(!state ==0)

DF_DISTRI_O <-DF_DISTRI  %>%
  filter(farm == "O")
DF_DISTRI_C <- DF_DISTRI %>%
  filter(farm == "C")


CAJADIST_O<- DF_HARVEST_GAMMA %>%
  filter(farm == "O") %>%
  ggplot()+
  geom_boxplot(aes(x= step), fill= "#EEEEEE")+ 
  theme_bw()+
  theme(text = element_text(size = 20))+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlim(0, 120)+
  labs(x= "Step length (m)", y= "")


DIS_PLOT_O<- DF_HARVEST_GAMMA %>%
  filter(farm == "O") %>%
  ggplot() +
  geom_histogram(size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI_O, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       mycolsStates)+
  facet_wrap(~farm)+
  ylim(0, 0.32)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(axis.title.y = element_blank())+
  theme(strip.background =element_rect(fill="white"))+
  labs(x= "Step length (m)", y= "Frequency", col= "State")


DIS_PLOT_COM_O <- DIS_PLOT_O + annotation_custom(ggplotGrob(CAJADIST_O), xmin = 15, xmax = 100, ymin = 0.1, ymax = 0.2)

#####################################
CAJADIST_C<- DF_HARVEST_GAMMA %>%
  filter(farm == "C") %>%
  ggplot()+
  geom_boxplot(aes(x= step), fill= "#EEEEEE")+ 
  theme_bw()+
  theme(text = element_text(size = 20))+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlim(0, 120)+
  labs(x= "Step length (m)", y= "")


DIS_PLOT_C<- DF_HARVEST_GAMMA %>%
  filter(farm == "C") %>%
  ggplot() +
  geom_histogram(size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI_C, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       mycolsStates)+
  facet_wrap(~farm)+
  ylim(0, 0.32)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(legend.position = "none")+
  theme(strip.background =element_rect(fill="white"))+
  labs(x= "Step length (m)", y= "Frequency", col= "State")


DIS_PLOT_COM_C <- DIS_PLOT_C + annotation_custom(ggplotGrob(CAJADIST_C), xmin = 15, xmax = 100, ymin = 0.1, ymax = 0.2)

library(patchwork)
DIS_PLOT_TOTAL <- DIS_PLOT_COM_C + DIS_PLOT_COM_O


ggsave(DIS_PLOT_TOTAL, filename= "../output/finalFigures/histoGAMMA_porPlantacion.png", height = 6, width = 15, device = "png")


######################################################################3

##3. We analize the areas diferences, according to different grain or rust transmision 




##comparacion entre areas vs distancia por farm sin considerar state

DF_HARVEST_GAMMA_RES <- DF_HARVEST_GAMMA %>%
  group_by(farm, IDREC) %>%
  summarise(distance = sum(step), numStates = sum(conteo))

DF_HARVEST_TOTAL <- DF_HARVEST_GAMMA_RES
DF_HARVEST_TOTAL$AR <- 0


for (AR in c(1,2.5, 5, 10, 20, 40)){ 

  DF_H_T <- DF_HARVEST_GAMMA_RES
  DF_H_T$AR <- AR
    
DF_HARVEST_GAMMA$x_area <- round(DF_HARVEST_GAMMA$x/AR)
DF_HARVEST_GAMMA$y_area <- round(DF_HARVEST_GAMMA$y/AR)
DF_HARVEST_GAMMA <- DF_HARVEST_GAMMA %>%
  unite(sec_area, c(x_area, y_area), sep = "_")

DF_HARVEST_GAMMA_AREA <- DF_HARVEST_GAMMA %>%
  group_by(farm, IDREC, sec_area)%>%
  summarise(conteo = sum(conteo)) %>%
  mutate(conteo = 1)

DF_HARVEST_GAMMA_AREA <- DF_HARVEST_GAMMA_AREA  %>%
  group_by(farm, IDREC)%>%
  summarise(conteoArea = sum(conteo))

print(AR)

DF_H_T$conteoArea <- DF_HARVEST_GAMMA_AREA$conteoArea

rm(DF_HARVEST_GAMMA_AREA)

###############

DF_H_T$areaNorm <- DF_H_T$conteoArea/DF_H_T$numStates

DF_HARVEST_TOTAL <- rbind(DF_HARVEST_TOTAL, DF_H_T)

}

DF_HARVEST_TOTAL <- DF_HARVEST_TOTAL %>%
  filter(AR !=0)



HIST_ST1 <- DF_HARVEST_TOTAL %>%
  ggplot(col= "black", aes(x= farm, y= areaNorm, 
  ))+
  geom_boxplot(aes(fill= as.factor(farm)))+ 
  geom_point(size= 3, aes(fill= as.factor(farm)), shape= 21)+
  
  #geom_segment(aes(x= 0, xend= 3, y=mediaGeneral, yend= mediaGeneral), linetype= 2)+
  scale_fill_manual(values= c("#AAAAAA", "white"))+
  facet_wrap(~AR, nrow = 1)+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 20))+
  theme(legend.position = "None")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.spacing = unit(0, "lines"))+
  labs(x= "Plantation", y= "Normalized number of squares visited", fill= "Plantation", title = "Length of the square grain (in m)")

ggsave(HIST_ST1, filename= "../output/finalFigures/figStates_porFinca_porGrano.png", height = 6, width = 12, device = "png")

write_csv(DF_HARVEST_TOTAL, file = "../data/areaPorGrano.csv")




#binaryPlot <- DF_HARVEST_GAMMA %>%
 # unite("farm_ID", c(farm, IDREC),remove = FALSE) %>% 
  #ggplot(aes(x= contador , y= farm_ID, fill= as.factor(state)))+
  #geom_tile(col= "black")+
  #facet_wrap(~farm, scales= "free_y", ncol= 1)+
  #scale_fill_manual(values= c("#FFFFFF", "black"))+
  # scale_y_continuous(breaks=seq(1,6,1))+
  #theme_bw()+
  #theme(strip.background =element_rect(fill="white"))+
  #theme(text = element_text(size = 20))+
  #theme(legend.position = "bottom")+
  #labs(x= "Steps", y= "ID", fill= "State", shape= "State")


#FIG_STATES <- binaryPlot + HIST_ST1 + plot_layout(widths = c(2, 1))

#ggsave(FIG_STATES, filename= "../output/finalFigures/figStates_porFinca.pdf", height = 6, width = 12, device = "pdf")


#DF_RESUMEN <- DF_HARVEST_GAMMA_RES %>%
 # group_by(farm) %>%
  #summarise(meanTrees = mean(numStates), sdTrees = sd(numStates), meanArea = mean(conteoArea), sdArea = sd(conteoArea), meanAreaNorm = mean(areaNorm), sdAreaNorm = sd(areaNorm  ))
                                                                                                                                                                  


######################3333


# 
# FIG_MAP_CONTEO <- DF_HARVEST_GAMMA %>% 
#   #filter(state == "Collect")%>% 
#   unite("farm_ID", c(farm, IDREC)) %>% 
#   ggplot(aes(x= x, y = y)) +
#   geom_path(aes(col= as.factor(state)), size= 0.5)+
#   geom_point(size=0.5, aes(fill= "Visited Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
#   scale_color_manual(values= mycolsStates)+
#   facet_wrap(~farm_ID, ncol = 3)+
#   geom_text(x = 140, y = 10, aes(label = observation), data = DF_HARVEST_RESUMEN)+
#   geom_text(x = 125, y = 10, label= "N =")+
#   theme_bw()+
#   theme(text = element_text(size = 15))+
#   theme(strip.background =element_rect(fill="white"))+
#   scale_y_continuous(breaks = seq(0, 120, by = 10))+
#   scale_x_continuous(breaks = seq(0, 150, by = 10))+
#   theme(panel.grid.major = element_line(color = "red",
#                                         size = 0.5,
#                                         linetype = 1))+
#   #theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
#   labs(x= "X (in m)", y= "Y (in m)", col= "State", fill= "")
