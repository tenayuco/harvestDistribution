
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



#DF_HARVEST_GAMMA_E <- read.csv("/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/data/analyzedData_figuresFINCA_E_.csv")
#DF_HARVEST_GAMMA_C <- read.csv("/home/emilio/archivosTrabajandose/harvestDistribution/distributionAnalisis/data/analyzedData_figuresFINCA_C_.csv")

DF_HARVEST_GAMMA_E <- read.csv("../data/analyzedData_figuresFINCA_E_.csv")
DF_HARVEST_GAMMA_C <- read.csv("../data/analyzedData_figuresFINCA_C_.csv")



DF_HARVEST_GAMMA <- rbind(DF_HARVEST_GAMMA_E, DF_HARVEST_GAMMA_C)

rm(DF_HARVEST_GAMMA_C)
rm(DF_HARVEST_GAMMA_E)
#DF_HARVEST_GAMMA_E <- read.csv("../data")


#aqui invierto el 1 con el 2 por medio de una funcon linea

#DF_HARVEST_GAMMA$state <-  (-DF_HARVEST_GAMMA$state) + 3

## Y agrego el nombre (long y short step)
#ver bien cuál quiero

DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "1"] <- "Collect"
DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "2"] <- "Search"



DF_HARVEST_GAMMA <- DF_HARVEST_GAMMA %>%
  separate(ID, into = c("farm", "IDREC"), sep = "_", remove = T)

#DF_HARVEST_GAMMA$farm[DF_HARVEST_GAMMA$farm== "H"] <- "C"
DF_HARVEST_GAMMA$farm[DF_HARVEST_GAMMA$farm== "E"] <- "O"

DF_HARVEST_RESUMEN <- DF_HARVEST_GAMMA %>%
  group_by(farm, IDREC)%>%
  summarise(observation = sum(conteo)) %>%
  unite("farm_ID", c(farm, IDREC))




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


################################################################################33

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
                       groupColors2)+
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
                       groupColors2)+
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


####################


binaryPlot <- DF_HARVEST_GAMMA %>%
  unite("farm_ID", c(farm, IDREC),remove = FALSE) %>% 
  ggplot(aes(x= contador , y= farm_ID, fill= as.factor(state)))+
  geom_tile(col= "black")+
  facet_wrap(~farm, scales= "free_y", ncol= 1)+
  scale_fill_manual(values= c("#FFFFFF", "black"))+
 # scale_y_continuous(breaks=seq(1,6,1))+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 20))+
  #theme(legend.position = "bottom")+
  labs(x= "Steps", y= "ID", fill= "State", shape= "State")




DF_HARVEST_GAMMA_RES_2 <- DF_HARVEST_GAMMA %>%
  group_by(state, farm, IDREC) %>%
  summarise(meanStep = mean(step), numStates = sum(conteo))



DF_HARVEST_GAMMA_RES_2_S <- DF_HARVEST_GAMMA_RES_2 %>%
  ungroup()%>%  #no entiendo que estaba agrupado, supongo que el ID con el state...
  complete(farm, IDREC, state)%>%
  filter(state=="Search")


DF_HARVEST_GAMMA_RES_2_S$numStates[is.na(DF_HARVEST_GAMMA_RES_2_S$numStates)] <- 0 
DF_HARVEST_GAMMA_RES_2_S$meanStep[is.na(DF_HARVEST_GAMMA_RES_2_S$meanStep)] <- 0 

mediaGeneral <- mean(DF_HARVEST_GAMMA_RES_2_S$meanStep)

dataTempTot <-  DF_HARVEST_GAMMA %>%
  group_by(farm, IDREC) %>%
  summarise(totalStep = sum(conteo)) 

DF_HARVEST_GAMMA_RES_2_S$totalStep <- dataTempTot$totalStep

DF_HARVEST_GAMMA_RES_2_S$percentage_ST2 <- (DF_HARVEST_GAMMA_RES_2_S$numStates/DF_HARVEST_GAMMA_RES_2_S$totalStep)*100

#VERIFICAR ESTO  

HIST_ST1 <- DF_HARVEST_GAMMA_RES_2_S %>%
  ggplot(col= "black", aes(x= farm, y= meanStep, 
  ))+
  geom_boxplot(aes(fill= as.factor(farm)))+ 
  geom_point(size= 3, aes(fill= as.factor(farm)), shape= 21)+
  
  geom_segment(aes(x= 0, xend= 3, y=mediaGeneral, yend= mediaGeneral), linetype= 2)+
  scale_fill_manual(values= c("#AAAAAA", "white"))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(legend.position = "None")+
  labs(x= "Plantation", y= "Total distance  of steps in state Search", fill= "Plantation")



FIG_STATES <- binaryPlot + HIST_ST1 + plot_layout(widths = c(2, 1))

ggsave(FIG_STATES, filename= "../output/finalFigures/figStates_porFinca.pdf", height = 6, width = 12, device = "pdf")
