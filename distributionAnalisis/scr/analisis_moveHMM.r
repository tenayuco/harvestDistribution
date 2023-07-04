library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(moveHMM)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <-c("#759580", "#1b4a64")



#WP_COSECHA_UTM_SP <- read.csv("../data/cleanData_wayPointsCoffee_UTM.csv", stringsAsFactors = FALSE )
WP_COSECHA_UTM_SP <- read.csv("archivosTrabajandose/harvestDistribution/distributionAnalisis/data/cleanData_wayPointsCoffee_UTM.csv", stringsAsFactors = FALSE )

WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>%
  mutate(finca = replace(finca, finca == "Irlanda", "I")) %>%
  mutate(finca = replace(finca, finca == "Hamburgo", "H"))

WP_COSECHA_UTM_SP$zona[WP_COSECHA_UTM_SP$zona == "falta"] <- "zonaBaja"

WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>% rowwise() %>% 
  group_by(ID_REC, pante)%>%  #esto debe seguirse con lo de aajo
  mutate(xNorm = x_UTM - min(x_UTM)) %>%
  mutate(yNorm = y_UTM - min(y_UTM))

#now the precision (not the accuracy) of the GPS goes to 5 decimal points (lat y lot) and this is equivalent to ~1m
#so we can round the xNorm and y yNorm

WP_COSECHA_UTM_SP$xNorm <- round(WP_COSECHA_UTM_SP$xNorm, 0)
WP_COSECHA_UTM_SP$yNorm <- round(WP_COSECHA_UTM_SP$yNorm, 0)

WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>%
  filter(delta !=0)

WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>%
  dplyr::select("ID" = ID_REC, xNorm, yNorm)

WP_COSECHA_UTM_SP$pante <- NULL



#########################IRREGULAR VEAMOS

data <- prepData(WP_COSECHA_UTM_SP, type= "UTM", coordNames = c("xNorm", "yNorm"))

data <- data %>% 
  filter(step!= 0)

summary(data)


plot(data)

################
#viene el loop para ver cuál de todas las combinaciones de dos estados es las que arroja los mejores intervalos de confianza

## initial parameters 
mu0 <- c(1,5) # step mean (two parameters: one for each state)
sigma0 <- c(1,1) # step SD
#zeromass0 <- c(0.1,0.05) # step zero-mass  #este solo si tengo ceros
#stepPar0 <- c(mu0,sigma0,zeromass0)
stepPar0 <- c(mu0,sigma0)

angleMean0 <- c(0,0) # angle mean
kappa0 <- c(1,1) # angle concentration
anglePar0 <- c(angleMean0,kappa0)

m<- fitHMM(data = data, nbStates = 2 , stepPar0 = stepPar0, anglePar0 = anglePar0)

CIM <- CI(m)


#viene el loop para ver cuál de todas las combinaciones de dos estados es las que arroja los mejores intervalos de confianza




## initial parameters 

######33

DF_CI <- data.frame("mu2"= 0, "upperLower"= 0,  "state"= 0,  "valueCI"= 0, "meanSd"= 0 )

for (mu2 in seq(0.1,5,0.5)){
  angleMean0 <- c(0,0) # angle mean
  kappa0 <- c(1,1) # angle concentration
  anglePar0 <- c(angleMean0,kappa0)
  sigma0 <- c(1,1) # step SD
  mu0 <- c(1,mu2)
  stepPar0 <- c(mu0,sigma0)
  m_Temp<- fitHMM(data = data, nbStates = 2 , stepPar0 = stepPar0, anglePar0 = anglePar0)
  CI_Temp = CI(m_Temp)
  
  for(i in seq(1,2)){
    for(j in seq(1,2)){
      steits <- c("state1", "state2")
      meanOsd<- c("mean", "sd")
      
      DF_TEMP_lower <-  data.frame("mu2"= mu2, "upperLower"= "lower", "meanSd"= meanOsd[i], "state"= steits[j]  , "valueCI"=CI_Temp$stepPar$lower[i,j])
      DF_TEMP_upper <-  data.frame("mu2"= mu2, "upperLower"= "upper", "meanSd"= meanOsd[i], "state"= steits[j]  , "valueCI"=CI_Temp$stepPar$upper[i,j])
      
      DF_CI <- rbind(DF_CI, DF_TEMP_lower, DF_TEMP_upper)
      }
  }
  }



FIG_CI <- DF_CI %>% 
  filter(mu2 !=0) %>%
  ggplot(aes(x= mu2, y= valueCI))+
  geom_line(size= 1.5, aes(color= upperLower))+
  #geom_point(aes(y= AverageRust, color= as.character(porcionCosecha)), size= 2)+
  facet_grid(meanSd ~state)+
  scale_color_manual(values = mycols3a)
 
#theme(legend.position = "none")




