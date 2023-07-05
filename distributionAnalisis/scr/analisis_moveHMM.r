library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(moveHMM)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <-c("#759580", "#1b4a64")



##Here we did a simple multistate analysis to corroborate the two-state model observed

#I. We load the data and modify it
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
  filter(delta !=0)%>%
  unite("Finca_ID_REC", finca, ID_REC, remove = FALSE)

WP_COSECHA_UTM_SP_PRE <- WP_COSECHA_UTM_SP %>%
  dplyr::select("ID" = Finca_ID_REC, xNorm, yNorm)

WP_COSECHA_UTM_SP_PRE$pante <- NULL
WP_COSECHA_UTM_SP_PRE$ID_REC <- NULL

############
##2. Now we prepare the data for hmm. Importantly we will only use the irregular data (time
#is not regular). In this sense, we only focus on the fact the big steps are present, independently 
#of the angle of the velocity

#########################IRREGULAR VEAMOS

dataCosecha <- prepData(WP_COSECHA_UTM_SP_PRE, type= "UTM", coordNames = c("xNorm", "yNorm"))

dataCosecha <- dataCosecha %>% 
  filter(step!= 0)  #we remove the zeros

summary(dataCosecha)



plot(dataCosecha)

################

## initial parameters 
mu0 <- c(1,5) # step mean (two parameters: one for each state)
sigma0 <- c(1,1) # step SD priors
#zeromass0 <- c(0.1,0.05) # step zero-mass  #este solo si tengo ceros
#stepPar0 <- c(mu0,sigma0,zeromass0)
stepPar0 <- c(mu0,sigma0)

angleMean0 <- c(0,0) # angle mean
kappa0 <- c(1,1) # angle concentration
anglePar0 <- c(angleMean0,kappa0)

m_cosecha<- fitHMM(data = dataCosecha, nbStates = 2 , stepPar0 = stepPar0, anglePar0 = anglePar0)

DF_meanSD <- as.data.frame(m_cosecha$mle$stepPar)

plot(m_cosecha, plotCI=TRUE)

CI_cosecha<- CI(m_cosecha)








#########ahora probamos con Weibull############3

shap1 <- c(1,5)
scal1 <- c(5, 10)
stepPar1 <- c(shap1,scal1)

#https://statisticsbyjim.com/probability/weibull-distribution/

m_weibull<- fitHMM(data = dataCosecha, stepDist = "weibull", nbStates = 2 , stepPar0 = stepPar1, anglePar0 = anglePar0)



############ ahora intentamos lognormal
#https://towardsdatascience.com/log-normal-distribution-a-simple-explanation-7605864fb67c

loc2 <- c(1, 2)
sig2 <- c(0.5, 1)

stepPar2 <- c(loc2,sig2)


#https://statisticsbyjim.com/probability/weibull-distribution/

m_logNorm<- fitHMM(data = dataCosecha, stepDist = "lnorm", nbStates = 2 , stepPar0 = stepPar1, anglePar0 = anglePar0)

AIC(m_cosecha, m_weibull, m_logNorm,  m3)

#por ejemplo, si probabamos uno de 3 

mu0 <- c(1, 5, 10)
sigma0 <- c(1,1,1)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(0,0,0)
kappa0 <- c(1,1,1)
anglePar0 <- c(angleMean0,kappa0)
# fit the 3-state model
m3 <- fitHMM(data=dataCosecha,nbStates=3,stepPar0=stepPar0,
             anglePar0=anglePar0)


##########criterio de akaike
AIC(m_cosecha,m3)






#viene el loop para ver cuál de todas las combinaciones de dos estados es las que arroja los mejores intervalos de confianza







#### ESTO ES POST ANALISIS


states <- viterbi(m_cosecha)

dataCosecha$states <- states
#probabilidad de estar en un estado o en el otro. 
#PERO FATLA hacerlo por FINCA. Y esto es lo que sepuede publicar DAI!

dataCosecha <- dataCosecha %>%
  separate(ID, into= c("finca", "ID_REC"), sep = "_")

##########

H_dataCosecha <- dataCosecha %>%
  filter(finca== "H")
I_dataCosecha <- dataCosecha %>%
  filter(finca== "I")

H_tableStates <- table(H_dataCosecha$states)/length(H_dataCosecha$states)
I_tableStates <- table(I_dataCosecha$states)/length(I_dataCosecha$states)


plotStates(m_cosecha)


########pruebas a ver de donde salen los promedios#########3

dataCosecha_1 <- dataCosecha %>% 
  filter(states==1)

mean(dataCosecha_1$step, na.rm = TRUE)
mean(dataCosecha_1$step, na.rm = TRUE)





#hacer criterio de AKAIKE  VAMOS A DARLE A ESTO


## initial parameters 









######33

##Sensitivity analisis for the confidence intervals

sens = 0
if (sens ==1){

DF_CI <- data.frame("sig"= 0, "upperLower"= 0,  "state"= 0,  "valueCI"= 0, "meanSd"= 0 )

for (sig in seq(0.1,5,0.5)){
  angleMean0 <- c(0,0) # angle mean
  kappa0 <- c(1,1) # angle concentration
  anglePar0 <- c(angleMean0,kappa0)
  sigma0 <- c(sig,sig) # step SD
  mu0 <- c(1,5)
  stepPar0 <- c(mu0,sigma0)
  m_Temp<- fitHMM(data = dataCosecha, nbStates = 2 , stepPar0 = stepPar0, anglePar0 = anglePar0)
  CI_Temp = CI(m_Temp)
  
  for(i in seq(1,2)){
    for(j in seq(1,2)){
      steits <- c("state1", "state2")
      meanOsd<- c("mean", "sd")
      
      DF_TEMP_lower <-  data.frame("sig"= sig, "upperLower"= "lower", "meanSd"= meanOsd[i], "state"= steits[j]  , "valueCI"=CI_Temp$stepPar$lower[i,j])
      DF_TEMP_upper <-  data.frame("sig"= sig, "upperLower"= "upper", "meanSd"= meanOsd[i], "state"= steits[j]  , "valueCI"=CI_Temp$stepPar$upper[i,j])
      
      DF_CI <- rbind(DF_CI, DF_TEMP_lower, DF_TEMP_upper)
      }
  }
  }



FIG_CI <- DF_CI %>% 
  filter(sig !=0) %>%
  ggplot(aes(x= sig, y= valueCI))+
  geom_line(size= 1.5, aes(color= upperLower))+
  #geom_point(aes(y= AverageRust, color= as.character(porcionCosecha)), size= 2)+
  facet_grid(meanSd ~state)+
  scale_color_manual(values = mycols3a)
 
}
#theme(legend.position = "none")




