
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

#median delta per traje
WP_COSECHA_UTM_SP <- read.csv("../data/cleanData_wayPointsCoffee_UTM.csv", stringsAsFactors = FALSE )

#punctual modifications
WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>%
  mutate(finca = replace(finca, finca == "Ecological", "E")) %>%
  mutate(finca = replace(finca, finca == "Conventional", "C"))

#normalization of UTM
WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>% rowwise() %>% 
  group_by(finca, ID_POR_FINCA, pante)%>% 
  mutate(xNorm = x_UTM - min(x_UTM)) %>%
  mutate(yNorm = y_UTM - min(y_UTM))

#now the precision (not the accuracy) of the GPS goes to 5 decimal points (lat y lot) and this is equivalent to ~1m
#so we can round the xNorm and y yNorm
WP_COSECHA_UTM_SP$xNorm <- round(WP_COSECHA_UTM_SP$xNorm, 0)
WP_COSECHA_UTM_SP$yNorm <- round(WP_COSECHA_UTM_SP$yNorm, 0)

#we remove some spurious data were delta = 0
WP_COSECHA_UTM_SP <- WP_COSECHA_UTM_SP %>%
  filter(delta !=0)%>%
  unite("Finca_ID", finca, ID_POR_FINCA, remove = TRUE)


#we create this ID column to prepare the data for movehmm
WP_COSECHA_UTM_SP_CALCULOS <- WP_COSECHA_UTM_SP %>%
  dplyr::select("ID" = Finca_ID, xNorm, yNorm, delta, altura..ft., pante)

dataCosechaCALCULOS <- prepData(WP_COSECHA_UTM_SP_CALCULOS, type= "UTM", coordNames = c("xNorm", "yNorm"))

dataCosechaCALCULOS <- dataCosechaCALCULOS%>% 
  filter(step!= 0)  #we remove the zeros

dataRES_time<- dataCosechaCALCULOS %>%
  group_by(ID)%>%
  mutate(conteo = 1)%>%
  summarise("DELTA_TOTAL" = sum(delta)/60, "TREE_TOTAL" = sum(conteo)) %>%
  mutate(treeTime = (TREE_TOTAL/DELTA_TOTAL))


DF_ALTURAS <- dataCosechaCALCULOS %>%
  group_by(ID, pante) %>%
  summarise(MAX_ALT = max(altura..ft.), MIN_ALT = min(altura..ft.), DeltaMean = mean(delta))
DF_ALTURAS$DIF_ft <- DF_ALTURAS$MAX_ALT - DF_ALTURAS$MIN_ALT
DF_ALTURAS$DIF_m <- DF_ALTURAS$DIF_ft * 0.3048

DF_ALTURAS<- DF_ALTURAS %>% group_by(ID)%>%
  summarise(Dif_m_Finca = mean(DIF_m))


#AREAS <-data.frame("AREAS" = c(25, 18, 12, 18, 22, 16, 37, 38, 31, 25, 19, 24))
WORKERS_TOTAL <-data.frame("WORKERS_TOTAL" = c(3,5,2,3,3,3, 7,2,3,8,8,2))
#JUMPS_TOTAL <-data.frame("JUMPS_TOTAL" = c(2, 0, 0, 1, 5, 3, 8, 6, 8, 3, 4, 2))


DF_TOTAL <- cbind(dataRES_time, DF_ALTURAS[,2], WORKERS_TOTAL)

#write.csv(DF_TOTAL, "archivosTrabajandose/harvestDistribution/distributionAnalisis/output/diferenciasFincas.csv")




##################33 VOLVERLO A HACER


DF_TOTAL<- DF_TOTAL %>%
  separate(ID, sep = "_", into = c("FINCA", "numFINCA"))

DF_TOTAL_C <- DF_TOTAL %>%
  filter(FINCA =="C")
DF_TOTAL_E <- DF_TOTAL %>%
  filter(FINCA =="E")

#todas son normales
shapiro.test(DF_TOTAL_C$TREE_TOTAL)
shapiro.test(DF_TOTAL_C$WORKERS_TOTAL) #no normal
shapiro.test(DF_TOTAL_C$treeTime)
shapiro.test(DF_TOTAL_C$Dif_m_Finca) #no normal!


shapiro.test(DF_TOTAL_E$TREE_TOTAL)
shapiro.test(DF_TOTAL_E$WORKERS_TOTAL) #no
shapiro.test(DF_TOTAL_E$treeTime)  #NO NORMAL
shapiro.test(DF_TOTAL_E$Dif_m_Finca)
# varianzas

var.test(DF_TOTAL_C$TREE_TOTAL, DF_TOTAL_E$TREE_TOTAL)
var.test(DF_TOTAL_C$treeTime, DF_TOTAL_E$treeTime)
var.test(DF_TOTAL_C$WORKERS_TOTAL, DF_TOTAL_E$WORKERS_TOTAL)
var.test(DF_TOTAL_C$Dif_m_Finca, DF_TOTAL_E$Dif_m_Finca)

#Vemos pruebas  #ar.true o var.equal false hace que sea de studen o de welch. el de welch es menos pontente. pero mas seguro. 

arbolesT <- t.test(DF_TOTAL_C$TREE_TOTAL, DF_TOTAL_E$TREE_TOTAL, var.equal = TRUE)
arbolesT_tiempo <- wilcox.test(DF_TOTAL_C$treeTime, DF_TOTAL_E$treeTime, var.equal = TRUE)
workersT <- wilcox.test(DF_TOTAL_C$WORKERS_TOTAL, DF_TOTAL_E$WORKERS_TOTAL, var.equal = TRUE)
alturaT <- wilcox.test(DF_TOTAL_C$Dif_m_Finca, DF_TOTAL_E$Dif_m_Finca, var.equal = TRUE)

######################3

DF_TOTAL_RES_MEAN <- DF_TOTAL %>% group_by(FINCA) %>%
  summarise_all(mean)

DF_TOTAL_RES_SD <- DF_TOTAL %>% group_by(FINCA) %>%
  summarise_all(sd)

#alturas ANAISIS SEPARADO

DF_AREAS <- read.csv("../data/areaPorGrano.csv")

DF_PRUEBAS_AREA <- data.frame("AR" = 0, "Equity" = 0, "Normality"= 0, "t_test" =0, "pvalue" = 0)



for (area in unique(DF_AREAS$AR)){

  DF_AREAS_F <- DF_AREAS %>% filter(AR == area)
  DF_AREAS_F_C <- DF_AREAS_F%>% filter(farm == "C")
  DF_AREAS_F_O <- DF_AREAS_F%>% filter(farm == "O")

  VAR <- var.test(DF_AREAS_F_C$conteoArea, DF_AREAS_F_O$conteoArea)
  equity = VAR$p.value >0.05 #saca true si la vaianzar no es suficientemente diferrente)
  
  SH_C <- shapiro.test(DF_AREAS_F_C$conteoArea)
  SH_O <- shapiro.test(DF_AREAS_F_O$conteoArea)
  
  if (SH_C$p.value < 0.05 |SH_O$p.value < 0.05  ){
    print("alguno es no normal")
    areaT <- wilcox.test(DF_AREAS_F_C$conteoArea, DF_AREAS_F_O$conteoArea, var.equal = equity)
    normal <- "no"
    DF_LINE <- data.frame("AR" = area, "Equity" = equity, "Normality"= normal, "t_test"= areaT$method,  "pvalue" =areaT$p.value)
    DF_PRUEBAS_AREA <- rbind(DF_PRUEBAS_AREA, DF_LINE)
    
  }
  else {
    print("todo  es normal")
    areaT <- t.test(DF_AREAS_F_C$conteoArea, DF_AREAS_F_O$conteoArea, var.equal = equity)
    normal <- "yes"
    DF_LINE <- data.frame("AR" = area, "Equity" = equity, "Normality"= normal, "t_test"= areaT$method,  "pvalue" =areaT$p.value)
    
    DF_PRUEBAS_AREA <- rbind(DF_PRUEBAS_AREA, DF_LINE)
  }
  
}


DF_PRUEBAS_AREA <- DF_PRUEBAS_AREA %>% filter(AR !=0)

write_csv(DF_PRUEBAS_AREA, file = "pruebasArea.csv")