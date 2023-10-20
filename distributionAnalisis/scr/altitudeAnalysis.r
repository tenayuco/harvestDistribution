library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(moveHMM)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <-c("#759580", "#1b4a64")
colorsGris <- c("black","#555555", "white")



##Here we did a simple multistate analysis to corroborate the two-state model observed

#I. We load the data and modify it
WP_COSECHA_UTM_SP <- read.csv("../data/cleanData_wayPointsCoffee_UTM.csv", stringsAsFactors = FALSE )
#WP_COSECHA_UTM_SP <- read.csv("archivosTrabajandose/harvestDistribution/distributionAnalisis/data/cleanData_wayPointsCoffee_UTM.csv", stringsAsFactors = FALSE )

WP_ALTURAS <- WP_COSECHA_UTM_SP %>%
  group_by(ID_POR_FINCA, pante, finca) %>%
  summarise(MAX_ALT = max(altura..ft.), MIN_ALT = min(altura..ft.), DeltaMean = mean(delta))

WP_ALTURAS$DIF_ft <- WP_ALTURAS$MAX_ALT - WP_ALTURAS$MIN_ALT
WP_ALTURAS$DIF_m <- WP_ALTURAS$DIF_ft * 0.3048
WP_ALTURAS_E <- WP_ALTURAS %>% filter (finca=="Ecological")
WP_ALTURAS_C <- WP_ALTURAS %>% filter (finca=="Conventional")

#aunque fue por pante


mean(WP_ALTURAS_C$DIF_m)
sd(WP_ALTURAS_C$DIF_m)
mean(WP_ALTURAS_E$DIF_m)
sd(WP_ALTURAS_E$DIF_m)


###########ahora la distancia arboles, que seguro no se ve con la moda

mean(WP_ALTURAS_C$DeltaMean)
sd(WP_ALTURAS_C$DeltaMean)
mean(WP_ALTURAS_E$DeltaMean)
sd(WP_ALTURAS_E$DeltaMean)



