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
  group_by(ID_REC, finca, pante) %>%
  summarise(MAX_ALT = max(altura..ft.), MIN_ALT = min(altura..ft.))

WP_ALTURAS$DIF_ft <- WP_ALTURAS$MAX_ALT - WP_ALTURAS$MIN_ALT
WP_ALTURAS$DIF_m <- WP_ALTURAS$DIF_ft * 0.3048
WP_ALTURAS_I <- WP_ALTURAS %>% filter (finca=="Irlanda")
WP_ALTURAS_H <- WP_ALTURAS %>% filter (finca=="Hamburgo")

#aunque fue por pante


mean(WP_ALTURAS_H$DIF_m)
sd(WP_ALTURAS_H$DIF_m)
mean(WP_ALTURAS_I$DIF_m)
sd(WP_ALTURAS_I$DIF_m)
