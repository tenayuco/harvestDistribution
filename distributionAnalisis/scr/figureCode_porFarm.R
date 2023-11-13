
```{r echo=FALSE, message=FALSE}

DF_TOTAL_MIN <- read.csv("../output/DF_TOTAL_AIC_porFincas.csv")

DF_TOTAL_MIN$st2_par0 <- as.numeric(DF_TOTAL_MIN$st2_par0)
DF_TOTAL_MIN$st2_par1 <- as.numeric(DF_TOTAL_MIN$st2_par1)
#we filtrate state =2 beacuse the AIC was minimal, and also the gamma!

MIN_gamma_2 <- DF_TOTAL_MIN %>%
  filter(states == 2)%>%
  filter(model == "gamma")
#here we generate the distributions with the sobtained parameters 

##now!! it is important to recall that the program gives us the mean adn the sd fro garmma. To transform and pĺot we havve 
# shape α = µ2 /σ 2
# rate β = µ/σ 2
#scale λ = 1/β.

MIN_USADOS <- MIN_gamma_2

MIN_USADOS$shape_st1 <- (MIN_USADOS$st1_par0*MIN_USADOS$st1_par0)/(MIN_USADOS$st1_par1* MIN_USADOS$st1_par1)
MIN_USADOS$shape_st2 <- (MIN_USADOS$st2_par0*MIN_USADOS$st2_par0)/(MIN_USADOS$st2_par1* MIN_USADOS$st2_par1)

MIN_USADOS$scale_st1 <- (MIN_USADOS$st1_par1*MIN_USADOS$st1_par1)/(MIN_USADOS$st1_par1)
MIN_USADOS$scale_st2 <- (MIN_USADOS$st2_par1*MIN_USADOS$st2_par1)/(MIN_USADOS$st2_par1)


#CVer tabla para valores)
X= seq(1, 120, 1)

DF_gamma_C_st1 <- data.frame("distribution"= c("gamma"), "plantation" = "C", "state" = 1, "X"= X,  "PDF" = dgamma(X, shape = 4.9, rate = 1/1.56))

DF_gamma_C_st2 <- data.frame("distribution"= c("gamma"), "plantation" = "C", "state" = 2, "X"= X,  "PDF" = dgamma(X, shape= 2.5, rate = 1/6.41))

DF_gamma_E_st1 <- data.frame("distribution"= c("gamma"), "plantation" = "E","state" = 1, "X"= X,  "PDF" = dgamma(X, shape = 1.13, rate = 1/31))

DF_gamma_E_st2 <- data.frame("distribution"= c("gamma"), "plantation" = "E","state" = 2, "X"= X,  "PDF" = dgamma(X, shape= 3.2, rate = 1/2.8))



DF_DISTRI <- rbind(DF_gamma_C_st1, DF_gamma_C_st2, DF_gamma_E_st1, DF_gamma_E_st2)
DF_DISTRI_E <-DF_DISTRI  %>%
  filter(plantation == "E")
DF_DISTRI_C <- DF_DISTRI %>%
  filter(plantation == "C")

```

\newpage

```{r echo=FALSE, warning=FALSE, fig.cap= "Two state model distributions. The used parameters are shown in Table 4", fig.height= 5, fig.width=10, message=FALSE}

dataCosecha_C <- read.csv("../output/dataCosecha_PorFinca_C_.csv")
dataCosecha_E<- read.csv("../output/dataCosecha_PorFinca_E_.csv")

```


```{r echo=FALSE, warning=FALSE, fig.cap= "Two state model distributions. The used parameters are shown in Table 4", fig.height= 5, fig.width=10, message=FALSE}
CAJADIST_E<- dataCosecha_E %>%
  ggplot()+
  geom_boxplot(aes(x= step), fill= "#EEEEEE")+ 
  theme_bw()+
  theme(text = element_text(size = 20))+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlim(0, 120)+
  labs(x= "Step length (m)", y= "")


DIS_PLOT_E<- dataCosecha_E %>%
  ggplot() +
  geom_histogram(size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI_E, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       groupColors2)+
  facet_wrap(~plantation)+
  ylim(0, 0.32)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  labs(x= "Step length (m)", y= "Frequency", col= "State")


DIS_PLOT_COM_E <- DIS_PLOT_E + annotation_custom(ggplotGrob(CAJADIST_E), xmin = 15, xmax = 100, ymin = 0.1, ymax = 0.2)


```

```{r}
CAJADIST_C<- dataCosecha_C %>%
  ggplot()+
  geom_boxplot(aes(x= step), fill= "#EEEEEE")+ 
  theme_bw()+
  theme(text = element_text(size = 20))+ 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlim(0, 120)+
  labs(x= "Step length (m)", y= "")


DIS_PLOT_C<- dataCosecha_C %>%
  ggplot() +
  geom_histogram(size= 0.2, 
                 aes(x= as.numeric(step), 
                     y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth=1, color= "black", fill= "#EEEEEE") +
  geom_line(data = DF_DISTRI_C, size= 0.8, aes(x= X, y= PDF, col= as.factor(state)))+
  scale_color_manual(values = 
                       groupColors2)+
  facet_wrap(~plantation)+
  ylim(0, 0.32)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  labs(x= "Step length (m)", y= "Frequency", col= "State")


DIS_PLOT_COM_C <- DIS_PLOT_C + annotation_custom(ggplotGrob(CAJADIST_C), xmin = 15, xmax = 100, ymin = 0.1, ymax = 0.2)

```

```{r}
library(patchwork)
DIS_PLOT_TOTAL <- DIS_PLOT_COM_C + DIS_PLOT_COM_E


ggsave(DIS_PLOT_TOTAL, filename= "../output/finalFigures/histoGAMMA_porPlantacion.png", height = 6, width = 15, device = "png")

```
