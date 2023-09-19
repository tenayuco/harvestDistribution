
```{r echo=FALSE, fig.cap= "Histogram of the steps lengths per farm. H: Hamburgo, I:Irlanda", warning=FALSE, fig.height= 4, fig.width=8}
pHisto_step <-dataCosecha %>%
separate(ID, into = c("Finca", "IDREC"), sep = "_", remove = FALSE)%>%
  ggplot(aes(x= as.numeric(step), y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  geom_histogram(binwidth=2, color= "black", aes(fill=as.character(Finca))) +
scale_fill_manual(values= mycols3c)+  
facet_wrap(~Finca)+
  theme(panel.spacing = unit(0.8, "lines"),
        text = element_text(size = 15))+
  theme_bw()+ 
   labs(x= "Distance (in m)", y= "Proportion", fill= "Farm")
pHisto_step
```

```{r echo=FALSE, fig.cap= "Radial histogram of the relative angles, per farm. H: Hamburgo, I: Irlanda",  warning=FALSE, fig.height= 4, fig.width=8}
pHisto_angle <-dataCosecha %>%
separate(ID, into = c("Finca", "IDREC"), sep = "_", remove = FALSE)%>%
  ggplot(aes(x= as.numeric(angle), y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  geom_histogram(binwidth=0.5, color= "black", aes(fill=as.character(Finca))) +
    coord_polar(theta="x", start=pi/2, direction=-1)+
scale_fill_manual(values= mycols3c)+  
facet_wrap(~Finca)+
  theme(panel.spacing = unit(0.8, "lines"),
        text = element_text(size = 15))+
  theme_bw()+ 
   labs(x= "Angle (in radians)", y= "Proportion", fill= "Farm")
pHisto_angle
```
