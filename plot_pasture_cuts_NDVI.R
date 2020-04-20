library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

NDVI_LAI_pasture_cuts <- read_csv("W:/VF/Eden_Valley/Pasture_CC_Survey_190704/extracted_pts_sampling_sites_NDVI_LAI.csv")
str(NDVI_LAI_pasture_cuts)

#Remove the na values


NDVI_LAI_pasture_cuts <- drop_na(NDVI_LAI_pasture_cuts, zone)
str(NDVI_LAI_pasture_cuts)


NDVI_LAI_pasture_cuts <- mutate(NDVI_LAI_pasture_cuts,
                                zone_date = paste0(zone , sample_dat))
                                  

NDVI_LAI_pasture_cuts <- mutate(NDVI_LAI_pasture_cuts,
                                zone_labels = case_when(
                                  zone_date == "GZ4/07/2019" ~ "Grazing zone",
                                  zone_date == "non_GZ4/07/2019" ~ "Non grazing zone",
                                  zone_date == "GZ16/05/2019" ~ "Grazing zone pre trial" ))

factor(NDVI_LAI_pasture_cuts$zone_labels)
NDVI_LAI_pasture_cuts$zone_labels <- ordered(NDVI_LAI_pasture_cuts$zone_labels, 
                                             levels = c("Grazing zone", 
                                                        "Non grazing zone",
                                                        "Grazing zone pre trial" ))


### pasture cut results on the day - after trial had eneded


Biomass <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, Plant_wt_T, colour = zone_labels))+
  geom_boxplot(alpha=0.1 )+
  scale_color_manual(values=c("red", "blue", "green"))+
  geom_point(colour = "black", alpha = 0.1) +
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   theme(legend.position = "none") +
  # ylim(0,1)+
   theme(axis.text=element_text(size=6),
         axis.title=element_text(size=8,))+
    labs(x = "",
         y= "Pasture measure - Plant t/ha")
         #,
         #title = "Sampling 04/07/2019 - end of trial")
Biomass

### crop circle results - after trial had eneded
NDVI <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, kriged_NDV, colour = zone_labels ))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(0,1)+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8,))+
  labs(x = "",
       y= "NDVI")
       #title = "Sampling 04/07/2019 - end of trial",
       #subtitle = "values extracted for sampling quadrats")
NDVI

### crop circle results - after trial had eneded
LAI <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, kriged_LAI, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(0,1)+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8,))+
  labs(x = "",
       y= "Leaf area index")
       #title = "Sampling 04/07/2019 - end of trial",
       #subtitle = "values extracted for sampling quadrats")
LAI
str(NDVI_LAI_pasture_cuts)

#regression of regression of NVDI vs LAI
LAI_NDVI_regression <- filter(NDVI_LAI_pasture_cuts, sample_dat == "4/07/2019") %>% 
ggplot( aes(kriged_LAI, kriged_NDV, colour = zone_labels))+
  geom_point(size = 3)+
  scale_color_manual(values=c("red", "blue", "green"))+
  theme_bw()+
  
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8,))+
  labs(x = "Leaf area index",
       y= "NDVI")
       #title = "Sampling 04/07/2019 - end of trial",
       #subtitle = "values extracted for sampling quadrats")

LAI_NDVI_regression
#regression of regression of LAI vs plant mass
LAI_regression <- filter(NDVI_LAI_pasture_cuts, sample_dat == "4/07/2019") %>% 
  ggplot( aes(kriged_LAI, Plant_wt_T, colour = zone_labels))+
  geom_point(size = 3)+
  scale_color_manual(values=c("red", "blue", "green"))+
  theme_bw()+
  
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8,))+
  labs(x = "Leaf area index",
       y= "Pasture measure - Plant t/ha")
       #title = "Sampling 04/07/2019 - end of trial",
       #subtitle = "values extracted for sampling quadrats")
LAI_regression
#regression of NDVI vs plant mass
NDVI_regression <- filter(NDVI_LAI_pasture_cuts, sample_dat == "4/07/2019") %>% 
  ggplot( aes(kriged_NDV, Plant_wt_T, colour = zone_labels))+
  geom_point(size = 3)+
  
  scale_color_manual(values=c("red", "blue", "green"))+
  theme_bw()+
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8,))+
  labs(x = "Normalised Difference Vegetative Index (NDVI)",
       y= "Pasture measure - Plant t/ha")
       #title = "Sampling 04/07/2019 - end of trial",
       #subtitle = "values extracted for sampling quadrats")
NDVI_regression



Biomass
LAI_NDVI_regression

LAI
NDVI

LAI_regression
NDVI_regression

graph_path <- file.path("W:", "VF", "Eden_Valley", "Pasture_cut_results")

biomass_crop_circle <- grid.arrange(Biomass, LAI_NDVI_regression,  
             LAI, NDVI, 
             LAI_regression, NDVI_regression, 
             nrow = 3)

ggsave(path= graph_path, filename = paste0("biomass_crop_circle.png"), device = "png", 
       width = 21, height = 15, units = "cm", biomass_crop_circle)
#########################################################################################################################



str(NDVI_LAI_pasture_cuts)
NDVI_LAI_pasture_cuts_post <- filter(NDVI_LAI_pasture_cuts, sample_dat == "4/07/2019") 
NDVI_LAI_pasture_cuts_pre <- filter(NDVI_LAI_pasture_cuts, sample_dat == "16/05/2019") 
  
  
NDF <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, ndf, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(0,100)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "Neutral Detergent Fibre")
#,
#title = "Sampling 04/07/2019 - end of trial")
NDF

adf <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, adf, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(0,100)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "Acid Detergent Fibre")
#,
#title = "Sampling 04/07/2019 - end of trial")
adf


dmdc <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, dmdc, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(0,100)+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 55, ymax = 65, 
           alpha = .2) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "DMDC")
#,
#title = "Sampling 04/07/2019 - end of trial")
dmdc


ompc <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, ompc, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  ylim(85,100)+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 55, ymax = 65, 
  #          alpha = .2) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  labs(x = "",
       y= "OMPC")
#,
#title = "Sampling 04/07/2019 - end of trial")
ompc


n <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, n, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  #ylim(90,100)+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 55, ymax = 65, 
  #          alpha = .2) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  labs(x = "",
       y= "N")
#,
#title = "Sampling 04/07/2019 - end of trial")
n
str(NDVI_LAI_pasture_cuts_post)




CP <- ggplot( NDVI_LAI_pasture_cuts, aes(zone_labels, `CP (crude protein %) n x 6.25`, colour = zone_labels))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "black", alpha = 0.1) +
  scale_color_manual(values=c("red", "blue", "green"))+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none") +
  #ylim(90,100)+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 55, ymax = 65, 
  #          alpha = .2) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  labs(x = "",
       y= "Crude protein")
#,
#title = "Sampling 04/07/2019 - end of trial")
CP



NDF
adf

dmdc
ompc

n
CP


biomass_lab_results <- grid.arrange(NDF, adf,  
             dmdc, ompc, 
             n, CP, 
             nrow = 3)

ggsave(path= graph_path, filename = paste0("biomass_lab_results.png"), device = "png", 
       width = 21, height = 15, units = "cm", biomass_lab_results)

##### multiple regressions

str(NDVI_LAI_pasture_cuts_post)
# Basic Scatterplot Matrix
pairs(~ ndf+adf+dmdc+ompc+n+`CP (crude protein %) n x 6.25`,
      data=NDVI_LAI_pasture_cuts_post,
      main="Simple Scatterplot Matrix")

pairs(~ kriged_NDV+Plant_wt_T+`CP (crude protein %) n x 6.25`,
      data=NDVI_LAI_pasture_cuts_post,
      main="Simple Scatterplot Matrix")

