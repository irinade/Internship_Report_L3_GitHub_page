## PCA sound parameters

# PREPARATION ########################################################################################################
# Libraries import -------------

library(devtools)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggbiplot)
library(dplyr)
library(data.table)



# Data import ----------------------------------------------------------

birds2 <- read.csv("~/Provini_Internship/L3_Internship/CodeR/1_Input/2_Analysing_Results/birdsNotComplete.csv", header = T, sep = ",", row.names = NULL)

### Choosing variables to plot for PCA
birds2 = subset(birds2, select = -c(MaxSylen, MinSylen, MeanSylen, SDSylen, 
                                    SyllableRate, SDDominantHarmonicFrequency, 
                                    SDHarmonicsIntensity, MeanHarmonicToNoiseRatio, 
                                    MaxHarmonicToNoiseRatio, MinHarmonicToNoiseRatio, 
                                    SDHarmonicToNoiseRatio, MeanFreqeuncyOfH2, 
                                    MeanFreqeuncyOfH3, MaxFreqeuncyOfH2, 
                                    MaxFreqeuncyOfH3, MinFreqeuncyOfH2, 
                                    MinFreqeuncyOfH3, SDFreqeuncyOfH2, 
                                    SDFreqeuncyOfH3, MeanEntropy, MaxEntropy, 
                                    MinEntropy, SDEntropy, MeanSlopeSpectro, 
                                    MaxSlopeSpectro, MinSlopeSpectro, SDSlopeSpectro) )

# Merging Data with bird informations ----------------------------------

birds.group <- read_csv("~/Provini_Internship/L3_Internship/CodeR/1_Input/2_Analysing_Results/BirdGroup.csv")



# Ordering dataframes --------------------------------------------------

### Ordering the birds.group dataframe using the birds2 dataframe
birds.group <- birds.group[ order(match(birds.group$`Scientific name`, birds2$Species)), ]

birds.group <- birds.group %>%
  slice(1:113)

### Other method
#intersect(birds.group$`Scientific name`, birds2$Species)
#setdiff(birds.group$`Scientific name`,birds2$Species)



# Plot ------------------------------------------------------------------

### Plot using species name
#rownames(birds2) <- birds2[,1]
#birds2[,1] <- NULL
#bird.pca <- prcomp(birds2[, which(apply(birds2, 2, var) != 0)], center = TRUE,scale. = TRUE)
#ggbiplot(bird.pca, labels=rownames(birds2))

bird.pca <- prcomp(birds2[, which(apply(birds2, 2, var) != 0)], center = TRUE,scale. = TRUE)


### Plotting by Clade++
ggbiplot(bird.pca, 
         ellipse=TRUE, circle = FALSE, 
         obs.scale=1, var.scale = 0.5, var.axes=TRUE,  
         labels=rownames(birds2), groups=birds.group$`Clade ++`)  + 
  ylim(-2.5,4) + xlim(-5.5,3)  +
  scale_colour_manual(name="Clades", values= c("dark orange", "brown1", "green", "cyan", "deepskyblue4"))+
  ggtitle("PCA of bird sound parameters grouping by Clades")+
  theme_minimal()+
  theme(legend.position = "bottom")

# We notice no clear distinction between the different bird clades, 
## however, the Telluraves seams to encompass all the other bird clades 
## in term of maximum pitch and loudness.
## Lets try plotting a PCA by narrowing bird clades!

### Plotting by Clade+
ggbiplot(bird.pca, 
         ellipse=TRUE, circle = FALSE, 
         obs.scale=1, var.scale = 0.5, var.axes=TRUE,  
         labels=rownames(birds2), groups=birds.group$`Clade +`)  + 
  ylim(-2.5,4) + xlim(-5.5,3)  +
  scale_colour_manual(name="Narrower Clades", values= c("cornflowerblue", "dark orange", "deepskyblue4", "coral", "green", "brown1", "cyan", "red"))+
  ggtitle("PCA of bird sound parameters grouping by subClades")+
  theme_minimal()+
  theme(legend.position = "bottom")

# The Australaves and Afroaves are both subclades from Telluraves. 
## We notice that its the Australaves subclade that encompass all the 
## others clades. If we look into this subclade, we realise that both
## passerines and passeriformes are in this subclade.
## Passerines and passeriformes contains some of the best mimicking 
## birds on the planets. Lets try to isolate those orders from the rest 
## of birds.

### Plotting by groups
ggbiplot(bird.pca,
         ellipse=TRUE, circle = FALSE, 
         obs.scale=1, var.scale = 0.5, var.axes=TRUE,  
         labels=rownames(birds2), groups=birds.group$Groups) + 
  ylim(-2.5,4) + xlim(-5.5,3)  +
  scale_colour_manual(name="Groups", values= c("red", "green", "darkorange", "blue"))+
  ggtitle("PCA of bird sound parameters grouping by passerines and parrots")+
  theme_minimal()+
  theme(legend.position = "bottom")


# Other stuff I tried

### Creating a column with body mass level
setDT(birds.group)[, BodyMassValueLevel := cut(BodyMassValue, 
                               quantile(BodyMassValue, c(0, .25, 0.5, .75, 1)), 
                               labels = c('XSmall', 'Small', 'Medium', 'Large'),
                               include.lowest = TRUE)]

### Plotting by body mass level 
ggbiplot(bird.pca,
         ellipse=TRUE, circle = FALSE, 
         obs.scale=1, var.scale = 0.5, var.axes=TRUE,  
         labels=rownames(birds2), groups=birds.group$BodyMassValueLevel) +
  ylim(-2.5,4) + xlim(-5.5,4)  +
  ggtitle("PCA of bird sound parameters grouping by body mass level")+
  theme_minimal()+
  labs(color="Body Mass categories")+
  theme(legend.position = "bottom")


### Plotting by Vision 
ggbiplot(bird.pca,
         ellipse=TRUE, circle = FALSE, 
         obs.scale=1, var.scale = 0.5, var.axes=TRUE,  
         labels=rownames(birds2), groups=birds.group$Vision) + 
  ylim(-2.5,4) + xlim(-5.5,3)  +
  scale_colour_manual(name="Vision type", values= c("skyblue3", "blue"))+
  ggtitle("PCA of bird sound parameters grouping by vision types")+
  theme_minimal()+
  theme(legend.position = "bottom")

