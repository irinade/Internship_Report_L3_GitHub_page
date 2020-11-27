## Phylogenetic correlation sound production in birds 

# PREPARATION ########################################################################################################
# Libraries import -------------
library("ape")
library("ggplot2")
library("phytools")
library("tidytree")
library("here")
library("ggtree")
# packageVersion("ggtree") #‘2.5.0.991’

# Data import ----------------------------------------------------------

DFsound <- read.table(file = "~/Provini_Internship/L3_Internship/CodeR/1_Input/2_Analysing_Results/birdsNotComplete.csv", header = TRUE, sep=",")

row.names(DFsound) <- sub(pattern = " ", replacement = "_", x = DFsound$Species)
DFsound$label <- sub(pattern = " ", replacement = "_", x = DFsound$Species) #it is not useful in this precise case but in case someone changes the data it can be useful


# Trees import from birdtree.org  ----------------------------------------------------------

Trees <- read.nexus("~/Provini_Internship/L3_Internship/CodeR/1_Input/2_Analysing_Results/birdsNotComplete.output.nex")

# Here does not work on my computer, it do not locate the package
#Trees <- read.nexus(here("1_Input", "2_Analysing_Results", "output.nex"))  
  
# Consensus tree  ------------------------------------------------------------
tree <- consensus.edges(trees = Trees, method = "least.squares" ) # "RSS: 4635.59198947324"


# Merging Data with Tree --------------------------------------------
DFphylo <- full_join(as_tibble(tree), DFsound, by = 'label')
tree <-as.phylo(DFphylo)



# Tree Visualisation with ggtree --------------------------------
p <- ggtree(tree, layout='circular') %<+% DFphylo    

# Maximum frequency, pitch:
p + aes(color=MaxDominantHarmonicFrequency) + 
  geom_tiplab(size=2.5, offset = 2, 
              aes(angle=angle, color = MaxDominantHarmonicFrequency))+
  geom_tippoint(size=0.2, aes(color = MaxDominantHarmonicFrequency))+ 
  scale_color_gradientn(colours=c('red', 'blue'))+
  geom_hilight(node=155, fill="gold") + 
  geom_hilight(node=121, fill="purple") +
  geom_hilight(node=171, fill="green") +
  labs(color="Maximum Pitch") +
  theme(legend.position="right") +
  ggtitle("Phylogenetic tree of birds higlighting maximum pitch") +
  xlim(0,150)
  

# Maximum intensity, loudness:
p + aes(color=MaxHarmonicsIntensity) + 
  geom_tiplab(size=2.5, offset = 2, 
              aes(angle=angle, color = MaxHarmonicsIntensity))+
  geom_tippoint(size=0.2, aes(color = MaxHarmonicsIntensity)) + 
  scale_color_gradientn(colours=c('red', 'blue'))+
  geom_hilight(node=155, fill="gold") + 
  geom_hilight(node=121, fill="purple") +
  geom_hilight(node=171, fill="green") +
  labs(color="Maximum Loudness") +
  theme(legend.position="right")+
  ggtitle("Phylogenetic tree of birds higlighting maximum loudness") +
  xlim(0,150)


  


# Tree visualisation -------------------
## Fan
plot.phylo(x = tree, no.margin = TRUE,adj = 1, cex=0.6, font = 1, type = "fan", show.tip.label = TRUE , label.offset = 1 ) 

## Rightwards
plot(x = tree, no.margin = TRUE,adj = 0, cex=0.6, font = 1,  direction = "rightwards",show.tip.label = TRUE)
