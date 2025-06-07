library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggpubr)
library(rstatix)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
# read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# Group by CoastalInland and get unique Genus names
unique_genera <- data %>%
  group_by(CoastalInland) %>%
  summarise(Genus_List = list(unique(Genus)))

# Print results
print(unique_genera)

# If you want a more readable format
unique_genera$Genus_List <- lapply(unique_genera$Genus_List, function(x) paste(x, collapse = ", "))
print(unique_genera)

# whole list 
expanded_genera <- unique_genera %>% unnest(cols = c(Genus_List))
print(expanded_genera)


install.packages("wordcloud")
install.packages("Rtools")
install.packages("tm")
install.packages("slam")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
library(tm)
library(slam)
# Extract genera and convert to a vector
genus_vector <- unlist(strsplit(paste(unique_genera$Genus_List, collapse=", "), ", "))

# Generate a word cloud
wordcloud(genus_vector, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

library(VennDiagram)

install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Create a formatted table
kable(unique_genera, caption = "Unique Genera by Coastal and Inland") %>%
  kable_styling(full_width = F)

