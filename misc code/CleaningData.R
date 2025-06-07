library(ggplot2)
library(tidyverse)
library(dplyr)

#load data
data <- read.csv("data/SummitLoopSBM.csv")

##clean data 
cleandata <- data %>% 
  #remove columns Species, and weird X columns
  select(-Species, -X, -X.1, -X.2, -X.3) %>%
  # filter out NA values  
  filter(!is.na(LobeLength) & !is.na(LobeWidth) & !is.na(PatchLength) & !is.na(PatchWidth) & !is.na(PatchHeight)) %>%
  #create lobe and patch area and volume columns 
  mutate(
    LobeArea = LobeLength * LobeWidth,
    PatchArea = PatchLength * PatchWidth,
    PatchVolume = PatchLength * PatchWidth * PatchHeight
  )

#save clean data 
write.csv(cleandata, "cleandata/CleanSummitLoopSBM_noCrustose.csv", row.names = FALSE)

##subset data by coastal/inland
coastaldata <- cleandata %>%
  filter(CoastalInland == "C") #subset for coastal

inlanddata <- cleandata %>% 
  filter(CoastalInland == "I") #subset for inland

#save subset data 
write.csv(coastaldata, "cleandata/CoastalSummitLoopSBM_noCrustose.csv", row.names = FALSE)
write.csv(inlanddata, "cleandata/InlandSummitLoopSBM_noCrustose.csv", row.names = FALSE)
