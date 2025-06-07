library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggpubr)
library(rstatix)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
# read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# 1. Count the number of unique genera in total
unique_genera_total <- length(unique(data$Genus))
cat("Total number of unique genera:", unique_genera_total, "\n")

# 2. Count the number of unique genera separated by 'CoastalInland' values (C and I)
unique_genera_by_CI <- tapply(data$Genus, data$CoastalInland, function(genus) length(unique(genus)))

# Display results
cat("Number of unique genera in Coastal (C):", unique_genera_by_CI["C"], "\n")
cat("Number of unique genera in Inland (I):", unique_genera_by_CI["I"], "\n")



# Assuming each row represents an individual:
# 1. Count the total number of individuals (overall)
total_individuals <- nrow(data)
cat("Total number of individuals:", total_individuals, "\n")

# 2. Count the total number of individuals by 'CoastalInland' (C and I)
individuals_by_CI <- table(data$CoastalInland)

# Display results
cat("Total individuals in Coastal (C):", individuals_by_CI["C"], "\n")
cat("Total individuals in Inland (I):", individuals_by_CI["I"], "\n")


# 1. Count the total number of Sexual individuals with 'Y'
sexual_count <- sum(data$Sexual == "Y", na.rm = TRUE)
cat("Total number of Sexual individuals (Y):", sexual_count, "\n")

# 2. Count the total number of Asexual individuals with 'Y'
asexual_count <- sum(data$Asexual == "Y", na.rm = TRUE)
cat("Total number of Asexual individuals (Y):", asexual_count, "\n")

# 1. Filter data for Sexual = "Y"
sexual_data <- data[data$Sexual == "Y", ]

# 2. Count Sexual individuals by CoastalInland
sexual_by_CI <- table(sexual_data$CoastalInland)

# Display results
cat("Total Sexual individuals in Coastal (C):", sexual_by_CI["C"], "\n")
cat("Total Sexual individuals in Inland (I):", sexual_by_CI["I"], "\n")

# 1. Filter data for Asexual = "Y"
asexual_data <- data[data$Asexual == "Y", ]

# 2. Count Asexual individuals by CoastalInland
asexual_by_CI <- table(asexual_data$CoastalInland)

# Display results
cat("Total Asexual individuals in Coastal (C):", asexual_by_CI["C"], "\n")
cat("Total Asexual individuals in Inland (I):", asexual_by_CI["I"], "\n")
