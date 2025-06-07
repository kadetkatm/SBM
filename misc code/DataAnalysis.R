library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)

#read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

##subset data by coastal/inland
coastaldata <- data %>%
  filter(CoastalInland == "C") #subset for coastal

inlanddata <- data %>% 
  filter(CoastalInland == "I") #subset for inland

##compare by species by transect (C/I)
#summarize coastal data by genus 
coastal_genus <- coastaldata %>%
  group_by(Genus) %>%
  summarize(
    Count = n(),  # Count the number of occurrences of each species
    MeanLobeLengthC = mean(LobeLength, na.rm = TRUE),  # Mean LobeLength
    MeanLobeWidthC = mean(LobeWidth, na.rm = TRUE),    # Mean LobeWidth
    MeanLobeAreaC = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea
    MeanPatchAreaC = mean(PatchArea, na.rm = TRUE),    # Mean PatchArea
    MeanPatchVolumeC = mean(PatchVolume, na.rm = TRUE) # Mean PatchVolume
  )

#summarize inland data by genus 
inland_genus <- inlanddata %>% 
  group_by(Genus) %>%
  summarize(
    Count = n(),  # Count the number of occurrences of each species
    MeanLobeLengthI = mean(LobeLength, na.rm = TRUE),  # Mean LobeLength
    MeanLobeWidthI = mean(LobeWidth, na.rm = TRUE),    # Mean LobeWidth
    MeanLobeAreaI = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea
    MeanPatchAreaI = mean(PatchArea, na.rm = TRUE),    # Mean PatchArea
    MeanPatchVolumeI = mean(PatchVolume, na.rm = TRUE) # Mean PatchVolume
  )

#join and compare by genus 
comparison <- coastal_genus %>%
  left_join(inland_genus, by = "Genus") %>%
  # Optionally, you can calculate the differences between the means for coastal and inland
  mutate(
    DiffLobeLength = MeanLobeLengthC - MeanLobeLengthI,
    DiffLobeWidth = MeanLobeWidthC - MeanLobeWidthI,
    DiffLobeArea = MeanLobeAreaC - MeanLobeAreaI,
    DiffPatchArea = MeanPatchAreaC - MeanPatchAreaI,
    DiffPatchVolume = MeanPatchVolumeC - MeanPatchVolumeI
  )




# Reshape the data for plotting: Gather coastal and inland data into long format
comparison_long <- comparison %>%
  gather(key = "Environment", value = "Mean", 
         MeanLobeLengthC, MeanLobeLengthI, 
         MeanLobeWidthC, MeanLobeWidthI,
         MeanLobeAreaC, MeanLobeAreaI,
         MeanPatchAreaC, MeanPatchAreaI,
         MeanPatchVolumeC, MeanPatchVolumeI) %>%
  mutate(
    Metric = case_when(
      grepl("LobeLength", Environment) ~ "LobeLength",
      grepl("LobeWidth", Environment) ~ "LobeWidth",
      grepl("LobeArea", Environment) ~ "LobeArea",
      grepl("PatchArea", Environment) ~ "PatchArea",
      grepl("PatchVolume", Environment) ~ "PatchVolume"
    ),
    Environment = case_when(
      grepl("C", Environment) ~ "Coastal",
      grepl("I", Environment) ~ "Inland"
    )
  )
# Plot: Bar plot comparing means for coastal and inland by genus and metric
ggplot(comparison_long, aes(x = Genus, y = Mean, fill = Environment, group = Environment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +  # Separate plots for each metric (LobeLength, LobeWidth, etc.)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Comparison of Coastal vs Inland Means by Genus",
    x = "Genus",
    y = "Mean Value",
    fill = "Environment"
  ) +
  scale_fill_manual(values = c("Coastal" = "skyblue", "Inland" = "salmon"))

# Plot: Difference in Means for each metric
comparison_long_diff <- comparison %>%
  gather(key = "Environment", value = "Mean", 
         DiffLobeLength, DiffLobeWidth, DiffLobeArea, 
         DiffPatchArea, DiffPatchVolume)

ggplot(comparison_long_diff, aes(x = Genus, y = Mean, fill = Environment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Environment, scales = "free_y") +  # Plot the difference for each environment (Coastal vs Inland)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Difference in Mean Values Between Coastal and Inland",
    x = "Genus",
    y = "Difference in Mean",
    fill = "Environment"
  ) +
  scale_fill_manual(values = c("Coastal" = "skyblue", "Inland" = "salmon"))
