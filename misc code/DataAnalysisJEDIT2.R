library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggpubr)
library(rstatix)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
# read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

## compare by species by transect (C/I)
# summarize data by genus and transect (for a summary table)
summary <- data %>%
  group_by(Genus, CoastalInland) %>% # group by genus and transect
  summarize(
    Count = n(),  # Count the number of occurrences of each species for each transect
    MeanLobeLength = mean(LobeLength, na.rm = TRUE),  # Mean LobeLength of each species for each transect
    MeanLobeWidth = mean(LobeWidth, na.rm = TRUE),    # Mean LobeWidth of each species for each transect
    MeanLobeArea = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea of each species for each transect
    MeanPatchArea = mean(PatchArea, na.rm = TRUE),    # Mean PatchArea of each species for each transect
    MeanPatchVolume = mean(PatchVolume, na.rm = TRUE) # Mean PatchVolume of each species for each transect
  ) 

# Reorder 'Genus' by MeanLobeArea across both Coastal and Inland
genus_order <- summary %>%
  group_by(Genus) %>%
  summarize(MeanLobeArea = mean(MeanLobeArea)) %>%
  arrange(desc(MeanLobeArea)) %>%
  pull(Genus)


# Reshape original data for plotting: Gather data into long format and 
data_long <- data %>%
  gather(key = "Metric", value = "Value", 
         LobeLength, 
         LobeWidth,
         LobeArea,
         PatchArea,
         PatchVolume,
         PatchLength,
         PatchWidth,
         PatchHeight) %>%
  mutate(Genus = factor(Genus, levels = rev(genus_order)))


# Plot: Box plot comparing means for coastal and inland by genus and metric
# perform statistical tests per genus, per metric
stat.test.Genus.Metric <- data_long %>%
  dplyr::filter(Metric %in% c("LobeLength",
                              "LobeWidth", 
                              "LobeArea", 
                              "PatchLength", 
                              "PatchArea", 
                              "PatchVolume")) %>%
  group_by(Metric, Genus) %>%
  filter(sum(CoastalInland == "C") >= 2 & sum(CoastalInland == "I") >= 2) %>%
  t_test(Value ~ CoastalInland) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

stat.test.Genus.Metric

# add y.position to place the p-value labels above the highest boxplot in each facet
stat.test.Genus.Metric <- stat.test.Genus.Metric %>%
  left_join(
    data_long %>%
      group_by(Metric, Genus) %>%
      summarize(max_value = max(Value, na.rm = TRUE), .groups = "drop"), 
    by = c("Metric", "Genus")
  ) %>%
  mutate(y.position = max_value * 1.1) # Adjust slightly above the max value

# plot per genus per metric
boxplot_data <- data_long %>%
  dplyr::filter(Metric %in% c("LobeLength",
                              "LobeWidth", 
                              "LobeArea", 
                              "PatchLength", 
                              "PatchArea", 
                              "PatchVolume")) 


# plot
boxplot <- ggplot(boxplot_data %>% 
    group_by(Genus, Metric)) + 
  geom_boxplot(aes(x = Genus, y = Value, fill = CoastalInland),
               size = 0.5, outlier.size = 0.5) +
  facet_grid(cols = vars(Metric), scales = "free") +
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
                    labels = c("Coastal", "Inland")) +
  theme_light() +
  theme(axis.text.y = element_text(face = 'italic')) +
  coord_flip() + 
  labs(
    title = "Boxplot of Measurement Means by Genus and Transect (Coastal v. Inland)", 
    x = "Genus", 
    y = "Size (in mm)", 
    fill = "Transect"
  ) + 
  theme(legend.position = "right")

boxplot

# challenge: add the sig from stat.test.Genus.Metric to the plot
# look into stat_pvalue_manual or geom_text

?stat_pvalue_manual
?geom_text

boxplot + stat_pvalue_manual(
  stat.test.Genus.Metric, x = "Genus",
  label = "p.adj.signif", 
  position = position_dodge(1)) # my attempt 


# plot again per metric to compare community as a whole 
# stats metric 
stat.test.Metric <- data_long %>%
  dplyr::filter(Metric == "LobeLength" |
                  Metric == "LobeWidth" |# pick and choose the metrics you want to plot here
                  Metric == "LobeArea" | 
                  Metric == "PatchLength" | 
                  Metric == "PatchArea" | 
                  Metric == "PatchVolume") %>%
  group_by(Metric) %>% 
  t_test(Value ~ CoastalInland) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test.Metric 

# add y.position to place the p-value labels above the highest boxplot in each facet
stat.test.Metric <- stat.test.Metric %>%
  left_join(
    data_long %>%
      group_by(Metric) %>%
      summarize(max_value = max(Value, na.rm = TRUE), .groups = "drop"), 
    by = c("Metric")
  ) %>%
  mutate(y.position = max_value * 1.1) # Adjust slightly above the max value


# plot per metric
boxplot_all <- ggplot(boxplot_data %>% group_by(Metric)) +
  geom_boxplot(aes(x = CoastalInland, y = Value, fill = CoastalInland),
               size = 0.5, outlier.size = 0.5) +
  scale_fill_manual(
    values = c("C" = "skyblue", "I" = "salmon"), 
    labels = c("Coastal", "Inland")) +
  facet_wrap(~Metric, scales = "free_y", ncol = 5) +
  theme_light() + 
  labs(
    title = "Boxplot of Measurement Means by Transect (Coastal v. Inland)",
    y = "Size (in mm)", 
    fill = "Transect"
  ) + 
  theme(legend.position = "right")
boxplot_all

# challenge: add the sig from stat.test.Metric to the plot
# look into stat_pvalue_manual or geom_text

boxplot_all + stat_pvalue_manual(
  stat.test.Metric,
  label = "p.adj.signif", 
  position = position_dodge(1)) # my attempt 

boxplot_all + stat_pvalue_manual(
  stat.test.Metric,
  label = "p.adj", 
  position = position_dodge(1)) # my attempt 

