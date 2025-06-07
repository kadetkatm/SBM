library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggpubr)
library(rstatix)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
# read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# summarize data by branch
summary_branch <- data %>%
  group_by(Branch) %>%  # Group by branch
  summarize(
    Count = n(),  # Count the number of lichens within each branch
    MeanLobeLength = mean(LobeLength, na.rm = TRUE),  # Mean LobeLength per branch
    MeanLobeWidth = mean(LobeWidth, na.rm = TRUE),    # Mean LobeWidth per branch
    MeanLobeArea = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea per branch
    MeanPatchArea = mean(PatchArea, na.rm = TRUE),    # Mean PatchArea per branch
    MeanPatchVolume = mean(PatchVolume, na.rm = TRUE) # Mean PatchVolume per branch
  ) 

# Reorder 'Branch' by MeanLobeArea 
#branch_order <- summary_branch %>%
 # group_by(Branch) %>%
  #summarize(MeanLobeArea = mean(MeanLobeArea)) %>%
  #arrange(desc(MeanLobeArea)) %>%
  #pull(Branch)


# Reshape data to long format for plotting
data_long_branch <- summary_branch %>%
  gather(key = "Metric", value = "Value", 
         MeanLobeLength, 
         MeanLobeWidth,
         MeanLobeArea,
         MeanPatchArea,
         MeanPatchVolume) # %>%
  #mutate(Branch = factor(Branch, levels = rev(branch_order)))


# Plotting the boxplot for each metric, comparing by branch
boxplot_branch <- ggplot(data_long_branch, aes(x = as.factor(Branch), y = Value)) + 
  geom_boxplot(aes(fill = as.factor(Branch)), 
               size = 0.5, outlier.size = 0.5) +
  facet_wrap(~Metric, scales = "free_y", ncol = 5) +
  theme_light() +
  labs(
    title = "Boxplot of Measurement Means by Branch", 
    x = "Branch", 
    y = "Size (in mm)", 
    fill = "Branch"
  ) + 
  theme(legend.position = "right")

boxplot_branch


##chatgpt gone wild 

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)

# set working directory and read in the data
setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# combine data by branch for each measurement
summary_branch <- data %>%
  group_by(Branch) %>%
  summarize(
    Count = n(),  # Number of samples in each branch
    MeanLobeLength = mean(LobeLength, na.rm = TRUE),  
    MeanLobeWidth = mean(LobeWidth, na.rm = TRUE),    
    MeanPatchLength = mean(PatchLength, na.rm = TRUE),
    MeanPatchWidth = mean(PatchWidth, na.rm = TRUE),  
    MeanPatchHeight = mean(PatchHeight, na.rm = TRUE), 
    MeanPatchVolume = mean(PatchVolume, na.rum = TRUE)
  )

# reshape the data into long format for easier plotting
data_long <- summary_branch %>%
  gather(key = "Metric", value = "Value", 
         MeanLobeLength, 
         MeanLobeWidth,
         MeanPatchLength,
         MeanPatchWidth,
         MeanPatchHeight, 
         MeanPatchVolume) 

# Check standard deviation of each measurement by branch
data %>%
  group_by(Branch) %>%
  summarize(
    sd_LobeLength = sd(LobeLength, na.rm = TRUE),
    sd_LobeWidth = sd(LobeWidth, na.rm = TRUE),
    sd_PatchLength = sd(PatchLength, na.rm = TRUE),
    sd_PatchWidth = sd(PatchWidth, na.rm = TRUE),
    sd_PatchHeight = sd(PatchHeight, na.rm = TRUE), 
    sd_PatchArea = sd(PatchHeight, na.rm = TRUE), 
    sd_PatchVolume = sd(PatchHeight, na.rm = TRUE), 
    sd_LobeArea = sd(PatchHeight, na.rm = TRUE)
  )

# Filter out branches with less than 2 observations for each metric
data_filtered <- data %>%
  group_by(Branch) %>%
  filter(n() > 1)  # Exclude branches with only one observation

# ANOVA with the filtered data
anova_results <- data_filtered %>%
  gather(key = "Metric", value = "Value", 
         LobeLength, LobeWidth, PatchLength, PatchWidth, PatchHeight) %>%
  group_by(Metric) %>%
  anova_test(Value ~ as.factor(Branch))

anova_results

# Use Kruskal-Wallis test instead of ANOVA for each metric
kruskal_results <- data_filtered %>%
  gather(key = "Metric", value = "Value", 
         LobeLength, LobeWidth, PatchLength, PatchWidth, PatchHeight) %>%
  group_by(Metric) %>%
  kruskal_test(Value ~ as.factor(Branch))

kruskal_results

# boxplot for each metric by branch
boxplot <- ggplot(data_long, aes(x = as.factor(Branch), y = Value, fill = as.factor(Branch))) +
  geom_boxplot() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Boxplot of Lichen Measurements by Branch",
       x = "Branch", y = "Measurement Value") +
  theme_minimal() +
  theme(legend.position = "none")

boxplot

# Improved boxplot for each metric by branch
boxplot2 <- ggplot(data_long, aes(x = as.factor(Branch), y = Value, fill = as.factor(Branch))) +
  geom_boxplot(outlier.size = 2, outlier.colour = "red", outlier.shape = 16) +  # Make outliers visible and distinguishable
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +  # Arrange into a 3-column layout
  labs(
    title = "Comparison of Lichen Measurements by Branch", 
    subtitle = "Boxplots for each measurement across branches",  # Add subtitle for clarity
    x = "Branch", 
    y = "Measurement Value (mm)",  # Clarify units
    fill = "Branch"
  ) +
  theme_minimal(base_size = 15) +  # Larger base size for better readability
  theme(
    legend.position = "none",  # Remove legend if branches are self-explanatory
    strip.text = element_text(size = 12, face = "bold"),  # Larger facet labels for better visibility
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase y-axis text size for clarity
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and center the title
    plot.subtitle = element_text(size = 12, hjust = 0.5)  # Subtitle in center
  ) +
  scale_fill_viridis_d()  # Use a color palette that is both accessible and visually appealing

boxplot2

# it looks like there is more significance of measurements in relation to branch (or elevation) 
# than in comparison by genus by transect (small sample sizes?)

