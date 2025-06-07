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
summary_branchtransect <- data %>%
  group_by(Genus, Branch) %>%  # Group by branch
  summarize(
    Count = n(),  # Count the number of lichens within each branch
    MeanLobeLength = mean(LobeLength, na.rm = TRUE),  # Mean LobeLength per branch
    MeanLobeWidth = mean(LobeWidth, na.rm = TRUE),    # Mean LobeWidth per branch
    MeanLobeArea = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea per branch
    MeanPatchArea = mean(PatchArea, na.rm = TRUE),    # Mean PatchArea per branch
    MeanPatchVolume = mean(PatchVolume, na.rm = TRUE) # Mean PatchVolume per branch
  ) 

# Reshape data to long format for plotting
data_long_branchtransect <- summary_branchtransect %>%
  gather(key = "Metric", value = "Value", 
         MeanLobeLength, 
         MeanLobeWidth,
         MeanLobeArea,
         MeanPatchArea,
         MeanPatchVolume) 


## FINAL PLOT
# Plotting the boxplot for each metric, comparing by branch
boxplot_branch <- ggplot(data_long_branchtransect, aes(x = as.factor(Branch), y = Value)) + 
  geom_boxplot(
    aes(fill = as.factor(Branch)), 
    size = 0.6, outlier.size = 1.0, outlier.shape = 21
  ) +
  facet_wrap(~Metric, scales = "free_y", ncol = 5) +
  theme_light(base_size = 14) +  # Increase base font size
  labs(
    title = "Distribution of Measurement Means by Branch", 
    subtitle = "Faceted by Metric, with Branches as Categories", 
    x = "Branch (1-20)", 
    y = "Measurement Size (mm)", 
    fill = "Branch Category"
  ) + 
  theme(
    legend.position = "none",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"), # Enhance facet titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

boxplot_branch

ggsave(
  filename = "images/boxplot_meangenusbybranch.png",
  plot = boxplot_branch,
  width = 18,  # Width in inches
  height = 8,  # Height in inches
  dpi = 300    # Resolution in dots per inch
)

# Boxplot faceted by Genus and Metric
boxplot_branch_facet_genus <- ggplot(data_long_branchtransect, aes(x = as.factor(Branch), y = Value)) + 
  geom_boxplot(
    aes(fill = as.factor(Branch)), 
    size = 0.6, outlier.size = 1.0, outlier.shape = 21
  ) +
  facet_grid(Genus ~ Metric, scales = "free_y") +
  theme_light(base_size = 14) + 
  labs(
    title = "Distribution of Measurement Means by Branch, Genus, and Metric", 
    subtitle = "Faceted by Genus and Metric", 
    x = "Branch (1-20)", 
    y = "Measurement Size (mm)", 
    fill = "Branch Category"
  ) + 
  theme(
    legend.position = "none",  # Remove legend to focus on facets
    strip.text = element_text(size = 12, face = "bold"), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold")
  )
boxplot_branch_facet_genus

# worth looking into more? 


# statistical analyses
# Perform ANOVA for each metric
anova_results <- data_long_branchtransect %>%
  group_by(Metric) %>%
  summarise(
    ANOVA = list(
      aov(Value ~ as.factor(Branch), data = pick(everything()))
    )
  )

# Extract summaries for ANOVA
anova_summaries <- anova_results %>%
  mutate(Summary = map(ANOVA, summary))

# Display results
anova_summaries$Summary

## Statistical significance by branch: MeanLobeWidth, MeanPatchArea, MeanPatchVolume
# Check assumptions for ANOVA on 'MeanLobeWidth', 'MeanPatchArea', and 'MeanPatchVolume' (as an example metric)
metric_to_check <- "MeanPatchVolume"  # Change to the metric you're analyzing

# Subset data for the selected metric
data_metric <- data_long_branchtransect %>% filter(Metric == metric_to_check)

# Perform ANOVA on the metric of interest
anova_model <- aov(Value ~ as.factor(Branch), data = data_metric)

# 1. Normality of residuals
# Q-Q plot
qqnorm(resid(anova_model))
qqline(resid(anova_model), col = "red")

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(resid(anova_model))

# 2. Homogeneity of variances: Levene's test
levene_test <- car::leveneTest(Value ~ as.factor(Branch), data = data_metric)

# 3. Independence of observations
# Residual vs. Fitted plot
plot(anova_model, which = 1)  # Residuals vs Fitted

# Print results
shapiro_test
levene_test

## because shapiro_test says normality is violated, run a non-parametric test
# Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal_test_results <- data_long_branchtransect %>%
  filter(Metric == metric_to_check) %>%
  kruskal.test(Value ~ as.factor(Branch))

kruskal_test_results # not accurate 

#### SIGNIFICANT DIFFERENCES BETWEEN BRANCHES bust residuals are not normally distributed
# Create a summary of Kruskal-Wallis test results for each metric
kruskal_summary <- data_long_branchtransect %>%
  group_by(Metric) %>%
  summarise(
    Kruskal_Wallis = list(kruskal.test(Value ~ as.factor(Branch))),
    .groups = 'drop'
  ) %>%
  mutate(
    Test_Statistic = sapply(Kruskal_Wallis, function(x) x$statistic),  # H value
    P_value = sapply(Kruskal_Wallis, function(x) x$p.value)            # p-value
  )

# View the Kruskal-Wallis summary table
kruskal_summary <- kruskal_summary %>%
  select(Metric, Test_Statistic, P_value)

# Display results
print(kruskal_summary)
### NO SIGNIFICANCE WHEN THEY ARE SEPARATED


# Try a log transformation
data_long_branchtransect$log_Value <- log(data_long_branchtransect$Value)

# Rerun the ANOVA on the transformed data
anova_model_log <- aov(log_Value ~ as.factor(Branch), data = data_long_branchtransect %>%
                         filter(Metric == metric_to_check))

# Check Shapiro-Wilk test again on the transformed residuals
shapiro_test_log <- shapiro.test(resid(anova_model_log))
shapiro_test_log

## DONE ANALYZING, NO SIGNIFICANCE 