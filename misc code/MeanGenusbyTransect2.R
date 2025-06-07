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
    MeanLobeArea = mean(LobeArea, na.rm = TRUE),      # Mean LobeArea of each species for each transect
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
         LobeArea) %>%
  mutate(Genus = factor(Genus, levels = rev(genus_order)))


# Plot: Box plot comparing means for coastal and inland by genus and metric
# perform statistical tests per genus, per metric
stat.test.Genus.Metric <- data_long %>%
  dplyr::filter(Metric %in% c("LobeArea")) %>%
  group_by(Metric, Genus) %>%
  filter(sum(CoastalInland == "C") >= 2 & sum(CoastalInland == "I") >= 2) %>%
  t_test(Value ~ CoastalInland) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

stat.test.Genus.Metric

# only lobearea, patcharea, patchvolume
#stat.test.Genus.Metric <- data_long %>%
# dplyr::filter(Metric %in% c("LobeArea", 
#                            "PatchArea", 
#                            "PatchVolume")) %>%
#group_by(Metric, Genus) %>%
#filter(sum(CoastalInland == "C") >= 2 & sum(CoastalInland == "I") >= 2) %>%
#t_test(Value ~ CoastalInland) %>%
#adjust_pvalue(method = "BH") %>%
#add_significance()

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
  dplyr::filter(Metric %in% c("LobeArea")) 



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

### Final plot Means by Genus by Transect, might want to remove PatchLength, LobeWidth, LobeLength
boxplot <- ggplot(boxplot_data %>% 
                    group_by(Genus, Metric)) + 
  geom_boxplot(
    aes(x = Genus, y = Value, fill = CoastalInland),
    size = 0.6, 
    outlier.size = 0.8,
    outlier.shape = 21
  ) +
  facet_grid(
    cols = vars(Metric), 
    scales = "free", 
    labeller = label_both
  ) +
  scale_fill_manual(
    values = c("C" = "skyblue", "I" = "salmon"), 
    labels = c("Coastal", "Inland")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = 'italic', size = 12),
    axis.text.x = element_text(size = 10),
    panel.grid.major.x = element_line(size = 0.5, linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  ) +
  coord_flip() + 
  labs(
    title = "Boxplot of Measurement Means by Genus and Transect (Coastal v. Inland)", 
    x = "Genus", 
    y = "Size (in mm)", 
    fill = "Transect"
  )

boxplot

# challenge: add the sig from stat.test.Genus.Metric to the plot
# look into stat_pvalue_manual or geom_text

?stat_pvalue_manual
?geom_text

boxplot + stat_pvalue_manual(
  stat.test.Genus.Metric, x = "Genus",
  label = "p.adj.signif", 
  position = position_dodge(1)) # my attempt 

boxplot_pvalues <- boxplot + 
  stat_pvalue_manual(
    stat.test.Genus.Metric, 
    x = "Genus", 
    label = "p.adj.signif", 
    y.position = stat.test.Genus.Metric$y.position, # Specify y.position column for placement
    label.size = 4, # Adjust font size of p-values
    tip.length = 0.01, # Shorten lines for a cleaner look
    vjust = -0.5 # Fine-tune vertical alignment
  )

boxplot_pvalues

ggsave(
  filename = "images/meangenustransect_boxplot_pvalues.png",  # PDF is ideal for presentations
  plot = boxplot_pvalues,           
  width = 16,                       # Adjust dimensions for slides
  height = 6,
  dpi = 300                         # High resolution
)

# plot again per metric to compare community as a whole 
# stats metric 
stat.test.Metric <- data_long %>%
  dplyr::filter(Metric == "LobeArea") %>%
  group_by(Metric) %>% 
  t_test(Value ~ CoastalInland) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test.Metric 

## AREA ONLY 
# plot again per metric to compare community as a whole 
# stats metric 
stat.test.MetricAREA <- data_long %>%
  dplyr::filter(Metric == "LobeArea") %>%
  group_by(Metric) %>% 
  t_test(Value ~ CoastalInland) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test.MetricAREA

# add y.position to place the p-value labels above the highest boxplot in each facet
stat.test.Metric <- stat.test.Metric %>%
  left_join(
    data_long %>%
      group_by(Metric) %>%
      summarize(max_value = max(Value, na.rm = TRUE), .groups = "drop"), 
    by = c("Metric")
  ) %>%
  mutate(y.position = max_value * 1.1) # Adjust slightly above the max value

## AREA ONLY
# add y.position to place the p-value labels above the highest boxplot in each facet 
stat.test.MetricAREA <- stat.test.MetricAREA %>%
  left_join(
    data_long %>%
      group_by(Metric) %>%
      summarize(max_value = max(Value, na.rm = TRUE), .groups = "drop"), 
    by = c("Metric")
  ) %>%
  mutate(y.position = max_value * 1.1) %>% 
  mutate(Metric = factor(Metric, levels = c("PatchArea", "LobeArea")))# Adjust slightly above the max value

# plot per metric
boxplot_all <- ggplot(boxplot_dataAREA %>% group_by(Metric)) +
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

## Final plot for presentation 
# Reorder Metric levels to arrange facets as desired
boxplot_data <- boxplot_data %>%
  mutate(
    Metric = factor(Metric, levels = c("LobeArea"
    ))
  )

## Filter out NA metric 
boxplot_data <- boxplot_data %>%
  filter(Metric %in% c("LobeArea"), !is.na(Metric))

# Reorder so that PatchArea is first
boxplot_data <- boxplot_data %>%
  mutate(Metric = factor(Metric, levels = c("LobeArea")))

boxplot_all <- ggplot(boxplot_data, aes(x = CoastalInland, y = Value, fill = CoastalInland)) +
  geom_boxplot(
    width = 0.8,   # Wider boxplots
    size = 1,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = CoastalInland),
    width = 0.08,  # Less horizontal spread
    size = 3.5,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_color_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +
  labs(
    title = "Lobe Area is Greater in Inland Transects than Coastal Ones",
    x = "Transect",
    y = expression("Size (in mm"^2*")")
  ) +
  theme_minimal(base_size = 30) +
  theme(
    strip.text = element_blank(),
    axis.title.x = element_text(size = 36, face = "bold"),
    axis.title.y = element_text(size = 36, face = "bold"),
    axis.text = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA)
  )

boxplot_all

# Save high-res image
ggsave(
  filename = "images/lobearea_transect_boxplot.png",
  plot = boxplot_all,
  width = 3649,
  height = 1645,
  units = "px",
  dpi = 300
)

boxplot_all <- ggplot(boxplot_data, aes(x = CoastalInland, y = Value, fill = CoastalInland)) +
  geom_boxplot(
    width = 0.8,   # Wider boxplots
    size = 1,
    outlier.shape = NA,
    coef = 1.5  # Default whisker length (can adjust this value as needed)
  ) +
  geom_jitter(
    aes(color = CoastalInland),
    width = 0.08,  # Less horizontal spread
    size = 3.5,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_color_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +
  labs(
    title = "Lobe Area is Greater in Inland Transects than Coastal Ones",
    x = "Transect",
    y = expression("Size (in mm"^2*")")
  ) +
  theme_minimal(base_size = 30) +
  theme(
    strip.text = element_blank(),
    axis.title.x = element_text(size = 36, face = "bold"),
    axis.title.y = element_text(size = 36, face = "bold"),
    axis.text = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA)
  )
boxplot_all


# Calculate the IQR and filter out outliers
boxplot_data_filtered <- boxplot_data %>%
  group_by(CoastalInland) %>%
  filter(
    Value >= (quantile(Value, 0.25) - 1.5 * IQR(Value)) &
      Value <= (quantile(Value, 0.75) + 1.5 * IQR(Value))
  ) %>%
  ungroup()

# Create the plot with the filtered data
boxplot_all_filtered <- ggplot(boxplot_data_filtered, aes(x = CoastalInland, y = Value, fill = CoastalInland)) +
  geom_boxplot(
    width = 0.8,   # Wider boxplots
    size = 1,
    outlier.shape = NA  # No outliers shown in the plot
  ) +
  geom_jitter(
    aes(color = CoastalInland),
    width = 0.08,  # Less horizontal spread
    size = 2.5,
    alpha = 0.2,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_color_manual(values = c("C" = "skyblue", "I" = "salmon")) +
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +
  labs(
    title = "Greater Lobe Area in Inland Transects",
    x = "Transect",
    y = expression("Size (in mm"^2*")")
  ) +
  theme_minimal(base_size = 30) +
  theme(
    strip.text = element_text(size = 36, face = "bold"),
    axis.title = element_text(size = 36, face = "bold"),
    axis.text = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    legend.position = "none",  # Remove the legend
    axis.text.x = element_text(size = 30, face = "bold")  # Bold x-axis labels
  )

# View the plot
boxplot_all_filtered

ggsave(
  filename = "C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect/images/meantransect_boxplot_pvalue.png",
  width = 3649,
  height = 1645,
  units = "px",
  dpi = 300
)
