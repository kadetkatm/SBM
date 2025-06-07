library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggpubr)
library(rstatix)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
# read in data
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# summarize data by genus, transect, and sexual (Y/N) count (for a summary table)
summarysexual <- data %>%
  filter(Sexual != "?") %>% #filter out ?
  group_by(Genus, CoastalInland, Sexual) %>% # group by genus and transect and presence of sexual
  summarize(
    Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Sexual, values_from = Count, #pivot sexual values into separate columns with counts for Y/N
              values_fill = list(Count = 0)) #fill NAs with 0

# Filter to keep only genera with non-zero counts for 'Y'
filtered_summarysexual <- summarysexual %>%
  filter(`Y` > 0)  # Keep only rows where Y > 0

# Filter the data to remove any rows where both 'Coastal' and 'Inland' are missing
filtered_summarysexual <- filtered_summarysexual %>%
  filter(CoastalInland %in% c("C", "I"))

# Plot the filtered data
ggplot(filtered_summarysexual, aes(x = Genus, y = `Y` / (`Y` + `N`), fill = CoastalInland)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Proportional bars for "Y"
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
                    labels = c("Coastal", "Inland")) +
  labs(title = "Proportion of Presence of Sexual Structures by Genus by Transect", 
       x = "Genus", 
       y = "Proportion of Presence", 
       fill = "Transect") +  # Proportion on y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# plot proportion of Y sexual without data filtered
ggplot(summarysexual, aes(x = Genus, y = `Y` / (`Y` + `N`), fill = CoastalInland)) + 
  geom_bar(stat = "identity", position = "dodge") +  # Proportional bars for "Y"
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
                    labels = c("Coastal", "Inland")) +
  labs(title = "Proportion of Presence of Sexual Structures by Genus by Transect", x = "Genus", y = "Proportion of Presence", fill = "Transect") +  # Proportion on y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# plot proportion of N sexual 
#ggplot(summarysexual, aes(x = Genus, y = `N` / (`Y` + `N`), fill = CoastalInland)) + 
 # geom_bar(stat = "identity", position = "dodge") +  # Proportional bars for "Y"
  #scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
  #                  labels = c("Coastal", "Inland")) +
  #labs(title = "Proportion of Absence of Sexual Structures by Genus by Transect", x = "Genus", y = "Proportion of Absence", fill = "Transect") +  # Proportion on y-axis
  #theme_minimal() +  # Clean theme
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# reshape and plot 
# Reshape your summary data to a long format
summarysexual_long <- summarysexual %>%
  pivot_longer(cols = c("Y", "N"), names_to = "Sexual", values_to = "Count") %>%
  filter(!Genus %in% c("Punctelia", "Melanelixia", "Parmelia"))  # Filter out the specified genera

## Potential FINAL PLOT 
# Create the bar plot with proportions
ggplot(summarysexual_long, aes(x = Genus, y = Count, fill = Sexual)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.7) +  # Adjust bar width for clarity
  facet_wrap(~CoastalInland, 
             labeller = labeller(CoastalInland = c("C" = "Coastal", "I" = "Inland"))) +  # Separate by Coastal/Inland
  labs(
    title = "Presence/Absence of Sexual Structures by Genus and Transect", 
    x = "Genus", 
    y = "Proportion", 
    fill = "Sexual"
  ) +  # Labels
  scale_y_continuous(labels = scales::percent) +  # Show percentages on the y-axis
  scale_fill_manual(values = c("Y" = "skyblue", "N" = "salmon"), 
                    labels = c("Y" = "Yes", "N" = "No")) +  # Color fill adjustments
  theme_minimal(base_size = 16) +  # Increase base font size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate and size x-axis labels
    axis.text.y = element_text(size = 14),  # Size y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Title styling
    legend.position = "top",  # Position the legend at the top for better visibility
    strip.text = element_text(size = 16, face = "bold")  # Size facet labels
  )

ggsave(
  filename = "images/sexual_structure_proportion_by_genus_and_transect.png",  # Set file path and name
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution (dots per inch)
)

## do the same thing but for asexual 
# summarize data by genus, transect, and asexual (Y/N) count (for a summary table)
summaryasexual <- data %>%
  filter(Asexual != "?") %>% #filter out ?
  mutate(Asexual = ifelse(Asexual == "M", "N", Asexual)) %>% #replace "M" with "N"
  group_by(Genus, CoastalInland, Asexual) %>% # group by genus and transect and presence of sexual
  summarize(
    Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Asexual, values_from = Count, #pivot sexual values into separate columns with counts for Y/N
              values_fill = list(Count = 0)) #fill NAs with 0

# Filter to keep only genera with non-zero counts for 'Y'
filtered_summaryasexual <- summaryasexual %>%
  filter(`Y` > 0)  # Keep only rows where Y > 0

# plot comparison of Y asexual 
ggplot(filtered_summaryasexual, aes(x = Genus, y = `Y` / (`Y` + `N`), fill = CoastalInland)) + 
  geom_bar(stat = "identity", position = "dodge") +  # Proportional bars for "Y"
  scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
                    labels = c("Coastal", "Inland")) +
  labs(title = "Proportion of Presence of Asexual Structures by Genus by Transect", x = "Genus", y = "Presence of Asexual Structures", fill = "Coastal/Inland") +  # Proportion on y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# plot comparison of N asexual 
#ggplot(summaryasexual, aes(x = Genus, y = `N` / (`Y` + `N`), fill = CoastalInland)) + 
  #geom_bar(stat = "identity", position = "dodge") +  # Proportional bars for "Y"
  #scale_fill_manual(values = c("C" = "skyblue", "I" = "salmon"), 
  #                  labels = c("Coastal", "Inland")) +
  #labs(title = "Proportion of Absence of Asexual Structures by Genus by Transect", x = "Genus", y = "Presence of Asexual Structures", fill = "Coastal/Inland") +  # Proportion on y-axis
  #theme_minimal() +  # Clean theme
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# reshape and plot 
# Reshape your summary data to a long format
summary_longasexual <- filtered_summaryasexual %>%
  filter(!Genus %in% c("Peltigera", "Collema")) %>%
  pivot_longer(cols = c("Y", "N"), names_to = "Asexual", values_to = "Count") 


### Potential FINAL PLOT
# Create the bar plot with proportions
ggplot(summary_longasexual, aes(x = Genus, y = Count, fill = Asexual)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.7) +  # Adjust bar width for clarity
  facet_wrap(~CoastalInland, 
             labeller = labeller(CoastalInland = c("C" = "Coastal", "I" = "Inland"))) +  # Separate by Coastal/Inland
  labs(
    title = "Presence/Absence of Asexual Structures by Genus and Transect", 
    x = "Genus", 
    y = "Proportion", 
    fill = "Asexual"
  ) +  # Labels
  scale_y_continuous(labels = scales::percent) +  # Show percentages on the y-axis
  scale_fill_manual(values = c("Y" = "skyblue", "N" = "salmon"), 
                    labels = c("Y" = "Yes", "N" = "No")) +  # Color fill adjustments
  theme_minimal(base_size = 16) +  # Increase base font size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate and size x-axis labels
    axis.text.y = element_text(size = 14),  # Size y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Title styling
    legend.position = "top",  # Position the legend at the top for better visibility
    strip.text = element_text(size = 16, face = "bold")  # Size facet labels
  )

ggsave(
  filename = "images/asexual_structure_proportion_by_genus_and_transect.png",  # Set file path and name
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution (dots per inch)
)

# compare by count (sexual/Asexual)
# create a new summary dataset that counts 'Y' from both Sexual and Asexual columns
summary_sexual_asexual <- data %>%
  filter(Sexual != "?" | Asexual != "?") %>%  # filter out "?" values
  mutate(Sexual = ifelse(Sexual == "Y", "Sexual", NA),  # Flag Sexual "Y" values
         Asexual = ifelse(Asexual == "Y", "Asexual", NA)) %>%  # Flag Asexual "Y" values
  pivot_longer(cols = c(Sexual, Asexual), 
               names_to = "Type", values_to = "Presence") %>%  # Combine Sexual and Asexual into one column
  filter(!is.na(Presence)) %>%  # Remove NA values
  filter(Genus != "Peltigera") %>% 
  group_by(Genus, CoastalInland, Type) %>%  # Group by Genus, Coastal/Inland, and Type
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of 'Y'

# plot the number of Sexual and Asexual structures by Genus and Transect
# Create the plot
ggplot(summary_sexual_asexual, aes(x = Genus, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Dodge position for side-by-side bars
  scale_fill_manual(values = c("Sexual" = "skyblue", "Asexual" = "salmon"), 
                    labels = c("Asexual", "Sexual")) +  # Color for Sexual and Asexual
  facet_wrap(~CoastalInland, 
             labeller = labeller(CoastalInland = c("C" = "Coastal", "I" = "Inland")), 
             scales = "free_y") +  # Facet by Coastal/Inland, with free y-axis for better scaling
  labs(
    title = "Comparison of Sexual and Asexual Structures by Genus and Transect", 
    x = "Genus", 
    y = "Count of Structures", 
    fill = "Structure Type"
  ) +  # Axis labels
  theme_minimal(base_size = 14) +  # Clean theme with larger base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase size of y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Bold and larger x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Bold and larger y-axis title
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered and bold plot title
    plot.subtitle = element_text(size = 14, hjust = 0.5)  # Subtitle formatting
  )

# Save the plot
ggsave(
  filename = "images/comparison_sexual_asexual_structures_by_genus.png",  # Set file path and name
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution (dots per inch)
)


## compare total presence of sexual/asexual by coastal/inland
# create new summary dataset for total sexual and asexual presence by Coastal/Inland transect
summary_sexual_asexual_total <- data %>%
  filter(Sexual != "?" | Asexual != "?") %>%  # Filter out "?" values
  mutate(Sexual = ifelse(Sexual == "Y", "Sexual", NA),  # Flag Sexual "Y" values
         Asexual = ifelse(Asexual == "Y", "Asexual", NA)) %>%  # Flag Asexual "Y" values
  pivot_longer(cols = c(Sexual, Asexual), 
               names_to = "Type", values_to = "Presence") %>%  # Combine Sexual and Asexual into one column
  filter(!is.na(Presence)) %>%  # Remove NA values
  group_by(CoastalInland, Type) %>%  # Group by Coastal/Inland and Type
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of 'Y'

# plot the total number of Sexual and Asexual presence by Coastal/Inland transect
# Create the plot
ggplot(summary_sexual_asexual_total, aes(x = CoastalInland, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Side-by-side bars
  scale_fill_manual(values = c("Sexual" = "skyblue", "Asexual" = "salmon"), 
                    labels = c("Asexual", "Sexual")) +  # Color for Sexual and Asexual
  labs(
    title = "Total Presence of Sexual and Asexual Structures by Coastal/Inland Transect", 
    x = "Transect", 
    y = "Total Presence Count", 
    fill = "Structure Type"
  ) +  # Axis labels
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +  # Modify x-axis labels
  theme_minimal(base_size = 14) +  # Clean theme with larger font size for presentation
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase size of y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Bold and larger x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Bold and larger y-axis title
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered and bold plot title
    plot.subtitle = element_text(size = 14, hjust = 0.5)  # Subtitle formatting
  )

# Save the plot as a PNG file
ggsave(
  filename = "images/total_sexual_asexual_structures_by_coastal_inland.png",  # File path and name
  plot = last_plot(),  # Use the last plot created
  width = 10,  # Width of the plot in inches
  height = 6,  # Height of the plot in inches
  dpi = 300    # Resolution (dots per inch)
)



## statistical analyses 
# Summarize the total presence of asexual structures by Coastal/Inland transect
summary_sexual_asexual_total <- data %>%
  filter(Asexual != "?") %>%  # Filter out "?" values
  mutate(Asexual = ifelse(Asexual == "Y", 1, 0)) %>%  # Convert presence to 1 and absence to 0
  group_by(CoastalInland, Asexual) %>%  # Group by Coastal/Inland and Asexual
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of presence/absence


# Chi-squared test for asexual presence across Coastal/Inland transects
# Create a contingency table
contingency_table <- table(summary_sexual_asexual_total$CoastalInland, summary_sexual_asexual_total$Asexual)

# Chi-squared test
chisq_test <- chisq.test(contingency_table)
chisq_test # p-value = 1 = no statistical significance 

# Summarize the presence of asexual structures by Genus and Coastal/Inland transect
summary_genus_asexual <- data %>%
  filter(Asexual != "?") %>%  # Filter out "?" values
  mutate(Asexual = ifelse(Asexual == "Y", 1, 0)) %>%  # Convert presence to 1 and absence to 0
  group_by(Genus, CoastalInland, Asexual) %>%  # Group by Genus, Coastal/Inland, and Asexual
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of presence/absence

# Perform Chi-squared tests by genus and transect
# Loop over each unique Coastal/Inland category (Coastal, Inland)
results <- summary_genus_asexual %>%
  group_by(CoastalInland) %>%
  do({
    genus_data <- .
    contingency_table_genus <- table(genus_data$Genus, genus_data$Asexual)
    test_result <- chisq.test(contingency_table_genus)
    data.frame(CoastalInland = unique(genus_data$CoastalInland),
               p_value = test_result$p.value)
  })

results

# logistic regression 
# Logistic regression comparing presence (1) vs. absence (0) of asexual structures by transect (Coastal vs. Inland)
data$Asexual_binary <- ifelse(data$Asexual == "Y", 1, 0)  # Convert to binary presence/absence

# Fit the logistic regression model
log_reg_model <- glm(Asexual_binary ~ CoastalInland, data = data, family = binomial)

# Summary of the model
summary(log_reg_model)

# Get odds ratios
exp(coef(log_reg_model))  # Exponentiated coefficients give you odds ratios


# two-proportion z-test
# Calculate proportions of presence (Y) for Coastal and Inland
coastal_presence <- sum(data$CoastalInland == "C" & data$Asexual == "Y") / sum(data$CoastalInland == "C")
inland_presence <- sum(data$CoastalInland == "I" & data$Asexual == "Y") / sum(data$CoastalInland == "I")

# Perform two-proportion z-test
prop.test(x = c(coastal_presence * sum(data$CoastalInland == "C"), inland_presence * sum(data$CoastalInland == "I")),
          n = c(sum(data$CoastalInland == "C"), sum(data$CoastalInland == "I")))


# Fisher's Exact Test for asexual presence/absence by Coastal/Inland transect
fisher_test <- fisher.test(contingency_table)
fisher_test


## repeat for sexual 
## statistical analyses 
# Summarize the total presence of sexual structures by Coastal/Inland transect
summary_sexual_total <- data %>%
  filter(Sexual != "?") %>%  # Filter out "?" values
  mutate(Sexual = ifelse(Sexual == "Y", 1, 0)) %>%  # Convert presence to 1 and absence to 0
  group_by(CoastalInland, Sexual) %>%  # Group by Coastal/Inland and Sexual
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of presence/absence


# Chi-squared test for sexual presence across Coastal/Inland transects
# Create a contingency table
contingency_table_sexual <- table(summary_sexual_total$CoastalInland, summary_sexual_total$Sexual)

# Chi-squared test
chisq_test_sexual <- chisq.test(contingency_table_sexual)
chisq_test_sexual # p-value = 1 = no statistical significance 

# Summarize the presence of sexual structures by Genus and Coastal/Inland transect
summary_genus_sexual <- data %>%
  filter(Sexual != "?") %>%  # Filter out "?" values
  mutate(Sexual = ifelse(Sexual == "Y", 1, 0)) %>%  # Convert presence to 1 and absence to 0
  group_by(Genus, CoastalInland, Sexual) %>%  # Group by Genus, Coastal/Inland, and Sexual
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of presence/absence

# Perform Chi-squared tests by genus and transect
# Loop over each unique Coastal/Inland category (Coastal, Inland)
results_sexual <- summary_genus_sexual %>%
  group_by(CoastalInland) %>%
  do({
    genus_data <- .
    contingency_table_genus_sexual <- table(genus_data$Genus, genus_data$Sexual)
    test_result <- chisq.test(contingency_table_genus_sexual)
    data.frame(CoastalInland = unique(genus_data$CoastalInland),
               p_value = test_result$p.value)
  })

results_sexual

# logistic regression 
# Logistic regression comparing presence (1) vs. absence (0) of sexual structures by transect (Coastal vs. Inland)
data$Sexual_binary <- ifelse(data$Sexual == "Y", 1, 0)  # Convert to binary presence/absence

# Fit the logistic regression model
log_reg_model_sexual <- glm(Sexual_binary ~ CoastalInland, data = data, family = binomial)

# Summary of the model
summary(log_reg_model_sexual)

# Get odds ratios
exp(coef(log_reg_model_sexual))  # Exponentiated coefficients give you odds ratios


# two-proportion z-test
# Calculate proportions of presence (Y) for Coastal and Inland
coastal_presence_sexual <- sum(data$CoastalInland == "C" & data$Sexual == "Y") / sum(data$CoastalInland == "C")
inland_presence_sexual <- sum(data$CoastalInland == "I" & data$Sexual == "Y") / sum(data$CoastalInland == "I")

# Perform two-proportion z-test
prop.test(x = c(coastal_presence_sexual * sum(data$CoastalInland == "C"), inland_presence_sexual * sum(data$CoastalInland == "I")),
          n = c(sum(data$CoastalInland == "C"), sum(data$CoastalInland == "I")))


# Fisher's Exact Test for sexual presence/absence by Coastal/Inland transect
fisher_test_sexual <- fisher.test(contingency_table_sexual)
fisher_test_sexual



## significance by number of sexual/asexual structures by genus by transect
library(dplyr)
library(tidyr)
library(ggplot2)

# Summarize the data for each genus and coastal/inland location
summary_sexual_asexual_by_genus <- data %>%
  filter(Sexual != "?" | Asexual != "?") %>%  # Filter out "?" values
  mutate(Sexual = ifelse(Sexual == "Y", "Sexual", NA),  # Flag Sexual "Y" values
         Asexual = ifelse(Asexual == "Y", "Asexual", NA)) %>%  # Flag Asexual "Y" values
  pivot_longer(cols = c(Sexual, Asexual), 
               names_to = "Type", values_to = "Presence") %>%  # Combine Sexual and Asexual into one column
  filter(!is.na(Presence)) %>%  # Remove NA values
  group_by(Genus, CoastalInland, Type) %>%  # Group by Genus, Coastal/Inland, and Type
  summarize(Count = n(), .groups = 'drop')  # Count occurrences of 'Y'

# Perform chi-squared test for each genus
genus_tests <- summary_sexual_asexual_by_genus %>%
  group_by(Genus) %>%
  do({
    test_data <- .  # Subset data for each genus
    test_result <- chisq.test(table(test_data$CoastalInland, test_data$Type))
    tibble(Genus = test_data$Genus[1], p_value = test_result$p.value)
  })

# View the results
genus_tests


## FINAL PLOT 
# Create a bar plot for each genus
ggplot(summary_sexual_asexual_by_genus, aes(x = CoastalInland, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust bar width for clarity
  facet_wrap(~ Genus, scales = "free_y") +  # Allow for different y-axis scales by genus
  labs(
    x = "Location (Coastal vs Inland)", 
    y = "Count of Structures", 
    title = "Sexual vs Asexual Structures by Genus and Location"
  ) +
  theme_minimal(base_size = 15) +  # Increase base font size for clarity
  scale_fill_manual(values = c("Sexual" = "skyblue", "Asexual" = "salmon")) +  # More distinct colors
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +  # Update x-axis labels
  theme(
    plot.margin = margin(10, 10, 10, 10),  # Adjust margins: top, right, bottom, left for balance
    strip.text = element_text(size = 16, face = "bold"),  # Increase facet title size for readability
    axis.title = element_text(size = 16, face = "bold"),  # Make axis titles larger and bold
    axis.text = element_text(size = 14),  # Increase axis text size
    legend.title = element_text(size = 16, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title with larger size and centered
    panel.grid = element_blank(),  # Remove panel gridlines for a cleaner look
    panel.border = element_rect(color = "black", size = 1, linetype = "solid", fill = NA)  # Add border around the plot
  )

# Save the plot as a high-quality PNG for presentation
ggsave(
  filename = "images/sexual_asexual_by_genus_location_presentation.png",  # Save path and filename
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches (larger for presentations)
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution of 300 DPI for high-quality presentations
)

# no significance but the plot is nice 

## NORMALIZE DATA for PRESENTATION 
# 1. Summarize the data by Genus, CoastalInland, and Type to calculate the total count for each group
summarized_data <- summary_sexual_asexual_by_genus %>%
  group_by(Genus, CoastalInland, Type) %>%
  summarise(
    total_count = sum(Count, na.rm = TRUE),  # Total count for each Type, Genus, and Coastal/Inland combination
    .groups = "drop"
  )

# 2. Calculate total individuals for each Genus and CoastalInland combination, separately for each Type (Sexual and Asexual)
total_individuals <- summarized_data %>%
  group_by(Genus, CoastalInland, Type) %>%
  summarise(
    total_individuals = sum(total_count, na.rm = TRUE),  # Total individuals across the two Types
    .groups = "drop"
  )

# 3. Merge the total_individuals data back to the summarized data for normalization
normalized_data <- summarized_data %>%
  left_join(total_individuals, by = c("Genus", "CoastalInland", "Type")) %>%
  mutate(
    normalized_count = total_count / total_individuals  # Normalize count by total individuals (per Type and Coastal/Inland)
  )

# 4. Reshape the data to long format for ggplot (though it's already in long format)
# In this case, the data is already in long format, so no further reshaping is needed.

# 5. Plotting the normalized data
ggplot(normalized_data, aes(x = CoastalInland, y = normalized_count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust bar width for clarity
  facet_wrap(~ Genus, scales = "free_y") +  # Allow for different y-axis scales by genus
  labs(
    x = "Location (Coastal vs Inland)", 
    y = "Normalized Count of Structures", 
    title = "Normalized Sexual vs Asexual Structures by Genus and Location"
  ) +
  theme_minimal(base_size = 15) +  # Increase base font size for clarity
  scale_fill_manual(values = c("Sexual" = "skyblue", "Asexual" = "salmon")) +  # More distinct colors
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +  # Update x-axis labels
  theme(
    plot.margin = margin(10, 10, 10, 10),  # Adjust margins
    strip.text = element_text(size = 16, face = "bold"),  # Increase facet title size for readability
    axis.title = element_text(size = 16, face = "bold"),  # Make axis titles larger and bold
    axis.text = element_text(size = 14),  # Increase axis text size
    legend.title = element_text(size = 16, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title with larger size and centered
    panel.grid = element_blank(),  # Remove gridlines for cleaner look
    panel.border = element_rect(color = "black", size = 1, linetype = "solid", fill = NA)  # Add plot border
  )

# Fill missing values for missing combinations (Sexual or Asexual for each Genus/CoastalInland)
# Define the desired order of the genera
genus_order <- c("Collema", "Pannaria", "Heterodermia", 
                 "Parmotrema", "Physcia", "Ramalina", 
                 "Sticta", "Teloschistes", "Usnea")

# Reorder the Genus column based on the specified order
normalized_data$Genus <- factor(normalized_data$Genus, levels = genus_order)

# Add missing combinations of Genus, CoastalInland, and Type
normalized_data <- normalized_data %>%
  complete(Genus, CoastalInland, Type, fill = list(normalized_count = 0))  # Add missing combinations

ggplot(normalized_data, aes(x = CoastalInland, y = normalized_count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjusted to "dodge" with no missing values
  facet_wrap(~ Genus, scales = "free_y") +  # Allow for different y-axis scales by genus
  labs(
    x = "Location (Coastal vs Inland)", 
    y = "Normalized Count of Structures", 
    title = "Normalized Sexual and Asexual Structures by Genus and Transect"
  ) +
  theme_minimal(base_size = 15) +  # Increase base font size for clarity
  scale_fill_manual(values = c("Sexual" = "thistle", "Asexual" = "mediumseagreen")) +  # More distinct colors
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +  # Update x-axis labels
  theme(
    plot.margin = margin(10, 10, 10, 10),  # Adjust margins
    strip.text = element_text(size = 16, face = "bold"),  # Increase facet title size for readability
    axis.title = element_text(size = 16, face = "bold"),  # Make axis titles larger and bold
    axis.text = element_text(size = 14),  # Increase axis text size
    legend.title = element_text(size = 16, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title with larger size and centered
    panel.grid = element_blank(),  # Remove gridlines for cleaner look
    panel.border = element_rect(color = "black", size = 1, linetype = "solid", fill = NA)  # Add plot border
  )

# Save the plot as a high-quality PNG for presentation
ggsave(
  filename = "images/sexual_asexual_by_genus_location_presentation.png",  # Save path and filename
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches (larger for presentations)
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution of 300 DPI for high-quality presentations
)


check_normalization <- normalized_data %>%
  group_by(Genus, CoastalInland) %>%
  summarize(total = sum(normalized_count), .groups = "drop")

print(check_normalization)  # Should all equal 1



# Step 1: Calculate total individuals by unique specimens for each Genus and Coastal/Inland group
total_individuals <- data %>%
  filter(Sexual != "?" | Asexual != "?") %>%  # Exclude uncertain data
  group_by(Genus, CoastalInland) %>%
  summarise(
    total_individuals = n_distinct(UniqueID),  # Count unique specimens
    .groups = "drop"
  )

# Step 2: Summarize the counts for Sexual and Asexual structures
summarized_data <- data %>%
  filter(Sexual == "Y" | Asexual == "Y") %>%  # Include only relevant rows
  pivot_longer(cols = c(Sexual, Asexual), names_to = "Type", values_to = "Presence") %>%
  filter(Presence == "Y") %>%  # Keep only rows where structures are present
  group_by(Genus, CoastalInland, Type) %>%
  summarise(
    Count = n_distinct(UniqueID),  # Count unique specimens for each type
    .groups = "drop"
  )

# Step 3: Merge the total individuals back to the summarized data
normalized_data <- summarized_data %>%
  left_join(total_individuals, by = c("Genus", "CoastalInland")) %>%
  mutate(
    normalized_count = Count / total_individuals  # Normalize by unique total specimens
  )

# Step 4: Fill missing combinations (if needed)
normalized_data <- normalized_data %>%
  complete(Genus, CoastalInland, Type, fill = list(normalized_count = 0, Count = 0))

# Step 5: Plot the normalized data
ggplot(normalized_data, aes(x = CoastalInland, y = normalized_count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Genus, scales = "free_y") +
  labs(
    x = "Location (Coastal vs Inland)",
    y = "Proportion of Structures (Normalized)",
    title = "Normalized Sexual vs Asexual Structures by Genus and Location"
  ) +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("Sexual" = "thistle", "Asexual" = "mediumseagreen")) +
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )



# Save the plot as a high-quality PNG for presentation
ggsave(
  filename = "images/sexual_asexual_by_genus_location_presentation.png",  # Save path and filename
  plot = last_plot(),  # Use the last plot created
  width = 12,  # Width of the plot in inches (larger for presentations)
  height = 8,  # Height of the plot in inches
  dpi = 300    # Resolution of 300 DPI for high-quality presentations
)


### PLOT FOR POSTER 

# Reorder Genus to control facet layout: 5 on top, 4 on bottom, right-aligned
genus_order <- c("Parmotrema", "Physcia", "Ramalina", "Collema", "Heterodermia",
                 "Sticta", "Teloschistes", "Usnea", "Pannaria")  # leaves 1 blank facet space

normalized_data$Genus <- factor(normalized_data$Genus, levels = genus_order)

# Fill missing combinations with 0
normalized_data <- normalized_data %>%
  complete(Genus, CoastalInland, Type, fill = list(normalized_count = 0))

# Create the plot
plot <- ggplot(normalized_data, aes(x = CoastalInland, y = normalized_count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Genus, scales = "free_y", nrow = 2) +
  labs(
    x = "Location (Coastal vs Inland)",
    y = "Proportion of Structures",
    title = "Sexual vs Asexual Structures by Genus and Location"
  ) +
  theme_minimal(base_size = 30) +
  scale_fill_manual(values = c("Sexual" = "thistle", "Asexual" = "mediumseagreen")) +
  scale_x_discrete(labels = c("C" = "Coastal", "I" = "Inland")) +
  theme(
    strip.text = element_text(size = 36, face = "bold"),
    axis.title = element_text(size = 36, face = "bold"),
    axis.text = element_text(size = 30),
    legend.title = element_text(size = 36, face = "bold"),
    legend.text = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    legend.position = "inside",
    legend.position.inside = c(0.97, 0.05),
    legend.justification = c("right", "bottom")
  )

# Save the plot as a high-res PNG
ggsave(
  filename = "images/final_sexual_asexual_by_genus_customlayout.png",
  plot = plot,
  width = 6715,
  height = 2257,
  units = "px",
  dpi = 300
)
