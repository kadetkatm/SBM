library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")

coyote <- read.csv("data/CoyoteBrushSurvey.csv")
lichen <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# Clean column names
colnames(coyote) <- make.names(colnames(coyote))
colnames(lichen) <- make.names(colnames(lichen))

# Step 1: Summarize Coyote Brush availability by Coastal/Inland
brush_summary <- coyote %>%
  group_by(Side.C.I.) %>%
  summarise(
    total_shrubs_over1m = sum(X.Over1m, na.rm = TRUE),
    mean_over1m_per_patch = mean(X.Over1m, na.rm = TRUE),
    patches = n()
  ) %>%
  rename(CoastalInland = Side.C.I.)

# Step 2: Summarize lichen reproduction by Coastal/Inland
lichen_summary <- lichen %>%
  group_by(CoastalInland) %>%
  summarise(
    n_total = n(),
    n_sexual = sum(Sexual == "Y", na.rm = TRUE),
    n_asexual = sum(Asexual == "Y", na.rm = TRUE),
    prop_sexual = n_sexual / n_total,
    prop_asexual = n_asexual / n_total
  )

# Step 3: Merge summaries
summary_combined <- merge(brush_summary, lichen_summary, by = "CoastalInland")

# Step 4: Plot proportion reproductive vs. shrub abundance

# One plot for sexual reproduction
ggplot(summary_combined, aes(x = total_shrubs_over1m, y = prop_sexual, label = CoastalInland)) +
  geom_point(size = 4, color = "blue") +
  geom_text(nudge_y = 0.02) +
  labs(
    title = "Lichen Sexual Reproduction vs Coyote Brush Availability",
    x = "Total Coyote Brush Individuals >1m",
    y = "Proportion of Lichen Patches with Sexual Structures"
  ) +
  theme_minimal()

# One plot for asexual reproduction
ggplot(summary_combined, aes(x = total_shrubs_over1m, y = prop_asexual, label = CoastalInland)) +
  geom_point(size = 4, color = "darkgreen") +
  geom_text(nudge_y = 0.02) +
  labs(
    title = "Lichen Asexual Reproduction vs Coyote Brush Availability",
    x = "Total Coyote Brush Individuals >1m",
    y = "Proportion of Lichen Patches with Asexual Structures"
  ) +
  theme_minimal()

# Plot both together
# Tidy format
summary_long <- summary_combined %>%
  select(CoastalInland, total_shrubs_over1m, prop_sexual, prop_asexual) %>%
  pivot_longer(cols = starts_with("prop"), names_to = "Reproduction", values_to = "Proportion") %>%
  mutate(Reproduction = recode(Reproduction,
                               prop_sexual = "Sexual",
                               prop_asexual = "Asexual"))

ggplot(summary_long, aes(x = total_shrubs_over1m, y = Proportion, color = Reproduction, label = CoastalInland)) +
  geom_point(size = 4) +
  geom_text(nudge_y = 0.02) +
  labs(
    title = "Lichen Reproduction vs Coyote Brush Availability",
    x = "Total Coyote Brush Individuals >1m",
    y = "Proportion of Lichen Patches"
  ) +
  theme_minimal()


