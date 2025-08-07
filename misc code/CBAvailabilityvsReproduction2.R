library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")

coyote <- read.csv("data/CoyoteBrushSurvey.csv")
lichen <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

colnames(coyote) <- make.names(colnames(coyote))
colnames(lichen) <- make.names(colnames(lichen))

# --- Aggregate Coyote Brush data by Coastal/Inland ---
coyote_summary <- coyote %>%
  group_by(Side.C.I.) %>%
  summarise(
    mean_height = mean(HeightOfTallest.cm., na.rm = TRUE),
    total_over1m = sum(X.Over1m, na.rm = TRUE),
    patches = n()
  )

# --- Aggregate Lichen reproductive data by Coastal/Inland ---
lichen_summary <- lichen %>%
  group_by(CoastalInland) %>%
  summarise(
    n_total = n(),
    n_sexual = sum(Sexual == "Y", na.rm = TRUE),
    n_asexual = sum(Asexual == "Y", na.rm = TRUE),
    prop_sexual = n_sexual / n_total,
    prop_asexual = n_asexual / n_total
  )

# --- Merge summaries by Coastal/Inland ---
combined_summary <- merge(
  coyote_summary,
  lichen_summary,
  by.x = "Side.C.I.",
  by.y = "CoastalInland"
)

# --- Tidy format ---
summary_long <- combined_summary %>%
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