
library(dplyr)
library(ggplot2)

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

# Plot proportion sexual by site
ggplot(combined_summary, aes(x = Side.C.I., y = prop_sexual, fill = Side.C.I.)) +
  geom_col() +
  labs(
    title = "Proportion of Lichens with Sexual Structures",
    x = "Site (Coastal or Inland)",
    y = "Proportion Sexual"
  ) +
  theme_minimal()

# Plot proportion asexual by site
ggplot(combined_summary, aes(x = Side.C.I., y = prop_asexual, fill = Side.C.I.)) +
  geom_col() +
  labs(
    title = "Proportion of Lichens with Asexual Structures",
    x = "Site (Coastal or Inland)",
    y = "Proportion Asexual"
  ) +
  theme_minimal()

# Create a contingency table
contingency <- matrix(
  c(
    combined_summary$n_sexual[combined_summary$Side.C.I. == "C"],
    combined_summary$n_total[combined_summary$Side.C.I. == "C"] - combined_summary$n_sexual[combined_summary$Side.C.I. == "C"],
    combined_summary$n_sexual[combined_summary$Side.C.I. == "I"],
    combined_summary$n_total[combined_summary$Side.C.I. == "I"] - combined_summary$n_sexual[combined_summary$Side.C.I. == "I"]
  ),
  nrow = 2,
  byrow = TRUE
)

fisher.test(contingency)


#### Not sure this is showing me anything useful or if it actually integrates the CB data meaningfully 