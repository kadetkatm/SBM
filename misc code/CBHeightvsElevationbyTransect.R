library(ggplot2)
library(dplyr)

setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
data <- read.csv("data/CoyoteBrushSurvey.csv")

data$Side.C.I. <- as.factor(data$Side.C.I.)
data$HeightOfTallest.cm. <- as.numeric(data$HeightOfTallest.cm.)
data$Elevation.ft. <- as.numeric(data$Elevation.ft.)

#linear regression by Coastal/Inland
ggplot(data, aes(x = Elevation.ft., y = HeightOfTallest.cm., color = Side.C.I.)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Elevation (ft)",
    y = "Height of Tallest Individual (cm)",
    color = "Side (C/I)",
    title = "Height vs. Elevation by Transect Side"
  ) +
  theme_minimal()

#regression models for each side
coastal_lm <- lm(HeightOfTallest.cm. ~ Elevation.ft., data = filter(data, Side.C.I. == "C"))
inland_lm <- lm(HeightOfTallest.cm. ~ Elevation.ft., data = filter(data, Side.C.I. == "I"))

summary(coastal_lm)
summary(inland_lm)


