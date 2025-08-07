library(ggplot2)
library(dplyr)
setwd("C:/Users/kadet/OneDrive/Desktop/school/SFSU/MEEP/Lichen Research/R/SBMLichenTransect")
data <- read.csv("data/CoyoteBrushSurvey.csv")

# Add residuals to your data
data <- data %>%
  mutate(
    residuals = residuals(lm(HeightOfTallest.cm. ~ Elevation.ft., data = .))
  )

# Coastal side
coastal_data <- filter(data, Side.C.I. == "C")
coastal_lm <- lm(HeightOfTallest.cm. ~ Elevation.ft., data = coastal_data)
coastal_data$residuals <- residuals(coastal_lm)

# Inland side
inland_data <- filter(data, Side.C.I. == "I")
inland_lm <- lm(HeightOfTallest.cm. ~ Elevation.ft., data = inland_data)
inland_data$residuals <- residuals(inland_lm)

# Histogram of residuals
ggplot(coastal_data, aes(x = residuals)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 10) +
  labs(title = "Coastal Residuals", x = "Residual", y = "Frequency") +
  theme_minimal()

ggplot(inland_data, aes(x = residuals)) +
  geom_histogram(color = "black", fill = "salmon", bins = 10) +
  labs(title = "Inland Residuals", x = "Residual", y = "Frequency") +
  theme_minimal()

# Q-Q plot (visual test of normality)
qqnorm(coastal_data$residuals)
qqline(coastal_data$residuals, col = "blue")

qqnorm(inland_data$residuals)
qqline(inland_data$residuals, col = "red")

# shapiro-wilk test (statistical test of normality)
shapiro.test(coastal_data$residuals)
shapiro.test(inland_data$residuals)
