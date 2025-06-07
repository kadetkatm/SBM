
## % difference in mean LobeArea between coastal/inland transect 
# Load the dataset
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# Check for any missing values in LobeArea and CoastalInland
sum(is.na(data$LobeArea))  # Check for missing values in LobeArea
sum(is.na(data$CoastalInland))  # Check for missing values in CoastalInland

# Remove rows where LobeArea or CoastalInland is NA
data_clean <- data[!is.na(data$LobeArea) & !is.na(data$CoastalInland), ]

# Ensure CoastalInland values are correctly labeled as 'Coastal' or 'Inland'
unique(data_clean$CoastalInland)  # Check unique values in CoastalInland

# Calculate the mean LobeArea for coastal and inland transects
mean_lobearea_coastal <- mean(data_clean$LobeArea[data_clean$CoastalInland == "C"], na.rm = TRUE)
mean_lobearea_inland <- mean(data_clean$LobeArea[data_clean$CoastalInland == "I"], na.rm = TRUE)

# Calculate the percentage difference between the two means
percent_difference <- ((mean_lobearea_coastal - mean_lobearea_inland) / mean_lobearea_inland) * 100

# Print the results
cat("Mean LobeArea (Coastal):", mean_lobearea_coastal, "\n")
cat("Mean LobeArea (Inland):", mean_lobearea_inland, "\n")
cat("Percentage difference between Coastal and Inland LobeArea:", percent_difference, "%\n")


## % difference in total number of asexual/sexual structures between coastal/inland transect
# Load the dataset
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# Check for any missing values in Sexual, Asexual, and CoastalInland
sum(is.na(data$Sexual))  # Check for missing values in Sexual column
sum(is.na(data$Asexual))  # Check for missing values in Asexual column
sum(is.na(data$CoastalInland))  # Check for missing values in CoastalInland column

# Remove rows where CoastalInland or Sexual/Asexual columns are NA
data_clean <- data[!is.na(data$CoastalInland) & 
                     (!is.na(data$Sexual) | !is.na(data$Asexual)), ]

# Calculate total sexual and asexual structures for coastal transect (C) and inland transect (I)
total_sexual_asexual_coastal <- sum(data_clean$Sexual == "Y" & data_clean$CoastalInland == "C") +
  sum(data_clean$Asexual == "Y" & data_clean$CoastalInland == "C")

total_sexual_asexual_inland <- sum(data_clean$Sexual == "Y" & data_clean$CoastalInland == "I") +
  sum(data_clean$Asexual == "Y" & data_clean$CoastalInland == "I")

# Calculate the percentage difference between the total coastal and inland structures
percent_diff <- ((total_sexual_asexual_coastal - total_sexual_asexual_inland) / 
                   total_sexual_asexual_inland) * 100

# Print the results
cat("Total Combined Structures (Coastal):", total_sexual_asexual_coastal, "\n")
cat("Total Combined Structures (Inland):", total_sexual_asexual_inland, "\n")
cat("Percentage Difference (Coastal vs Inland):", percent_diff, "%", "\n")




# Load the dataset
data <- read.csv("cleandata/CleanSummitLoopSBM_noCrustose.csv")

# Check for any missing values in Sexual, Asexual, and CoastalInland
sum(is.na(data$Sexual))  # Check for missing values in Sexual column
sum(is.na(data$Asexual))  # Check for missing values in Asexual column
sum(is.na(data$CoastalInland))  # Check for missing values in CoastalInland column

# Remove rows where CoastalInland or Sexual/Asexual columns are NA
data_clean <- data[!is.na(data$CoastalInland) & 
                     (!is.na(data$Sexual) | !is.na(data$Asexual)), ]

# Calculate the total number of sexual structures by Coastal/Inland
total_sexual_coastal <- sum(data_clean$Sexual == "Y" & data_clean$CoastalInland == "C")
total_sexual_inland <- sum(data_clean$Sexual == "Y" & data_clean$CoastalInland == "I")

# Calculate the total number of asexual structures by Coastal/Inland
total_asexual_coastal <- sum(data_clean$Asexual == "Y" & data_clean$CoastalInland == "C")
total_asexual_inland <- sum(data_clean$Asexual == "Y" & data_clean$CoastalInland == "I")

# Print the results
cat("Total Sexual Structures (Coastal):", total_sexual_coastal, "\n")
cat("Total Sexual Structures (Inland):", total_sexual_inland, "\n")
cat("Total Asexual Structures (Coastal):", total_asexual_coastal, "\n")
cat("Total Asexual Structures (Inland):", total_asexual_inland, "\n")

