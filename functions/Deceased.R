#Använd tidigare kod för OFI för att få rätt kohort - finns i table1.R

# Convert DateTime_ArrivalAtHospital and DeceasedDate to Date format
ofi$DateTime_ArrivalAtHospital <- as.Date(ofi$DateTime_ArrivalAtHospital)
ofi$DeceasedDate <- as.Date(ofi$DeceasedDate)
# Calculate the difference in days
ofi$days_to_deceased <- as.numeric(ofi$DeceasedDate - ofi$DateTime_ArrivalAtHospital)
# Calculate the difference in days
ofi$days_to_deceased <- as.numeric(ofi$DeceasedDate - ofi$DateTime_ArrivalAtHospital)
# Count patients deceased within 1 day and 2 days
deceased_within_1_day <- sum(ofi$days_to_deceased == 1, na.rm = TRUE)
deceased_within_2_days <- sum(ofi$days_to_deceased <= 2 & ofi$days_to_deceased > 0, na.rm = TRUE)

# Print the results
cat("Patients deceased within 1 day:", deceased_within_1_day, "\n")
cat("Patients deceased within 2 days:", deceased_within_2_days, "\n")
