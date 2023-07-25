####################
# Source Documents #
####################

source("C:/R Portfolio/Global_Terrorism_Prediction/Functions 29 06 23.R")
source("C:/R Portfolio/Global_Terrorism_Prediction/South_Asia_Data_Object_Locations.R")

# South Asia ---------------------------------------------

Region_Name <- "South Asia"
SA <- Region_Prep(GTD_WD, Region_Name)
glimpse(SA)

# Select Columns

SA_Select <- select(SA, c(Year, Dead, Group, Target, Attack, Weapon, Country, Province, City, Latitude, Longitude))

# Remove Outliers in Latitude and Longitude Columns

# Combine Longitude and Latitude into a matrix
data_matrix <- as.matrix(SA_Select[, c("Longitude", "Latitude")])

# Calculate the Mahalanobis distance for each observation
mahalanobis_dist <- mahalanobis(data_matrix, colMeans(data_matrix), cov(data_matrix))

# Set a threshold for outlier detection (you can adjust this based on your data)
mahalanobis_threshold <- qchisq(0.975, df = 2)

# Identify bivariate outliers
bivariate_outliers <- which(mahalanobis_dist > mahalanobis_threshold)

cat("Bivariate outliers - Row numbers:", ifelse(length(bivariate_outliers) > 0, toString(bivariate_outliers), "None"), "\n")

# Ask the user if they want to remove the bivariate outliers
remove_bivariate_outliers <- readline(prompt = "Do you want to remove the bivariate outliers? (yes/no): ")

if (tolower(remove_bivariate_outliers) == "yes") {
  # Exclude outliers with "Sri Lanka" in the Country column
  outliers_to_remove <- bivariate_outliers[!(SA_Select$Country[bivariate_outliers] == "Sri Lanka")]
  
  # Remove bivariate outliers from the original dataset
  SA_Select_1 <- SA_Select[-outliers_to_remove, ]
  cat("Data with bivariate outliers removed.\n")
} else {
  cat("Data with bivariate outliers not removed.\n")
}

# Convert SA_Select_1 to an sf object
sf_object <- st_as_sf(SA_Select_1, coords = c("Longitude", "Latitude"))

# Assuming sf_object is your sf data as provided earlier

data <- sf_object
createTerroristAttackMapApp(data)
