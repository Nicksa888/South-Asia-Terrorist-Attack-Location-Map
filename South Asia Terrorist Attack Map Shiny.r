####################
# Source Documents #
####################

source("C:/R Portfolio/South_Asia_Terrorism_Map_Shiny/Functions.R")
source("C:/R Portfolio/South_Asia_Terrorism_Map_Shiny/South_Asia_Data_Object_Locations.R")

# South Asia ---------------------------------------------
names(GTD_WD)
Region_Name <- "South Asia"
SA <- Region_Prep_geo(GTD_WD_geo, Region_Name)
glimpse(SA)

# Select Columns

SA_Select <- select(SA, c(Year, Dead, Group, Target, Attack, Weapon, Country, Province, City, Latitude, Longitude))

# Remove Outliers in Latitude and Longitude Columns

# Creating a Matrix of Coordinates:
data_matrix <- as.matrix(SA_Select[, c("Longitude", "Latitude")])
# This line extracts the 'Longitude' and 'Latitude' columns from a data frame SA_Select and converts these columns into a matrix called data_matrix. 
# This format is required for the subsequent Mahalanobis distance calculation.

# Calculate the Mahalanobis distance for each observation
mahalanobis_dist <- mahalanobis(data_matrix, colMeans(data_matrix), cov(data_matrix))
# The mahalanobis function calculates the Mahalanobis distance for each observation (row) in the data_matrix from the mean of the data. This distance 
# measures how far each point is from the center of the data distribution considering the covariance among the variables. The function takes three 
arguments:
# The data matrix of interest.
# The mean of each column (variable) in the matrix (colMeans(data_matrix)).
# The covariance matrix of the variables (cov(data_matrix)).

# Set a threshold for outlier detection
mahalanobis_threshold <- qchisq(0.975, df = 2)
# Here, a threshold is set using the chi-squared distribution (qchisq). This function returns the critical value for a chi-squared distribution where 97.5% 
# of the values fall below this threshold (assuming a 95% confidence level). The degrees of freedom (df = 2) corresponds to the number of variables 
(Longitude and Latitude).

# Identify bivariate outliers
bivariate_outliers <- which(mahalanobis_dist > mahalanobis_threshold)
# This line determines which observations have a Mahalanobis distance greater than the calculated threshold. These are marked as outliers. The which 
# function returns the indices (row numbers) of these outliers.

cat("Bivariate outliers - Row numbers:", ifelse(length(bivariate_outliers) > 0, toString(bivariate_outliers), "None"), "\n")
# This line prints the row numbers of the outliers. If there are no outliers (length is 0), it prints "None".

# Exclude outliers with "Sri Lanka" in the Country column
outliers_to_remove <- bivariate_outliers[!(SA_Select$Country[bivariate_outliers] == "Sri Lanka")]
# This line further filters the identified outliers by excluding those where the 'Country' column is "Sri Lanka". This suggests that for some reason, the 
# analysis specifically wants to retain data points from Sri Lanka, even if they are statistical outliers based on their geographic coordinates.
  
# Remove bivariate outliers from the original dataset
SA_Final <- SA_Select[-outliers_to_remove, ]

# Convert SA_Select_1 to an sf object
sf_object <- st_as_sf(SA_Select_1, coords = c("Longitude", "Latitude"))

# Assuming sf_object is your sf data as provided earlier

data <- sf_object
createTerroristAttackMapApp(data)



