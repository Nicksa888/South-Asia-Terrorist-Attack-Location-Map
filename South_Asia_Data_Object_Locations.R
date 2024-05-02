###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function for easier library loading
suppressPackageStartupMessages(
  libraries("data.table",
            "dplyr",
            "Boruta", # for Boruta variable selection algorithm
            "caret", 
            "purrr", # for reduce()
            "mltools", # for one_hot()
            "glmnet",
            "glmulti",
            "DescTools",
            "tidyr", # for crossing function
            "forcats", # for fct_lump_prop function
            "shiny",
            "leaflet",
            "sf",
            "leaflet.extras",
            "bnlearn"
            
  ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/R Portfolio/Global_Terrorism_Prediction")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0522dist.csv")
GTD1 <- read.csv("globalterrorismdb_2021Jan-June_1222dist.csv")

GTD_Initial <- rbind(GTD, GTD1)
GTD_WD <- GTD_Prep(GTD_Initial)
GTD_WD_geo <- GTD_Prep_geo(GTD_Initial)

##################
##################
# Country Counts #
##################
##################

t <- GTD_WD %>%
  dplyr::count(Country, sort = T)

factor_columns <- c("Group", 
                    "Target", 
                    "Attack", 
                    "Weapon", 
                    "Country",  
                    "Nationality",  
                    "Province", 
                    "City")

Intial_Columns_Remove <- c("Month", 
                           "Day", 
                           "Dead")

Corr_Columns_Remove <- c("Pakistan",
                         "Afghanistan_Nationality",
                         "OtherGroup", 
                         "India",
                         "BombAttack",
                         "Explosives", 
                         "Sindh_Province", 
                         "Karachi_City",
                         "OtherNationality", 
                         "OtherWeapon",
                         "Sri_Lanka_Nationality")

#########################
#########################
# Boruta Model Formulas #
#########################
#########################

###################
# Year Regression #
###################

SA_Boruta_Year_vector <- c("Lethal", 
                           "Taliban", 
                           "Business", 
                           "GovtGen", 
                           "OtherTarget", 
                           "Police", 
                           "Private", 
                           "ArmedAssaultAttack", 
                           "Assassination", 
                           "HostageKidnapAttack", 
                           "OtherAttack", 
                           "Firearms", 
                           "Afghanistan", 
                           "OtherCountry", 
                           "SriLanka", 
                           "India_Nationality", 
                           "Pakistan_Nationality", 
                           "Balochistan_Province", 
                           "Jammu_Kashmir_Province", 
                           "Khyber_Pakhtunkhwa_Province", 
                           "OtherProvince", 
                           "OtherCity")

# Convert the vector into a formula

SA_Boruta_Year_formula <- as.formula(paste(SA_Boruta_Year_vector[1],
                                     paste(SA_Boruta_Year_vector[-1],
                                     collapse = " + "),
                                     sep = " ~ "))


#########################
# Proportion Regression #
#########################

SA_Boruta_Proportion_vector <- c("Lethal",
                                 "Taliban", 
                                 "Business", 
                                 "GovtGen", 
                                 "OtherTarget", 
                                 "Police", 
                                 "Private", 
                                 "ArmedAssaultAttack", 
                                 "Assassination", 
                                 "HostageKidnapAttack", 
                                 "OtherAttack", 
                                 "Firearms", 
                                 "Afghanistan", 
                                 "OtherCountry", 
                                 "SriLanka", 
                                 "India_Nationality", 
                                 "Pakistan_Nationality", 
                                 "Balochistan_Province", 
                                 "Jammu_Kashmir_Province", 
                                 "Khyber_Pakhtunkhwa_Province", 
                                 "OtherProvince", 
                                 "OtherCity")

# Convert the vector into a formula

SA_Boruta_Proportion_formula <- as.formula(paste(SA_Boruta_Proportion_vector[1],
                                           paste(SA_Boruta_Proportion_vector[-1],
                                           collapse = " + "),
                                           sep = " ~ "))

########################
########################
# Lasso Model Formulas #
########################
########################

###################
# Year Regression #
###################

SA_Lasso_Year_vector <- c("Lethal", 
                          "Taliban",
                          "Business",                   
                          "GovtGen",
                          "Police",
                          "Private",                    
                          "Transportation",
                          "OtherTarget",
                          "ArmedAssaultAttack",         
                          "Assassination",
                          "HostageKidnapAttack",
                          "OtherAttack",                
                          "Firearms",
                          "Afghanistan",
                          "SriLanka",                   
                          "OtherCountry",
                          "India_Nationality",
                          "Pakistan_Nationality",       
                          "Jammu_Kashmir_Province",
                          "OtherProvince",              
                          "OtherCity")

# Convert the vector into a formula

SA_Lasso_Year_formula <- as.formula(paste(SA_Lasso_Year_vector[1],
                                    paste(SA_Lasso_Year_vector[-1],
                                    collapse = " + "),
                                    sep = " ~ "))


#########################
# Proportion Regression #
#########################

SA_Lasso_Proportion_vector <- c("Lethal",
                                "Taliban",
                                "Business",                   
                                "Police",
                                "Private",
                                "Transportation",             
                                "OtherTarget",
                                "ArmedAssaultAttack",
                                "Assassination",              
                                "HostageKidnapAttack",
                                "OtherAttack",
                                "Firearms",                   
                                "Afghanistan",
                                "SriLanka",
                                "OtherCountry",               
                                "India_Nationality",
                                "Pakistan_Nationality",
                                "Jammu_Kashmir_Province",     
                                "Khyber_Pakhtunkhwa_Province",
                                "OtherProvince",
                                "OtherCity")

# Convert the vector into a formula

SA_Lasso_Proportion_formula <- as.formula(paste(SA_Lasso_Proportion_vector[1],
                                          paste(SA_Lasso_Proportion_vector[-1],
                                          collapse = " + "),
                                          sep = " ~ "))

##########################
##########################
# Glmulti Model Formulas #
##########################
##########################

###################
# Year Regression #
###################

SA_Glmulti_Year_vector <- c("Lethal",
                            "Taliban",
                            "GovtGen", 
                            "Police", 
                            "Private",
                            "Transportation",
                            "ArmedAssaultAttack",
                            "Assassination", 
                            "HostageKidnapAttack",      
                            "OtherAttack",
                            "Firearms", 
                            "Afghanistan",
                            "OtherCountry",
                            "SriLanka",
                            "India_Nationality", 
                            "Jammu_Kashmir_Province", 
                            "OtherProvince",    
                            "OtherCity")

# Convert the vector into a formula

SA_Glmulti_Year_formula <- as.formula(paste(SA_Glmulti_Year_vector[1],
                                      paste(SA_Glmulti_Year_vector[-1],
                                      collapse = " + "),
                                      sep = " ~ "))


#########################
# Proportion Regression #
#########################

SA_Glmulti_Proportion_vector <- c("Lethal",
                                  "Taliban",
                                  "GovtGen",
                                  "Police",
                                  "Private",
                                  "Transportation",
                                  "ArmedAssaultAttack",
                                  "Assassination",
                                  "OtherAttack",
                                  "Firearms",   
                                  "Afghanistan",
                                  "OtherCountry",
                                  "SriLanka",
                                  "India_Nationality",    
                                  "Jammu_Kashmir_Province",
                                  "OtherProvince",
                                  "OtherCity")

# Convert the vector into a formula

SA_Glmulti_Proportion_formula <- as.formula(paste(SA_Glmulti_Proportion_vector[1],
                                            paste(SA_Glmulti_Proportion_vector[-1],
                                            collapse = " + "),
                                            sep = " ~ "))

SA_train_Year_Names <- c("Lethal",
                         "Taliban",
                         "GovtGen", 
                         "Police", 
                         "Private",
                         "Transportation",
                         "ArmedAssaultAttack",
                         "Assassination", 
                         "HostageKidnapAttack",      
                         "OtherAttack",
                         "Firearms", 
                         "Afghanistan",
                         "OtherCountry",
                         "SriLanka",
                         "India_Nationality", 
                         "Jammu_Kashmir_Province", 
                         "OtherProvince",    
                         "OtherCity") 

SA_x_year_names <- c("Taliban", 
                     "Business",
                     "GovtGen",                    
                     "Police",
                     "Private",
                     "Transportation",
                     "OtherTarget",
                     "ArmedAssaultAttack",
                     "Assassination", 
                     "HostageKidnapAttack",        
                     "OtherAttack",
                     "Firearms", 
                     "Afghanistan",
                     "SriLanka",                   
                     "OtherCountry", 
                     "India_Nationality",
                     "Pakistan_Nationality",
                     "Balochistan_Province",       
                     "Jammu_Kashmir_Province",
                     "Khyber_Pakhtunkhwa_Province",
                     "OtherProvince", 
                     "OtherCity")

SA_Train_Year_Names <- c()

SA_Test_Year_Names <- c("Lethal",  
                        "Taliban", 
                        "Business",
                        "GovtGen",                    
                        "Police",
                        "Private",
                        "Transportation",
                        "OtherTarget",
                        "ArmedAssaultAttack",
                        "Assassination", 
                        "HostageKidnapAttack",        
                        "OtherAttack",
                        "Firearms", 
                        "Afghanistan",
                        "SriLanka",                   
                        "OtherCountry", 
                        "India_Nationality",
                        "Pakistan_Nationality",
                        "Balochistan_Province",       
                        "Jammu_Kashmir_Province",
                        "Khyber_Pakhtunkhwa_Province",
                        "OtherProvince", 
                        "OtherCity")

SA_Train_Proportion_Names <- c("Lethal", 
                               "Taliban", 
                               "GovtGen",
                               "Police", 
                               "Private",
                               "Transportation",
                               "ArmedAssaultAttack", 
                               "Assassination",
                               "OtherAttack",
                               "Firearms",    
                               "Afghanistan",
                               "SriLanka",
                               "OtherCountry",
                               "India_Nationality",     
                               "Jammu_Kashmir_Province",
                               "OtherProvince", 
                               "OtherCity")

SA_Proportion_Test_Names <- c("Lethal",  
                      "Taliban", 
                      "Business",
                      "GovtGen",                    
                      "Police",
                      "Private",
                      "Transportation",
                      "OtherTarget",
                      "ArmedAssaultAttack",
                      "Assassination", 
                      "HostageKidnapAttack",        
                      "OtherAttack",
                      "Firearms", 
                      "Afghanistan",
                      "SriLanka",                   
                      "OtherCountry", 
                      "India_Nationality",
                      "Pakistan_Nationality",
                      "Balochistan_Province",       
                      "Jammu_Kashmir_Province",
                      "Khyber_Pakhtunkhwa_Province",
                      "OtherProvince", 
                      "OtherCity")

SA_train_Proportion_Names <- c("Lethal",
                               "Taliban",
                               "GovtGen",
                               "Police",
                               "Private",
                               "Transportation",
                               "ArmedAssaultAttack",
                               "Assassination",
                               "OtherAttack",
                               "Firearms",   
                               "Afghanistan",
                               "OtherCountry",
                               "SriLanka",
                               "India_Nationality",    
                               "Jammu_Kashmir_Province",
                               "OtherProvince",
                               "OtherCity")

Proportion_Test_Names <- c("Lethal",  
                           "Taliban", 
                           "Business",
                           "GovtGen",                    
                           "Police",
                           "Private",
                           "Transportation",
                           "OtherTarget",
                           "ArmedAssaultAttack",
                           "Assassination", 
                           "HostageKidnapAttack",        
                           "OtherAttack",
                           "Firearms", 
                           "Afghanistan",
                           "SriLanka",                   
                           "OtherCountry", 
                           "India_Nationality",
                           "Pakistan_Nationality",
                           "Balochistan_Province",       
                           "Jammu_Kashmir_Province",
                           "Khyber_Pakhtunkhwa_Province",
                           "OtherProvince", 
                           "OtherCity")