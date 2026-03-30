attach(carPricePrediction)
DataSet <- carPricePrediction
#Dimension of original data set
dim(DataSet)
str(DataSet)
# Load necessary library
library(dplyr)

# Rename columns by replacing spaces with underscores
colnames(DataSet) <- gsub(" ", "_", colnames(DataSet))

# Check updated column names
colnames(DataSet) <- gsub("Prod._year", "Prod_year", colnames(DataSet))
colnames(DataSet)
###DUPLICATES###
# Identify duplicate rows
duplicates <- DataSet[duplicated(DataSet), ]
#find no of duplicates 
dim(duplicates)
#remove duplicates
DataSet <- DataSet[!duplicated(DataSet), ]
dim(DataSet)

#data types of variables
str(DataSet)
###MESSY VARIABLES###
#1. Levy
#Levy data type is "chr"
# Replace "-" with NA
DataSet$Levy[DataSet$Levy == "-"] <- NA
# Convert to numeric
DataSet$Levy <- as.numeric(DataSet$Levy)
# Check the structure to confirm the change
str(DataSet$Levy)
# Check for remaining missing values
sum(is.na(DataSet$Levy))

#2. Engine.volume
#install.packages("dplyr")
library(dplyr)
# Create a new Turbo indicator variable (1 if Turbo is present, 0 otherwise)
DataSet <- DataSet %>%
  mutate(Turbo = ifelse(grepl("Turbo", Engine_volume), 1, 0))
DataSet <- DataSet %>%
  relocate(Turbo, .after = Engine_volume)
# Remove "Turbo" text and convert Engine.volume to numeric
DataSet$Engine_volume<- as.numeric(gsub(" Turbo", "", DataSet$Engine_volume))
DataSet$Turbo <- as.character(DataSet$Turbo)

#3. Mileage
# Remove "km" text and convert Mileage to numeric
DataSet$Mileage <- as.numeric(gsub(" km", "", DataSet$Mileage))
#change the variable name(Mileage to Mileage(km))
colnames(DataSet)[colnames(DataSet) == "Mileage"] <- "Mileage_km"

#4. Doors
# Replace incorrect values with correct ones
DataSet$Doors[DataSet$Doors == "04-May"] <- "4"
DataSet$Doors[DataSet$Doors == "02-Mar"] <- "2"
# Handling ">5" (You can decide what to do with it, for now, setting it to NA)
DataSet$Doors[DataSet$Doors == ">5"] <- "5"
# Convert Doors back to numeric
DataSet$Doors <- as.numeric(DataSet$Doors)

#5. Model
# Select only categorical columns (character or factor)
#categorical_vars <- DataSet[, sapply(DataSet, function(x) is.character(x) | is.factor(x))]

# View the selected columns
#head(categorical_vars)
#lapply(categorical_vars, unique)

# Define a threshold for "popular models" (e.g., models with more than 50 occurrences)
#popular_models <- names(which(table(DataSet$Model) > 50))
# Replace rare models with "Other"
#DataSet$Model <- ifelse(DataSet$Model %in% popular_models, DataSet$Model, "Other")
#DataSet$Manufacturer_Model <- paste(DataSet$Manufacturer, DataSet$Model, sep = "_")
#DataSet <- DataSet %>%
 # relocate(Manufacturer_Model, .after =Manufacturer )
#DataSet$Manufacturer_Model

#data types
str(DataSet)

###MISSING VALUES###
colSums(is.na(DataSet))
#Levy - 5709
#proportion of missing values
no_rows <- nrow(DataSet)
MissingProp <- (5709 / no_rows) * 100
MissingProp
# 30.16804% of data are missing therefore we can remove from data set
# Load the dplyr package
install.packages("dplyr")
library(dplyr)
# Verify the changes
str(DataSet)
DataSet <- DataSet%>% select(-Levy)
str(DataSet)

#Remove outliers from Isolation forest
# Load necessary library
#install.packages("isotree")
library(isotree)

# Train Isolation Forest model
iso_model <- isolation.forest(DataSet, ntrees = 100, sample_size = 256, ndim = ncol(DataSet), seed = 42)

# Predict anomaly scores
anomaly_scores <- predict(iso_model, DataSet)

# Add anomaly scores to the dataset
DataSet$anomaly_score <- anomaly_scores

# Classify outliers (e.g., using a threshold)
threshold <- 0.5  # Adjust based on your dataset
DataSet$is_outlier <- ifelse(DataSet$anomaly_score > threshold, TRUE, FALSE)
dim(DataSet)
sum(DataSet$is_outlier==TRUE)
 #Remove outliers
cleaned_data <- DataSet %>% filter(is_outlier == FALSE)
dim(cleaned_data)

#cleaned_data = DataSet
# Example: Check anomaly scores
hist(anomaly_scores, breaks = 50, main = "Anomaly Scores Distribution")
#Distribution of Price
#install.packages("ggplot2")
library(ggplot2)
ggplot(cleaned_data, aes(x = Price)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### 1. Mileage
#Histogram
ggplot(cleaned_data, aes(x = Mileage_km)) + 
  geom_histogram(fill = "steelblue", color = "black", bins = 50) + 
  labs(title = "Distribution of Mileage", x = "Mileage(km)", y = "Frequency") + 
  theme_minimal()
##########################################
# Load necessary libraries
library(ggplot2)
library(MASS)  # For Box-Cox transformation

# Check the original distribution of Mileage_km
ggplot(cleaned_data, aes(x = Mileage_km)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  labs(title = "Original Distribution of Mileage_km", x = "Mileage (Km)", y = "Frequency")

# 2. Square Root Transformation
cleaned_data$Mileage_sqrt <- sqrt(cleaned_data$Mileage_km)

# Plot histogram after square root transformation
ggplot(cleaned_data, aes(x = Mileage_sqrt)) +
  geom_histogram(fill = "#8b0000", color = "black", bins = 50) +
  labs(title = "Square Root Transformation of Mileage_km", x = "Mileage_sqrt", y = "Frequency")


#########################################

#log transformation for price
cleaned_data$LogPrice <- log1p(cleaned_data$Price)  # log1p() handles zero values
cleaned_data$LogPrice <- log1p(cleaned_data$Price)
library(ggplot2)
ggplot(cleaned_data, aes(x = LogPrice)) + 
  geom_histogram(fill = "#8b0000", color = "black", bins = 50) + 
  labs(title = "Log-Transformed Price Distribution", x = "LogPrice", y = "Frequency") + 
  theme_minimal()

#log transformation for price
cleaned_data$LogMileage <- log1p(cleaned_data$Mileage_km)  # log1p() handles zero values
cleaned_data$LogMileage <- log1p(cleaned_data$Mileage_km)
library(ggplot2)
ggplot(cleaned_data, aes(x = LogMileage)) + 
  geom_histogram(fill = "#8b0000", color = "black", bins = 50) + 
  labs(title = "Log-Transformed mileage Distribution", x = "LogMileage", y = "Frequency") + 
  theme_minimal()
# 2. Square Root Transformation
cleaned_data$price_sqrt <- sqrt(cleaned_data$Price)

# Plot histogram after square root transformation
ggplot(cleaned_data, aes(x = price_sqrt)) +
  geom_histogram(fill = "#8b0000", color = "black", bins = 50) +
  labs(title = "Square Root Transformation of Price", x = "Price_sqrt", y = "Frequency")







cleaned_data$Turbo <- factor(cleaned_data$Turbo,levels = c(0, 1), labels = c("0","1"))
cleaned_data$Cylinders <- factor(cleaned_data$Cylinders)
cleaned_data$Doors <- factor(cleaned_data$Doors)
cleaned_data$Airbags <- factor(cleaned_data$Airbags)



# Step 1: Define the threshold for high mileage (e.g., 75th percentile)
high_mileage_threshold <- quantile(cleaned_data$Engine_volume, probs = 0.5, na.rm = TRUE)
cat("High Mileage Threshold (75th percentile):", high_mileage_threshold, "\n")

# Step 2: Filter the dataset to include only high mileage cars
high_mileage_cars <- cleaned_data %>% filter(Engine_volume < 0.2)

# Step 3: Group by Model and calculate average mileage for each model
high_mileage_models <- high_mileage_cars %>%
  group_by(Model,Manufacturer,Turbo,Price) %>%
  summarise(
    Average_Mileage = mean(Engine_volume, na.rm = TRUE),
    Count = n()  # Number of cars in each model
  ) %>%
  arrange(desc(Average_Mileage))  # Sort by average mileage in descending order

# Step 4: Print the high mileage models
print(high_mileage_models)

# Filter the dataset for Engine_volume greater than 6
high_engine_volume_data <- cleaned_data %>% filter(Engine_volume < 5 & Engine_volume > 4)

# Extract unique manufacturers
unique_manufacturers <- unique(high_engine_volume_data$Manufacturer)

# Print the unique manufacturers
print(unique_manufacturers)

# Calculate the frequency of each manufacturer
manufacturer_counts <- table(cleaned_data$Manufacturer)
# Convert to a data frame for easier manipulation
manufacturer_freq <- as.data.frame(manufacturer_counts)
colnames(manufacturer_freq) <- c("Manufacturer", "Frequency")

# Calculate the percentage of each category
total_observations <- sum(manufacturer_freq$Frequency)
manufacturer_freq$Percentage <- manufacturer_freq$Frequency / total_observations * 100

# View the frequency table
print(manufacturer_freq)


# Define the threshold (e.g., 5%)
threshold <- 5

# Identify low-frequency categories
low_freq_manufacturers <- manufacturer_freq$Manufacturer[manufacturer_freq$Percentage < threshold]

# View low-frequency categories
print(low_freq_manufacturers)
# Group low-frequency categories into "Other"
cleaned_data$Manufacturer <- ifelse(cleaned_data$Manufacturer %in% low_freq_manufacturers, "Other", cleaned_data$Manufacturer)

# Check the updated frequency of manufacturers
updated_manufacturer_counts <- table(cleaned_data$Manufacturer)
print(updated_manufacturer_counts)
#############
# Calculate the frequency of each model
model_counts <- table(cleaned_data$Model)

# Convert to a data frame for easier manipulation
model_freq <- as.data.frame(model_counts)
colnames(model_freq) <- c("Model", "Frequency")

# Calculate the percentage of each category
total_observations <- sum(model_freq$Frequency)
model_freq$Percentage <- model_freq$Frequency / total_observations * 100

# View the frequency table
print(model_freq)
# Order by Percentage (descending)
model_freq_ordered <- model_freq %>% arrange(desc(Percentage))

# Print the ordered frequency table
print(model_freq_ordered)


# Define the threshold (e.g., 5%)
threshold <- 0.5

# Identify low-frequency categories
low_freq_model <- model_freq$Model[model_freq$Percentage < threshold]

# View low-frequency categories
print(low_freq_model)
# Group low-frequency categories into "Other"
cleaned_data$Model <- ifelse(cleaned_data$Model %in% low_freq_model, "Other", cleaned_data$Model)

# Check the updated frequency of manufacturers
updated_model_counts <- table(cleaned_data$Model)
print(updated_model_counts)

str(cleaned_data)
dim(cleaned_data)



###Data Splitting###
#install.packages("caTools")
library(caTools)
# Set a random seed for reproducibility
set.seed(123)
# Split data: 80% training, 20% testing
split <- sample.split(cleaned_data$price_sqrt, SplitRatio = 0.8)  # Assuming 'Price' is the target variable
trainingSet <- subset(cleaned_data, split == TRUE)
testingSet  <- subset(cleaned_data, split == FALSE)
# Check dimensions
dim(trainingSet)  # Should be ~80% of total rows
dim(testingSet)   # Should be ~20% of total rows
#summary(trainingSet)
trainingClean = trainingSet
testingClean = testingSet
str(testingClean)

trainingClean$Car_Age <- 2024 - trainingClean$Prod_year
testingClean$Car_Age <- 2024 - testingClean$Prod_year
library(ggplot2)

# Example dataset (replace with your actual dataset)
ggplot(trainingClean, aes(x = Car_Age, y = Mileage_sqrt)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
  labs(title = "Scatter Plot of Mileage_sqrt vs. Car Age",
       x = "Car Age (Years)",
       y = "Mileage (Square Root Transformed)") +
  theme_minimal()

#install.packages("ggplot2")  # Install ggplot2 (only run this once)
library(ggplot2)  # Load ggplot2

str(trainingClean)

str(trainingClean)
colnames(trainingClean)
colnames(testingClean)

#install.packages("caret")
library(caret)
library(dplyr)

dim(trainingSet)
dim(trainingClean)
dim(testingSet)
dim(testingClean)
str(trainingClean)
str(testingClean)
head(trainingClean)  
trainingClean <- trainingClean %>% mutate_if(is.character, as.factor)
testingClean <- testingClean %>% mutate_if(is.character, as.factor)
# Ensure test set has the same factor levels as the training set
for (col in names(trainingClean)) {
  if (is.factor(trainingClean[[col]])) {
    testingClean[[col]] <- factor(testingClean[[col]], levels = levels(testingClean[[col]]))
  }
}
fuel_countsTrain <- table(trainingClean$Fuel_type)
fuel_countsTest <- table(testingClean$Fuel_type)

trainingClean$Turbo <- factor(trainingClean$Turbo,levels = c(0, 1), labels = c("0","1"))
trainingClean$Cylinders <- factor(trainingClean$Cylinders)
trainingClean$Doors <- factor(trainingClean$Doors)
trainingClean$Airbags <- factor(trainingClean$Airbags)
trainingClean$Color <- factor(trainingClean$Color)
str(trainingClean)
testingClean$Turbo <- factor(testingClean$Turbo,levels = c(0, 1), labels = c("0","1"))
testingClean$Cylinders <- factor(testingClean$Cylinders)
testingClean$Doors <- factor(testingClean$Doors)
testingClean$Airbags <- factor(testingClean$Airbags)
testingClean$Color <- factor(testingClean$Color)
str(testingClean)
str(trainingClean)
str(testingClean)

common_levels <- union(levels(trainingClean$Fuel_type), levels(testingClean$Fuel_type))
trainingClean$Fuel_type <- factor(trainingClean$Fuel_type, levels = common_levels)
testingClean$Fuel_type <- factor(testingClean$Fuel_type, levels = common_levels)
levels(testingClean$Fuel_type)
levels(trainingClean$Fuel_type)
trainingModel = trainingClean
testingModel = testingClean
hist(trainingModel$price_sqrt, main = "Histogram of Price_sqrt", 
     xlab = "Price_sqrt", col = "lightblue", border = "black")
qqnorm(trainingModel$price_sqrt)
qqline(trainingModel$price_sqrt, col = "red")


sapply(trainingModel, function(x) if(is.factor(x)) levels(x))
airbagsTrain <- table(trainingModel$Airbags)
airbagsTest <- table(testingModel$Airbags)
# Calculate the frequency of each manufacturer
airbags_counts <- table(trainingModel$Airbags)
# Convert to a data frame for easier manipulation
airbags_freq <- as.data.frame(airbags_counts)
colnames(airbags_freq) <- c("Airbags", "Frequency")

# Calculate the percentage of each category
total_observations <- sum(airbags_freq$Frequency)
airbags_freq$Percentage <- airbags_freq$Frequency / total_observations * 100


# View the frequency table
print(airbags_freq) 

colorTrain <- table(trainingModel$Color)
colorTest <- table(testingModel$Color)
# Calculate the frequency of each manufacturer
color_counts <- table(trainingModel$Color)
color_freq <- as.data.frame(color_counts)
colnames(color_freq) <- c("Color", "Frequency")

# Calculate the percentage of each category
total_observations <- sum(color_freq$Frequency)
color_freq$Percentage <- color_freq$Frequency / total_observations * 100


# View the frequency table
print(color_freq)


trainingModel$Cylinders <- factor(trainingModel$Cylinders,levels = c("1", "2", "3", "4", "5", "6", "7", "8", "10", "12"),
                                  labels = c("Small", "Small", "Small", "Standard", "Standard", "Standard", 
                                             "Performance", "Performance", "High_Performance", "High_Performance"))
testingModel$Cylinders <- factor(testingModel$Cylinders,levels = c("1", "2", "3", "4", "5", "6", "7", "8", "10", "12"),
                                 labels = c("Small", "Small", "Small", "Standard", "Standard", "Standard", 
                                            "Performance", "Performance", "High_Performance", "High_Performance"))
cylinderTrain <- table(trainingModel$Cylinders)
cylinderTest <- table(testingModel$Cylinders)
common_levels_c <- union(levels(trainingModel$Cylinders), levels(testingModel$Cylinders))
trainingModel$Cylinders <- factor(trainingModel$Cylinders, levels = common_levels_c)
testingModel$Cylinders <- factor(testingModel$Cylinders, levels = common_levels_c)
levels(testingModel$Cylinders)
levels(trainingModel$Cylinders)

trainingModel$Category <- factor(trainingModel$Category,levels = c("Cabriolet", "Coupe", "Goods wagon", "Hatchback", "Jeep", "Limousine",
                                                                   "Microbus", "Minivan", "Pickup", "Sedan", "Universal"),
                                 labels = c("Sports_and_Luxury", "Sports_and_Luxury", "Commercial", "Passenger_Cars", 
                                            "SUV_and_Offroad", "Luxury", "Family_and_Vans", "Family_and_Vans", 
                                            "Commercial", "Passenger_Cars", "Wagon"))
testingModel$Category <- factor(testingModel$Category,levels = c("Cabriolet", "Coupe", "Goods wagon", "Hatchback", "Jeep", "Limousine",
                                                                 "Microbus", "Minivan", "Pickup", "Sedan", "Universal"),
                                labels = c("Sports_and_Luxury", "Sports_and_Luxury", "Commercial", "Passenger_Cars", 
                                           "SUV_and_Offroad", "Luxury", "Family_and_Vans", "Family_and_Vans", 
                                           "Commercial", "Passenger_Cars", "Wagon"))
categoryTrain <- table(trainingModel$Category)
categoryTest <- table(testingModel$Category)
common_levels_c <- union(levels(trainingModel$Category), levels(testingModel$Category))
trainingModel$Category <- factor(trainingModel$Category, levels = common_levels_c)
testingModel$Category <- factor(testingModel$Category, levels = common_levels_c)
levels(testingModel$Category)
levels(trainingModel$Category)
trainingModel$Airbags <- factor(trainingModel$Airbags,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
                                  labels = c("No_Airbags", "Rare_Low_Safety", "Moderate_Low_Safety", "Rare_Low_Safety", "Popular_Low_Safety", "Rare_Low_Safety", 
                                             "Moderate_Low_Safety", "Rare_Low_Safety", "Moderate_High_Safety", "Rare_High_Safety","Moderate_High_Safety",
                                             "Rare_High_Safety","Popular_High_Safety","Rare_High_Safety","Rare_High_Safety","Rare_High_Safety","Rare_High_Safety"))

testingModel$Airbags <- factor(testingModel$Airbags,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
                                labels = c("No_Airbags", "Rare_Low_Safety", "Moderate_Low_Safety", "Rare_Low_Safety", "Popular_Low_Safety", "Rare_Low_Safety", 
                                           "Moderate_Low_Safety", "Rare_Low_Safety", "Moderate_High_Safety", "Rare_High_Safety","Moderate_High_Safety",
                                           "Rare_High_Safety","Popular_High_Safety","Rare_High_Safety","Rare_High_Safety","Rare_High_Safety","Rare_High_Safety"))
airbagsTrain <- table(trainingModel$Airbags)
airbagsTest <- table(testingModel$Airbags)
# Remove unused levels from the Airbags variable
trainingModel$Airbags <- droplevels(trainingModel$Airbags)
testingModel$Airbags <- droplevels(testingModel$Airbags)

# Verify the updated levels
print(levels(trainingModel$Airbags))
print(levels(testingModel$Airbags))
common_levels_c <- union(levels(trainingModel$Airbags), levels(testingModel$Airbags))
trainingModel$Airbags <- factor(trainingModel$Airbags, levels = common_levels_c)
testingModel$Airbags <- factor(testingModel$Airbags, levels = common_levels_c)
levels(testingModel$Airbags)
levels(trainingModel$Airbags)

trainingModel$Fuel_type <- factor(trainingModel$Fuel_type,levels = c("CNG", "Diesel", "Hybrid", "Hydrogen", "LPG", "Petrol", "Plug-in Hybrid"),
                                  labels = c("Alternative_Fuel", "Diesel", "Hybrid", "Hydrogen", "Alternative_Fuel", "Petrol", "Plug_in_Hybrid"))
testingModel$Fuel_type <- factor(testingModel$Fuel_type,levels = c("CNG", "Diesel", "Hybrid", "Hydrogen", "LPG", "Petrol", "Plug-in Hybrid"),
                                 labels = c("Alternative_Fuel", "Diesel", "Hybrid", "Hydrogen", "Alternative_Fuel", "Petrol", "Plug_in_Hybrid"))
fuelTrain <- table(trainingModel$Fuel_type)
fuelTest <- table(testingModel$Fuel_type)

common_levels_c <- union(levels(trainingModel$Fuel_type), levels(testingModel$Fuel_type))
trainingModel$Fuel_type <- factor(trainingModel$Fuel_type, levels = common_levels_c)
testingModel$Fuel_type <- factor(testingModel$Fuel_type, levels = common_levels_c)
levels(testingModel$Fuel_type)
levels(trainingModel$Fuel_type)

trainingModel$Wheel <- factor(trainingModel$Wheel,levels = c("Left wheel","Right-hand drive"),
                              labels = c("Left_wheel","Right_hand_drive"))
testingModel$Wheel <- factor(testingModel$Wheel,levels = c("Left wheel","Right-hand drive"),
                             labels = c("Left_wheel","Right_hand_drive"))
wheelTrain <- table(trainingModel$Wheel)
wheelTest <- table(testingModel$Wheel)

common_levels_c <- union(levels(trainingModel$Wheel), levels(testingModel$Wheel))
trainingModel$Wheel <- factor(trainingModel$Wheel, levels = common_levels_c)
testingModel$Wheel <- factor(testingModel$Wheel, levels = common_levels_c)
levels(testingModel$Wheel)
levels(trainingModel$Wheel)


trainingModel$Manufacturer <- factor(trainingModel$Manufacturer,levels = c("BMW","CHEVROLET","FORD","HONDA","HYUNDAI","LEXUS",         
"MERCEDES-BENZ","Other","TOYOTA"),
labels = c("BMW","CHEVROLET","FORD","HONDA","HYUNDAI","LEXUS",         
           "MERCEDES_BENZ","Other","TOYOTA"))
testingModel$Manufacturer <- factor(testingModel$Manufacturer,levels = c("BMW","CHEVROLET","FORD","HONDA","HYUNDAI","LEXUS",         
                                                                         "MERCEDES-BENZ","Other","TOYOTA"),
                                    labels = c("BMW","CHEVROLET","FORD","HONDA","HYUNDAI","LEXUS",         
                                               "MERCEDES_BENZ","Other","TOYOTA"))

manuTrain <- table(trainingModel$Manufacturer)
manuTest <- table(testingModel$Manufacturer)


common_levels_c <- union(levels(trainingModel$Manufacturer), levels(testingModel$Manufacturer))
trainingModel$Manufacturer <- factor(trainingModel$Manufacturer, levels = common_levels_c)
testingModel$Manufacturer <- factor(testingModel$Manufacturer, levels = common_levels_c)
levels(testingModel$Manufacturer)
levels(trainingModel$Manufacturer)




trainingModel$Color <- factor(trainingModel$Color,levels = c("Beige","Black","Blue","Brown","Carnelian red","Golden","Green",       
                                                             "Grey","Orange","Pink","Purple","Red","Silver","Sky blue","White","Yellow"),
                              labels = c("Rare_colors","Populor_colors","Moderate_colors","Rare_colors","Rare_colors","Rare_colors","Moderate_colors",       
                                         "Populor_colors","Moderate_colors","Rare_colors","Rare_colors","Moderate_colors","Populor_colors","Rare_colors","Populor_colors","Rare_colors"))
testingModel$Color <- factor(testingModel$Color,levels = c("Beige","Black","Blue","Brown","Carnelian red","Golden","Green",       
                                                             "Grey","Orange","Pink","Purple","Red","Silver","Sky blue","White","Yellow"),
                              labels = c("Rare_colors","Populor_colors","Moderate_colors","Rare_colors","Rare_colors","Rare_colors","Moderate_colors",       
                                         "Populor_colors","Moderate_colors","Rare_colors","Rare_colors","Moderate_colors","Populor_colors","Rare_colors","Populor_colors","Rare_colors"))

colorTrain <- table(trainingModel$Color)
colorTest <- table(testingModel$Color)

common_levels_c <- union(levels(trainingModel$Color), levels(testingModel$Color))
trainingModel$Color <- factor(trainingModel$Color, levels = common_levels_c)
testingModel$Color <- factor(testingModel$Color, levels = common_levels_c)
levels(testingModel$Color)
levels(trainingModel$Color)


trainingModel$Model <- factor(trainingModel$Model,levels = c( "Actyon","Aqua","Astra","Camry","Captiva","Civic","Cruze",     
                                                              "CT 200h","E 350","Elantra","Escape","FIT","Forester","Fusion",    
                                                              "Genesis","GX 460","GX 470","H1","Highlander","Insight","Jetta",     
                                                              "Juke","Lacetti","ML 350","Optima","Orlando","Other","Passat",    
                                                              "Prius","Prius C","RAV 4","REXTON","RX 450","Santa FE","Sonata",    
                                                              "Transit","Tucson","Volt","X5" ),
                                     labels = c("Actyon","Aqua","Astra","Camry","Captiva","Civic","Cruze",     
                                                "CT_200h","E_350","Elantra","Escape","FIT","Forester","Fusion",    
                                                "Genesis","GX_460","GX_470","H1","Highlander","Insight","Jetta",     
                                                "Juke","Lacetti","ML_350","Optima","Orlando","Other","Passat",    
                                                "Prius","Prius C","RAV_4","REXTON","RX_450","Santa_FE","Sonata",    
                                                "Transit","Tucson","Volt","X5"))
testingModel$Model <- factor(testingModel$Model,levels = c("Actyon","Aqua","Astra","Camry","Captiva","Civic","Cruze",     
                                                           "CT 200h","E 350","Elantra","Escape","FIT","Forester","Fusion",    
                                                           "Genesis","GX 460","GX 470","H1","Highlander","Insight","Jetta",     
                                                           "Juke","Lacetti","ML 350","Optima","Orlando","Other","Passat",    
                                                           "Prius","Prius C","RAV 4","REXTON","RX 450","Santa FE","Sonata",    
                                                           "Transit","Tucson","Volt","X5" ),
                                    labels = c("Actyon","Aqua","Astra","Camry","Captiva","Civic","Cruze",     
                                               "CT_200h","E_350","Elantra","Escape","FIT","Forester","Fusion",    
                                               "Genesis","GX_460","GX_470","H1","Highlander","Insight","Jetta",     
                                               "Juke","Lacetti","ML_350","Optima","Orlando","Other","Passat",    
                                               "Prius","Prius C","RAV_4","REXTON","RX_450","Santa_FE","Sonata",    
                                               "Transit","Tucson","Volt","X5"))
modTrain <- table(trainingModel$Model)
modTest <- table(testingModel$Model)

common_levels_c <- union(levels(trainingModel$Model), levels(testingModel$Model))
trainingModel$Model <- factor(trainingModel$Model, levels = common_levels_c)
testingModel$Model <- factor(testingModel$Model, levels = common_levels_c)
levels(testingModel$Model)
levels(trainingModel$Model)

str(trainingModel)
str(testingModel)
colnames(trainingModel)
# Identify missing columns in each dataset
missing_in_test <- setdiff(colnames(trainingModel), colnames(testingModel))
missing_in_train <- setdiff(colnames(testingModel), colnames(trainingModel))
install.packages("dplyr")
library(dplyr)
str(trainingModel)
str(testingModel)
trainingModel = trainingModel %>% select(-Price,-ID,-Prod_year,-Mileage_km,-anomaly_score,-is_outlier,-LogPrice,-LogMileage)
testingModel = testingModel %>% select(-Price,-ID,-Prod_year,-Mileage_km,-anomaly_score,-is_outlier,-LogPrice,-LogMileage)
# Ensure both datasets have the same column order
trainingModel <- trainingModel[, order(names(trainingModel))]
testingModel <- testingModel[, order(names(testingModel))]
colnames(trainingModel)
colnames(testingModel)
str(trainingModel)
str(testingModel$man)
dim(trainingModel)
dim(testingModel)
# List of categorical variables
categorical_vars <- c("Airbags", "Category","Color","Cylinders","Doors","Drive_wheels","Fuel_type","Gear_box_type","Leather_interior","Turbo","Wheel","Manufacturer","Model")  # Add all categorical variables here

# Apply the same levels to all categorical variables in the testing dataset
for (var in categorical_vars) {
  # Get the levels from the training dataset
  var_levels <- levels(trainingModel[[var]])
  
  # Apply the same levels to the testing dataset
  testingModel[[var]] <- factor(testingModel[[var]], levels = var_levels)
}

# Verify the levels for one variable (e.g., "Airbags")
print(levels(testingModel$Airbags))
# Check levels for all categorical variables
for (var in categorical_vars) {
  cat("Variable:", var, "\n")
  cat("Training levels:", levels(trainingModel[[var]]), "\n")
  cat("Testing levels:", levels(testingModel[[var]]), "\n")
  cat("\n")
}
#add your file location
write.csv(trainingModel, "D:/UOC/3rd year/Sem 2/ST 3082 - Machine learning/Car price/trainingModel.csv", row.names = FALSE)
write.csv(testingModel, "D:/UOC/3rd year/Sem 2/ST 3082 - Machine learning/Car price/testingModel.csv", row.names = FALSE)

