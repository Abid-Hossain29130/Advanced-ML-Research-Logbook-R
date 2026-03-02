# --- Step 1: Loading Libraries & Data ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, FNN)

# Define the source URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data"

# Define Column Names (The raw data doesn't have a header)
cols <- c("symboling", "normalized_losses", "make", "fuel_type", "aspiration", 
          "num_doors", "body_style", "drive_wheels", "engine_location", 
          "wheel_base", "length", "width", "height", "curb_weight", 
          "engine_type", "num_cylinders", "engine_size", "fuel_system", 
          "bore", "stroke", "compression_ratio", "horsepower", "peak_rpm", 
          "city_mpg", "highway_mpg", "price")

# Fetch and Read Data
cars <- read_csv(url, col_names = cols)

# Audit the data
head(cars)

# --- Step 2: Data Cleaning ---

# 1. Replace the '?' character with actual NA (Not Available) values
cars[cars == "?"] <- NA

# 2. Select only the columns we need for prediction (The "Feature Set")
# We focus on numeric specs that likely influence price
cars_clean <- cars %>% 
  select(wheel_base, length, width, height, curb_weight, 
         engine_size, bore, stroke, compression_ratio, 
         horsepower, peak_rpm, city_mpg, highway_mpg, price) %>%
  mutate(across(everything(), as.numeric)) # Force all to numeric

# 3. Remove rows with missing values to ensure model stability
cars_clean <- na.omit(cars_clean)

# 4. Quality Audit: Check the dimensions and structure after cleaning
dim(cars_clean)
str(cars_clean)

# --- Step 3: Normalization ---

# 1. Separate the 'Target' (Price) from the 'Features'
# We don't normalize the price because that's what we want to predict!
target <- cars_clean$price
features <- cars_clean %>% select(-price)

# 2. Apply Scaling to the features
# The scale() function subtracts the mean and divides by the standard deviation
features_scaled <- as.data.frame(scale(features))

# 3. Re-combine them into our final modeling dataset
cars_final <- cbind(features_scaled, price = target)

# 4. Visual Audit: Compare the data before and after
summary(features$curb_weight)        # Large original scale
summary(features_scaled$curb_weight) # Normalized scale (Mean = 0)

# --- Step 4: Data Splitting ---

# 1. Set seed for reproducibility (so we both get the same split)
set.seed(123)

# 2. Generate a random sample of row indices for the training set (80%)
train_indices <- sample(1:nrow(cars_final), size = 0.8 * nrow(cars_final))

# 3. Create the Training Set
train_data <- cars_final[train_indices, ]

# 4. Create the Test Set (everything NOT in the training indices)
test_data <- cars_final[-train_indices, ]

# 5. Audit the split sizes
cat("Training Set Rows:", nrow(train_data), "\n")
cat("Test Set Rows:", nrow(test_data), "\n")


# --- Step 5: Training the Machine Learning Model ---

# 1. Isolate the 'Questions' (Features) for both Train and Test
train_x <- train_data %>% select(-price)
test_x <- test_data %>% select(-price)

# 2. Isolate the 'Answers' (Target) for the Training set
train_y <- train_data$price

# 3. Run the KNN Regression Model (k = 5)
# This asks R to find the 5 most similar cars in train_x to guess test_x
predictions <- knn.reg(train = train_x, 
                       test = test_x, 
                       y = train_y, 
                       k = 5)

# 4. View the Predicted Prices vs. The Actual Prices
results_comparison <- data.frame(Actual = test_data$price, 
                                 Predicted = predictions$pred)

head(results_comparison)


# --- Step 6: Performance Evaluation ---

# 1. Calculate the Error (Residuals)
error <- results_comparison$Actual - results_comparison$Predicted

# 2. Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean(error^2))

# 3. Calculate MAE (Mean Absolute Error)
mae <- mean(abs(error))

# 4. Print the "Report Card"
cat("Final Model Grade:\n")
cat("------------------\n")
cat("RMSE: $", round(rmse, 2), "\n")
cat("MAE:  $", round(mae, 2), "\n")


# Final Accuracy Visualization
ggplot(results_comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "royalblue", size = 3, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Project 03 Final Audit: Actual vs. Predicted Prices",
       subtitle = "Red dashed line represents perfect prediction",
       x = "Actual Price ($)", y = "AI Predicted Price ($)") +
  theme_minimal()














