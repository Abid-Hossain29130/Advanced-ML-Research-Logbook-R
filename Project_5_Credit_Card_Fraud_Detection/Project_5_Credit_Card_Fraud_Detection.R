#install.packages("ranger")
#install.packages("caret")
#install.packages("data.table")


# 1. Load the heavy machinery
library(ranger)
library(caret)
library(data.table)

# 2. Import the data from YOUR folder
# (Make sure the file is named exactly 'creditcard.csv')
creditcard_data <- read.csv("creditcard.csv")

# 3. Quick audit to make sure it loaded correctly
dim(creditcard_data)

#-----------Step:02 Data exploration---------------------

# 1. Inspect the top and bottom of the dataset
head(creditcard_data, 6)
tail(creditcard_data, 6)

# 2. Audit the target variable (Class)
table(creditcard_data$Class)

# 3. Statistical analysis of the Amount column
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)
sd(creditcard_data$Amount)

#-----------Step:02 Data Manipulation--------------------
# 1. Scale the Amount column
creditcard_data$Amount = scale(creditcard_data$Amount)

# 2. Remove the 'Time' column as it is not needed for the classification
NewData = creditcard_data[, -c(1)]

# 3. Inspect the transformed data
head(NewData)

#----------Step:04 Data MOdeling (Train-test split)-----------------------

# 1. Load the splitting library
library(caTools)

# 2. Set the seed for reproducibility
set.seed(123)

# 3. Create a split ratio of 0.80 (80% for training)
data_sample = sample.split(NewData$Class, SplitRatio = 0.80)

# 4. Subset the data into training and testing sets
train_data = subset(NewData, data_sample == TRUE)
test_data = subset(NewData, data_sample == FALSE)

# 5. Check the dimensions of the new sets
dim(train_data)
dim(test_data)

#-----------Step:05 Fitting the logistic Regression Model-----------
# 1. Fit the Logistic Regression Model
# We use glm (Generalized Linear Model) with family=binomial for binary classification
Logistic_Model = glm(Class ~ ., test_data, family = binomial())

# 2. Summarize the model results
summary(Logistic_Model)

# 3. Visualize the model performance through diagnostic plots
plot(Logistic_Model)

# 4. Assessing performance with the ROC Curve
library(pROC)
auc.gbm = roc(train_data$Class, lr.predict, plot = TRUE, col = "blue")
library(pROC)

#------------Step:06 Fitting a Decision Tree Model---------------
#install.packages("rpart")       # For the Decision Tree logic
#install.packages("rpart.plot")  # For the Flowchart visualization
#install.packages("neuralnet")   # For the Step 07 Brain-like model
#install.packages("gbm")
# 1. Load the decision tree libraries
library(rpart)
library(rpart.plot)

# 2. Train the Decision Tree model using recursive partitioning
# We use the full dataset here to capture the most patterns for the visualization
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')

# 3. Generate predictions (Class and Probabilities)
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')

# 4. Plot the actual Decision Tree flowchart
rpart.plot(decisionTree_model)



#------------Step:07 Artificial Neural Network (ANN)---------------
library(neuralnet)

# 1. Train the ANN model 
# We use the training split to teach the 'Brain'
# Note: If this takes too long, we can simplify the layers.
ANN_model = neuralnet(Class ~ ., train_data, linear.output = FALSE, hidden = 2)

# 2. Visualize the Network
# This will open a NEW window with the neural diagram
plot(ANN_model)

# 3. Predict and Threshold
predANN = compute(ANN_model, test_data)
resultANN = ifelse(predANN$net.result > 0.5, 1, 0)

#------------Step:08 Gradient Boosting (GBM)---------------
library(gbm)
library(pROC)

# 1. Train the GBM Model 
# (We use the stable OOB method for performance)
model_gbm <- gbm(Class ~ . , 
                 distribution = "bernoulli", 
                 data = train_data, 
                 n.trees = 500, 
                 interaction.depth = 3, 
                 shrinkage = 0.01,
                 train.fraction = 0.8) # This helps gbm.perf work!

# 2. Find the best performance iteration using 'test' 
# (This should now show a plot without the 'ylim' error)
gbm.iter = gbm.perf(model_gbm, method = "test")

# 3. Final Prediction
gbm_predict = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)

# 4. Generate the final ROC Curve
gbm_auc = roc(test_data$Class, gbm_predict, plot = TRUE, col = "red")
print(gbm_auc)

































