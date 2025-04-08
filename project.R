# Load necessary libraries
library(tidyverse)
library(caret)      # for preprocessing and modeling
library(DataExplorer) # for easy EDA visuals

# Load data
df <- read.csv(file.choose())

# View structure
str(df)

# Quick summary
summary(df)

# Check for missing values
colSums(is.na(df))

# Check class distribution for target variable
table(df$Alzheimer.s.Diagnosis)
prop.table(table(df$Alzheimer.s.Diagnosis))  # relative freq

# Convert character columns to factors
df <- df %>% 
  mutate(across(where(is.character), as.factor))

# View updated structure
str(df)

# Optional: Convert integer columns with few unique values to factors
# (e.g. 0/1, Yes/No encoded as numbers)
#df <- df %>%
#  mutate(across(where(is.integer), ~ ifelse(n_distinct(.) <= 5, as.factor(.), .)))

# Recheck
str(df)

# Handle missing values
# If less than 10% missing, drop rows
#df <- df %>% drop_na()
#We do not have NA's

# Scale numeric features (important for KNN!)
# Keep factor columns as is
numeric_cols <- df %>% select(where(is.numeric)) %>% names()
df_scaled <- df %>%
  mutate(across(all_of(numeric_cols), scale))



set.seed(123)  # for reproducibility

# We'll assume the target is named 'Alzheimer.Diagnosis'
# df$Alzheimer.s.Diagnosis <- as.factor(df$Alzheimer.s.Diagnosis)

# Train/Test split
train_index <- createDataPartition(df$Alzheimer.s.Diagnosis, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

# Fit logistic model
logit_model <- glm(Alzheimer.s.Diagnosis ~ ., data = train_data, family = "binomial")

# Predict probabilities
logit_probs <- predict(logit_model, newdata = test_data, type = "response")

# Convert to class predictions
logit_preds <- ifelse(logit_probs > 0.5, "Yes", "No")
logit_preds <- factor(logit_preds, levels = levels(test_data$Alzheimer.s.Diagnosis))

# Evaluate
mean(logit_preds == test_data$Alzheimer.s.Diagnosis)
confusionMatrix(logit_preds, test_data$Alzheimer.s.Diagnosis)


#Diagnosis: Your model is better at catching the "No Alzheimer's" cases. 
#We might be missing some "Yes" cases (false negatives), which is important in a medical context.

library(MASS)

# Fit LDA model
lda_model <- lda(Alzheimer.s.Diagnosis ~ ., data = train_data)

# Predict on test data
lda_preds <- predict(lda_model, newdata = test_data)$class

# Evaluate
mean(lda_preds == test_data$Alzheimer.s.Diagnosis)
confusionMatrix(lda_preds, test_data$Alzheimer.s.Diagnosis)


library(class)

# Identify numeric columns
numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]

# Preprocess (scale based on training data)
pre_proc <- preProcess(train_data[, numeric_cols], method = c("center", "scale"))

train_scaled <- train_data
test_scaled  <- test_data

train_scaled[, numeric_cols] <- predict(pre_proc, train_data[, numeric_cols])
test_scaled[, numeric_cols]  <- predict(pre_proc, test_data[, numeric_cols])

# Prepare input/output
train_x <- train_scaled[, numeric_cols]
test_x  <- test_scaled[, numeric_cols]
train_y <- train_scaled$Alzheimer.s.Diagnosis
test_y  <- test_scaled$Alzheimer.s.Diagnosis

# Apply KNN with k = 5 (try different k later)
knn_preds <- knn(train = train_x, test = test_x, cl = train_y, k = 5)

# Evaluate
mean(knn_preds == test_y)
confusionMatrix(knn_preds, test_y)




# Set up 10-fold CV
control <- trainControl(method = "cv", number = 10)

# Logistic Regression CV
cv_logit <- train(Alzheimer.s.Diagnosis ~ ., data = df, method = "glm", family = "binomial", trControl = control)

# LDA CV
cv_lda <- train(Alzheimer.s.Diagnosis ~ ., data = df, method = "lda", trControl = control)

# KNN CV (tune k)
# cv_knn <- train(Alzheimer.s.Diagnosis ~ ., data = df, method = "knn", tuneLength = 10, trControl = control)
#cv_knn <- train(Alzheimer.s.Diagnosis ~ ., 
              #  data = df, 
              # method = "knn", 
              # tuneLength = 5,  # fewer k values to test
              # trControl = trainControl(method = "cv", number = 5))  # 5-fold instead of 10

# View accuracies
cv_logit$results
cv_lda$results
#cv_knn$results

resamples <- resamples(list(
  Logistic = cv_logit,
  LDA = cv_lda
  #KNN = cv_knn
))

summary(resamples)
bwplot(resamples)  # visualize comparison


#######################
## QDA  ##

qda_model <- qda(Alzheimer.s.Diagnosis ~ ., data = train_data)
qda_preds <- predict(qda_model, newdata = test_data)$class

mean(qda_preds == test_data$Alzheimer.s.Diagnosis)
confusionMatrix(qda_preds, test_data$Alzheimer.s.Diagnosis)


#####################
##  NAIVE BAYES   ##

library(e1071)

nb_model <- naiveBayes(Alzheimer.s.Diagnosis ~ ., data = train_data)
nb_preds <- predict(nb_model, newdata = test_data)

mean(nb_preds == test_data$Alzheimer.s.Diagnosis)
confusionMatrix(nb_preds, test_data$Alzheimer.s.Diagnosis)


##################33

# Logistic Regression
logit_cm <- confusionMatrix(logit_preds, test_data$Alzheimer.s.Diagnosis)

# LDA
lda_cm <- confusionMatrix(lda_preds, test_data$Alzheimer.s.Diagnosis)

# QDA
qda_cm <- confusionMatrix(qda_preds, test_data$Alzheimer.s.Diagnosis)

# Naive Bayes
nb_cm <- confusionMatrix(nb_preds, test_data$Alzheimer.s.Diagnosis)




# Create a data frame with the key metrics
model_comparison <- data.frame(
  Model = c("Logistic Regression", "LDA", "QDA", "Naive Bayes"),
  Accuracy = c(
    logit_cm$overall["Accuracy"],
    lda_cm$overall["Accuracy"],
    qda_cm$overall["Accuracy"],
    nb_cm$overall["Accuracy"]
  ),
  Sensitivity_No = c(
    logit_cm$byClass["Sensitivity"],
    lda_cm$byClass["Sensitivity"],
    qda_cm$byClass["Sensitivity"],
    nb_cm$byClass["Sensitivity"]
  ),
  Specificity_Yes = c(
    logit_cm$byClass["Specificity"],
    lda_cm$byClass["Specificity"],
    qda_cm$byClass["Specificity"],
    nb_cm$byClass["Specificity"]
  ),
  Kappa = c(
    logit_cm$overall["Kappa"],
    lda_cm$overall["Kappa"],
    qda_cm$overall["Kappa"],
    nb_cm$overall["Kappa"]
  )
)

# Print the results
print(model_comparison)
