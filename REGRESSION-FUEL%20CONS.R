library(dplyr)
library(ggplot2)
library(moments)
library(nortest)
library(corrplot)

set.seed(1)
dataset <- read.csv(file.choose(), header=T)

# Calculate NA counts for each variable
na_counts <- sapply(dataset, function(x) sum(is.na(x)))
na_counts
#There are no NA values in our dataset

head(dataset)

unique_values <- lapply(dataset, unique)
unique_values
#All the cars are from 2014
#I checked for the unique values to make sure there are no similar
#entries or the same word spelled differently.
#There are 39 different makers

names(dataset)
summary(dataset)

#Gives us the type of each variable
str(dataset)

# Counts the number of duplicate rows
sum(duplicated(df))  #0


#Because the YEAR column does not give us any information we could drop it


#EXPLORE THE NUMERICAL COLUMNS
hist(dataset$ENGINESIZE, main="Histogram of ENGINESIZE", col="lightblue")
hist(dataset$FUELCONSUMPTION_CITY, main="Histogram of FUELCONSUMPTION CITY", col="lightblue")
hist(dataset$FUELCONSUMPTION_HWY, main="Histogram of FUELCONSUMPTION_HWY", col="lightblue")
hist(dataset$FUELCONSUMPTION_COMB, main="Histogram of FUELCONSUMPTION COMB", col="lightblue")
hist(dataset$CYLINDERS, main="Histogram of CYLINDERS", col="lightblue")
hist(dataset$FUELCONSUMPTION_COMB_MPG, main="Histogram of FUELCONSUMPTION COMB MPG", col="lightblue")
hist(dataset$CO2EMISSIONS, main="Histogram of CO2 EMMISSIONS", col="lightblue")


#EXPLORE THE CATEGORICAL COLUMNS
table(dataset$MAKE) #Gives us how many times each variable appears
ggplot(dataset, aes(x = MAKE)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Make", x = "Make", y = "Count")
#As we can see from the plot, we have many different values and it is difficult
#to understand clearly some insights on the graph, however we can see from which
#maker we have the most cars and the least cars.

#What we could check instead is the top 10 makes
top_makes <- dataset %>%
  count(MAKE, sort = TRUE) %>%
  top_n(10)

ggplot(top_makes, aes(x = reorder(MAKE, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Vehicle Makes", x = "Make", y = "Count")


ggplot(dataset, aes(x = MODEL)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Model", x = "Model", y = "Count")
#We have too many values so a plot is not helpful here

ggplot(dataset, aes(x = VEHICLECLASS)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Vehicle Class", x = "Vehicle Class", y = "Count")
#From this plot we can see that most cars are mid-size (other conclusions can
#be made by looking at the graph).

ggplot(dataset, aes(x = TRANSMISSION)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Transmission", x = "Transmission", y = "Count")

ggplot(dataset, aes(x = FUELTYPE)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Fuel Type", x = "Fuel Type", y = "Count")


#Next we could check for any correlation in the numerical features 
cor_matrix<-cor(dataset[sapply(dataset, is.numeric)], use="complete.obs")  # Computes correlation matrix
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7)

##################################################################
#Some of the relationships we could explore are the following

#1. Since the columns FUELCONSUMPTION_COMB and the FUELCONSUMPTION_COMB_MPG
#   tell us the same thing in different unit of measurement we could check
#   if the calculations are correct and if so, keep only one of the 2 columns

#The equation for transforming L/100km to miles per gallon is:
# l/100km = 235.214583 / mpg
# Create a new column with the calculated MPG
dataset$calculated_mpg <- 235.214583 / dataset$FUELCONSUMPTION_COMB
# Compare with the existing column
sum(abs(dataset$FUELCONSUMPTION_COMB_MPG - round(dataset$calculated_mpg)) > 5)
#There are 196 values that even with an error of 5, they are not calculated correctly.
#We could drop these rows when dealing with this column.
#We could also check for other thresholds and how they behave in each case.
sum(abs(dataset$FUELCONSUMPTION_COMB_MPG - round(dataset$calculated_mpg)) > 3)
sum(abs(dataset$FUELCONSUMPTION_COMB_MPG - round(dataset$calculated_mpg)) > 8)
sum(abs(dataset$FUELCONSUMPTION_COMB_MPG - round(dataset$calculated_mpg)) > 10)

# Compute the difference between reported and calculated MPG
dataset$mpg_difference <- dataset$FUELCONSUMPTION_COMB_MPG - dataset$calculated_mpg

# Plot the histogram of differences
hist(dataset$mpg_difference,
     breaks = 30,
     col = "skyblue",
     border = "black",
     main = "Difference: Reported MPG - Calculated MPG",
     xlab = "MPG Difference",
     ylab = "Frequency")

#Another way of choosing a threshold is by using the Interquartile Range
# Calculate IQR
Q1 <- quantile(dataset$mpg_difference, 0.25)  # 25th percentile
Q3 <- quantile(dataset$mpg_difference, 0.75)  # 75th percentile
IQR_value <- Q3 - Q1  # Interquartile range

# Define an outlier threshold (1.5 * IQR rule)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

print(upper_bound)  # This is the suggested threshold

#Another way of choosing a threshold is the use of percentiles
# Set threshold as the 95th percentile
percentile_95 <- quantile(dataset$mpg_difference, 0.95)
print(percentile_95)
# Count values exceeding the threshold
sum(dataset$mpg_difference > percentile_95) #52
#So there are 52 rows that we could delete since they do not satisfy the 
#formula we use.


#2. Check if the combutations are done correctly when calculating the
#   FUELCONSUMPTION_COMB

# Calculate the simple average
dataset$simple_avg <- (dataset$FUELCONSUMPTION_CITY + dataset$FUELCONSUMPTION_HWY) / 2

# Calculate the weighted average (55% city, 45% hwy)
dataset$weighted_avg <- 0.55 * dataset$FUELCONSUMPTION_CITY + 0.45 * dataset$FUELCONSUMPTION_HWY

# Compare to the actual COMB column
dataset$diff_simple <- abs(dataset$FUELCONSUMPTION_COMB - dataset$simple_avg)
dataset$diff_weighted <- abs(dataset$FUELCONSUMPTION_COMB - dataset$weighted_avg)

# Average absolute differences
mean(dataset$diff_simple)
mean(dataset$diff_weighted)

#We first perform some tests to see which formula is used for the calculation
#of the average of fuel consumption in city and in highway.
#The two possible averages is the simple average and the 55/45 weighted average.
#By looking at the mean above, we can see that the formula with the lower 
#mean difference is the one likely used to compute. 
#So the formula used is:
#        COMB = 0.55 x CITY + 0.45 x HWY

#The next step is to set a threshold:
#A proper threshold could be around 0.1 since the mean difference is close 
#to 0.02510778
#We can also check by using the IQR method
Q1 <- quantile(dataset$diff_weighted, 0.25)
Q3 <- quantile(dataset$diff_weighted, 0.75)
IQR <- Q3 - Q1

threshold <- Q3 + 1.5 * IQR  # Upper threshold
print(threshold)

sum(dataset$diff_weighted > threshold)
#Does not give us anything. So we use the 0.1 threshold.
sum(dataset$diff_weighted>0.1)
#Since also with 0.1 threshold we can see that there are no points with 
#possible noise, we assume that all the calculations are done correctly.

#3. Relationship between Engine Size & CO2 Emissions
#Hypothesis: Larger engines produce more CO₂.
plot(dataset$ENGINESIZE, dataset$CO2EMISSIONS,
     main = "Engine Size vs CO2 Emissions",
     xlab = "Engine Size (L)", ylab = "CO2 Emissions (g/km)",
     col = "tomato", pch = 19)
abline(lm(CO2EMISSIONS ~ ENGINESIZE, data = dataset), col = "blue")

#4. Cylinders vs Fuel Consumption
#Hypothesis: More cylinders → higher consumption.
boxplot(FUELCONSUMPTION_COMB ~ CYLINDERS, data = dataset,
        main = "Fuel Consumption by Cylinder Count",
        xlab = "Cylinders", ylab = "Combined Fuel Consumption",
        col = "lightgreen")

#5. Fuel Type vs CO2 Emissions
#Hypothesis: Fuel types differ in emissions — e.g., diesel vs premium gas.
boxplot(CO2EMISSIONS ~ FUELTYPE, data = dataset,
        main = "CO2 Emissions by Fuel Type",
        xlab = "Fuel Type", ylab = "CO2 Emissions (g/km)",
        col = "lightblue")

#6. Vehicle Class vs Consumption or Emissions
#Hypothesis: Larger vehicles (SUVs) consume more fuel.
ggplot(dataset, aes(x = VEHICLECLASS, y = CO2EMISSIONS)) +
  geom_boxplot(fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "CO2 Emissions by Vehicle Class", x = "Vehicle Class", y = "CO2 Emissions")

#7. Transmission Type vs Efficiency
#Hypothesis: Some automatic/CVT gearboxes may offer better efficiency.
boxplot(FUELCONSUMPTION_COMB ~ TRANSMISSION, data = dataset,
        main = "Fuel Consumption by Transmission",
        xlab = "Transmission", ylab = "Combined Fuel Consumption",
        col = "lightpink", las = 2)

#####################################################
########## Feature Engineering ######################

# Extract Transmission Type and Number of Gears
# The current TRANSMISSION column (e.g., "AS5", "M6") combines both transmission 
# type and number of gears. We'll split them into two new columns.

# Extract Transmission Type (first letter or letters, e.g., "A", "AM", "AS", "M", "AV")
dataset$TransmissionType <- gsub("[0-9]", "", dataset$TRANSMISSION)

# Extract Transmission Gears (numbers only)
dataset$Gears <- as.numeric(gsub("[^0-9]", "", dataset$TRANSMISSION))

# Check the new columns
table(dataset$TransmissionType)
summary(dataset$Gears)

# Simplify Fuel Type (Make them more readable)
# Map fuel codes to readable labels
dataset$FuelTypeClean <- recode(dataset$FUELTYPE,
                                "X" = "Regular",
                                "Z" = "Premium",
                                "D" = "Diesel",
                                "E" = "Ethanol")

# Check distribution
table(dataset$FuelTypeClean)


#Optional Binning (Engine Size into Categories)
# Some models or plots perform better with grouped variables.
# Bin engine sizes into small, medium, large
dataset$EngineCategory <- cut(dataset$ENGINESIZE,
                              breaks = c(0, 2.0, 3.5, Inf),
                              labels = c("Small", "Medium", "Large"))

table(dataset$EngineCategory)


#####################################################
########## Statistical Testing ######################

# ANOVA — Does Fuel Type Affect CO2 Emissions?
# ANOVA tests if the means of multiple groups (e.g., fuel types) 
# are significantly different.
# One-way ANOVA: CO2 emissions by Fuel Type
fuel_anova <- aov(CO2EMISSIONS ~ FuelTypeClean, data = dataset)

# View the ANOVA table
summary(fuel_anova)

#F-value conclusions: This is a very large F-statistic — the variation between 
#                     fuel types is much greater than within groups
#p-value conclusions: The p-value is way smaller than the significance level
#                     0.05 so the differences between fuel types are statistically significant.
#CONCLUSION: There is strong statistical evidence that CO₂ emissions differ by fuel type.
#            This supports including FuelTypeClean in your regression model.



#ANOVA — Does Vehicle Class Affect CO2 Emissions?
# ANOVA: CO2 emissions by Vehicle Class
class_anova <- aov(CO2EMISSIONS ~ VEHICLECLASS, data = dataset)

# View results
summary(class_anova)
# We have high F-value which means there's a lot of variability between vehicle 
# classes relative to within-class variability.
# Also the p-value is smaller than the significance level so we have
# statistically significant result.
# CONCLUSIONS: Vehicle class strongly affects CO₂ emissions.
#              The differences between vehicle categories are not due to chance.
# VEHICLECLASS should definitely be included in your regression model too.


#Visualize the Differences
boxplot(CO2EMISSIONS ~ FuelTypeClean, data = dataset,
        main = "CO2 Emissions by Fuel Type", col = "lightblue")

boxplot(CO2EMISSIONS ~ VEHICLECLASS, data = dataset,
        main = "CO2 Emissions by Vehicle Class", col = "orange", las = 2)

#Normality Check for CO2 Emissions (Optional)
#Some statistical models (like linear regression) assume that the residuals are normally distributed.
#Let’s check the distribution of CO2EMISSIONS using a Q-Q plot and the Shapiro-Wilk test:
# Q-Q plot
qqnorm(dataset$CO2EMISSIONS)
qqline(dataset$CO2EMISSIONS, col = "red")

# Shapiro-Wilk normality test
shapiro.test(dataset$CO2EMISSIONS)



#####################################################
########## Build First Regression Model #############

#STEP1: Simple Linear Regression
#Use one numerical predictor — ENGINESIZE — to predict CO2EMISSIONS.
# Fit a simple linear regression model
model_simple <- lm(CO2EMISSIONS ~ ENGINESIZE, data = dataset)

# View the model summary
summary(model_simple)


#STEP2: Interpretation of Model Output
#Model Equation
#From the output:  CO2EMISSIONS = 125.30 + 39.13×ENGINESIZE

#Coefficient Interpretation
#Intercept (125.30): This is the estimated CO2 emissions when engine size is 0 L. 
#(Not meaningful in real-world terms, but necessary in the model.)
#Slope (39.13): For every 1L increase in engine size, CO₂ emissions increase by ~39.13 g/km, on average.
#This is very significant, confirmed by the very low p-value (< 2e-16).
#####Metric	Value	Meaning
#R-squared->0.7641:    ~76.4% of the variability in CO2 emissions is explained by engine size
#Adjusted R-squared->0.7639:   	Same idea, adjusted for number of predictors (just 1 here)
#Residual Std. Error->30.79:   	Typical prediction error is ~30.8 g/km
#F-statistic->3451:   	Extremely high → strong overall model fit

#CONCLUSION: Engine size is a strong, statistically significant predictor of CO₂ emissions.
#But since 23.6% of the variation is still unexplained, you should now move on to:
#Multiple Linear Regression

#####################################################
########### Multiple Linear Regression ##############

#STEP1: CHOOSE OUR PREDICTORS
#ENGINESIZE (numeric)
#CYLINDERS (numeric)
#FuelTypeClean (categorical)
#VEHICLECLASS (categorical)
#TransmissionType (categorical)

# Fit a multiple linear regression model
model_multi <- lm(CO2EMISSIONS ~ ENGINESIZE + CYLINDERS + FuelTypeClean + VEHICLECLASS + TransmissionType, data = dataset)

# View the summary
summary(model_multi)

#R automatically handles categorical variables as dummy variables (one-hot encoding).
#It picks a reference level for each categorical variable (shown in output).
#The output will include coefficients, p-values, R², and more.

#STEP2: Interpretation of the Multiple Regression Model
#Model Quality Indicators
###Metric	Value	What it Means
#Multiple R-squared->0.8766:   	Your model explains 87.7% of the variation in CO₂ emissions – excellent! 
#Adjusted R²->0.8737:         	Adjusted for number of predictors – still very high, which means your model generalizes well.
#Residual Std. Error->22.52:  	On average, your predictions are off by ±22.5 g/km.
#F-statistic->308.4:          	Very high, and the p-value < 2.2e-16, so your model is overall highly significant.

#Important Coefficients (SEE NOTES ON WORD)

#####################################################
################# Model Diagnostics #################

#Model diagnostics help verify if your linear regression assumptions are met. 
#This is critical to make sure your model is valid and trustworthy.
# Plot diagnostic plots for your model
par(mfrow = c(2, 2))  # Arrange plots in 2x2 layout
plot(model_multi)

# Cook's distance
cooksd <- cooks.distance(model_multi)

# Plot Cook's distance
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Distance")
abline(h = 4 / nrow(dataset), col = "red", lty = 2)

