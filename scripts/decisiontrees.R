library(tidyverse)
library(rpart)
library(rpart.plot)
# Maptree library can change the font size in decision trees
#install.packages("maptree")
library(maptree)

# Setting working directory 
setwd("~/*COMPLETE COURSE/INFO 507/cariovascular disease ")

# Import the Cardiovascular disease csv file and called cardio
cardiodisease= read_csv(file = "cardio_dataset.csv",
                        col_names = TRUE,
                        col_types = "iifiiiiffllll")

summary(cardiodisease)

# drop missing data
cardio <- drop_na(cardiodisease)

# filter out records which has extremely large and low systolic and diastolic.
# filter out records is hypotension
cardio <- filter(.data = cardio,
                 ap_hi >= 90, ap_hi <=370,
                 ap_lo >= 60, ap_lo <=360)
summary(cardio)
# age into years
cardio$age <- cardio$age / 365 

# drop height and weight 
cardio <- select(.data = cardio,
                 -height, -weight)

# distinguish systolic and diastolic into 5 categories: normal, prehypertension, 
# stage 1, stage 2, and hypertension crisis
cardio <- cardio %>%
  mutate(blood_pressure = 1)
for (i in 1:length(cardio$ap_hi)) {
  if(cardio$ap_hi[i] >= 180 && cardio$ap_lo[i] >= 120 ){
    cardio$blood_pressure[i] <- 5
  } else if (cardio$ap_hi[i]>= 140 || cardio$ap_lo[i] >= 90) {
    cardio$blood_pressure[i] <-4
  } else if (cardio$ap_hi[i]>= 130 || cardio$ap_lo[i] >= 80) {
    cardio$blood_pressure[i] <- 3
  } else if (cardio$ap_hi[i] >= 120 && cardio$ap_lo[i]<80){
    cardio$blood_pressure[i] <- 2
  } else {
    cardio$blood_pressure[i] <- 1
  }
}

# dataset for models
cardio2<- cardio %>%
  select(-id, -gender,-ap_hi,-ap_lo,)

# Spilt data into training and testing
# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process. 
set.seed(203)

# Create a vector of 75% randomly sample rows from the orighinal dataset
sampleSet <- sample(nrow(cardio2),
                    round(nrow(cardio2) * 0.75),
                    replace = FALSE)
# Put the records from the 75% sample into mobilePhoneTraining
cardioTraining <- cardio2[sampleSet, ]
# Put the records from the 25% sample into mobilePhoneTesting
cardioTesting <- cardio2[-sampleSet, ]

# Generate the decision tree model to predict cardio based on the other
# variables in the dataset. Use 0.01 as the complexity parameter
DecisionTreeModel <- rpart(formula = cardio ~ .,
                           method = "class",
                           cp = 0.001,
                           data = cardioTraining)

# Display the DecisionTreeModel in the console
rpart.plot(DecisionTreeModel)

# Predict the classes for each record in the testing dataset
Prediction <- predict(DecisionTreeModel,
                      cardioTesting,
                      type = "class")

# Display Prediction on the console
print(Prediction)

# Evaluate the model by forming a confusion matrix
ConfusionMatrix <- table(cardioTesting$cardio, 
                         Prediction)

# Display the confusion matrix on the console
print(ConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable called
# predictiveAccuracy
predictiveAccuracy <- sum(diag(ConfusionMatrix)) /
  nrow(cardioTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)

# Create a new decision tree model using 0.007 as the complexity parameter 

DecisionTreeModel2 <- rpart(formula = cardio ~ .,
                            method = "class",
                            cp = 0.0008,
                            data = cardioTraining)

# Display the decision tree visualization
rpart.plot(DecisionTreeModel2,
           tweak = 1.2)

# Calculate its predictive accuracy
Prediction2 <- predict(DecisionTreeModel2,
                       cardioTesting,
                       type = "class")
print(Prediction2)

# Evaluate the model by forming a confusion matrix
ConfusionMatrix2 <- table(cardioTesting$cardio,
                          Prediction2)

# Display the confusion matrix on the console
print(ConfusionMatrix2)

# Calculate the model predictive accuracy and store it into a variable called
# predictiveAccuracy
predictiveAccuracy2 <- sum(diag(ConfusionMatrix2)) /
  nrow(cardioTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy2)


