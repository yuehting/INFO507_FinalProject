# INFO 507 prelilminary 
# caridiovasculary disease dataset

# Install tidyverse and arules packages
# install.packages("tidyverse")
# install.packages("arules")

# Load tidyverse and arules packages
library(tidyverse)
library(arules)
library(corrplot)
library(olsrr)
library(smotefamily)

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
cardio$blood_pressure <- as.factor(cardio$blood_pressure)

# Recreate the displayAllHistograms() function 
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") +
    facet_wrap (~key, scales = "free") +
    theme_minimal ()
}
# Call the displayAllHistograms() function, passing into mobilePhone as an 
# argument
displayAllHistograms(cardio)

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
corrplot(cor(cardio %>%
               keep(is.numeric)),
         method = "number",
         type = "lower")

# prediabetes
# cardio <- cardio %>%
#   mutate(prediabetes = ifelse(gluc == 2, 1, 0))
# cardio$prediabetes = as.logical(cardio$prediabetes)

# diabetes 
# cardio <- cardio %>%
#   mutate(diabetes = ifelse(gluc ==3, 1, 0))
# cardio$diabetes = as.logical(cardio$diabetes)

# cardio <- cardio %>%
#   mutate(normal_gluc = ifelse(gluc ==1, 1,0))
# cardio$normal_gluc = as.logical(cardio$normal_gluc)
# 
# cardio <- cardio %>%
#   mutate(normal_chole = ifelse(cholesterol == 1, 1, 0))
# cardio$normal_chole = as.logical(cardio$normal_chole)
# 
# cardio <- cardio %>%
#   mutate(above_chole = ifelse(cholesterol == 2, 1, 0))
# cardio$above_chole = as.logical(cardio$above_chole)
# 
# cardio <- cardio %>% 
#   mutate(WellAbove_chole = ifelse(cholesterol ==3, 1, 0))
# cardio$WellAbove_chole = as.logical(cardio$WellAbove_chole)

#cardio <- cardio %>%
  #mutate(normal_pressure = ifelse(blood_pressure == 1, 1, 0))
#cardio$normal_pressure = as.logical(cardio$normal_pressure)

#cardio <- cardio %>%
  #mutate(pre_hypertension = ifelse(blood_pressure == 2, 1, 0))
#cardio$pre_hypertension = as.logical(cardio$pre_hypertension)

# cardio <- cardio %>%
#   mutate(stage1_hypertension = ifelse(blood_pressure ==3, 1, 0))
# cardio$stage1_hypertension = as.logical(cardio$stage1_hypertension)
# 
# cardio <- cardio %>%
#   mutate(stage2_hypertension = ifelse(blood_pressure == 4, 1, 0))
# cardio$stage2_hypertension = as.logical(cardio$stage2_hypertension)

# cardio <- cardio %>%
#   mutate(hypertension_crisis = ifelse(blood_pressure == 5, 1, 0))
# cardio$hypertension_crisis = as.logical(cardio$hypertension_crisis)

# dataset for models
cardio2<- cardio %>%
  select(-id, -gender,-ap_hi,-ap_lo)
summary(cardio2)


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

# Check if we have a class imbalance issue in cardio
summary(cardioTraining$cardio)

# Generate the logistic regression model 
cardioModel <- glm(data = cardioTraining,
                        family = binomial,
                        formula = cardio ~ .)

# Display the logistic regression model results using the summary() function
summary(cardioModel)

# Use the model to predict outcomes in the testing dataset as described
cardioPrediction <- predict(cardioModel,
                            cardioTesting,
                            type = "response")

# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
cardioPrediction <- ifelse(cardioPrediction >= 0.5, 1, 0)
print(cardioPrediction)
# Generate a confusion matrix of predictions
ConfusionMatrix <- 
  table(cardioTesting$cardio, cardioPrediction)

print(ConfusionMatrix)

# Calculate the false positive rate, 130/(282+130)
# This predict that the customer would cancel service, but they did not
ConfusionMatrix[1, 2] / 
  (ConfusionMatrix[1, 1] + 
     ConfusionMatrix[1, 2])

# Calculate the false negative rate, 33/(33+93)
# This predict that the customer would not cancel service, but they did
ConfusionMatrix[2, 1] / 
  (ConfusionMatrix[2, 1] + 
     ConfusionMatrix[2, 2])

# Calculate the prediction accuracy by dividing the number of true positive and
# true negatives by the total amount of predictions in the testing dataset
predictiveAccuracy <- sum(diag(ConfusionMatrix)) / nrow(cardioTesting)
print(predictiveAccuracy)


