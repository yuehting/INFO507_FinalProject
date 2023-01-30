# Load the tidyverse and arules libraries
library(tidyverse)
library(arules)

# Setting working directory 
setwd("~/*COMPLETE COURSE/INFO 507/cariovascular disease ")

# Import the Cardiovascular disease csv file and called cardio
cardiodisease= read_csv(file = "cardio_dataset.csv",
                        col_names = TRUE,
                        col_types = "iiiiiiiiifffl")

summary(cardiodisease)

# drop missing data
cardio <- drop_na(cardiodisease)

# filter out records which has extremely large and low systolic and diastolic.
# filter out records is hypotension
cardio <- filter(.data = cardio,
                 ap_hi >= 90, ap_hi <=240,
                 ap_lo >= 60, ap_lo <200)
summary(cardio)
# age into years
cardio$age <- cardio$age / 365 

# drop height and weight 
cardio <- select(.data = cardio,
                 -height, -weight)

# distinguish systolic and diastolic into 5 categories: normal, prehypertension, 
# stage 1, stage 2, and hypertension crisis
cardio <- cardio %>%
  mutate(blood_pressure = "normal")
for (i in 1:length(cardio$ap_hi)) {
  if(cardio$ap_hi[i] >= 180 && cardio$ap_lo[i] >= 120 ){
    cardio$blood_pressure[i] <- "hypertension_crisis"
  } else if (cardio$ap_hi[i]>= 140 || cardio$ap_lo[i] >= 90) {
    cardio$blood_pressure[i] <- "stage_2_hypertension"
  } else if (cardio$ap_hi[i]>= 130 || cardio$ap_lo[i] >= 80) {
    cardio$blood_pressure[i] <- "stage_1_hypertension"
  } else if (cardio$ap_hi[i] >= 120 && cardio$ap_lo[i]<80){
    cardio$blood_pressure[i] <- "prehypertension"
  } else {
    cardio$blood_pressure[i] <- "normal"
  }
}
# new feature: glucose
cardio <- cardio %>%
  mutate(glucose = "normal")
  for (i in 1:length(cardio$gluc)) {
    if(cardio$gluc[i] ==1 ){
      cardio$glucose[i] <- "normal"
    } else if (cardio$gluc[i] == 2) {
      cardio$glucose[i] <- "prediabetes"
    } else {
      cardio$glucose[i] <- "diabetes"
    }
  }

# new feature: chole:
cardio <- cardio %>%
  mutate(chole = "normal")
for (i in 1 : length(cardio$cholesterol)) {
  if(cardio$cholesterol[i] == 1) {
    cardio$chole[i] <- "normal"
  } else if (cardio$cholesterol[i] ==2) {
    cardio$chole[i] <- "above"
  } else {
    cardio$chole[i] <- "well_above"}
}
summary(cardio)
# smoking 
cardio <- cardio %>%
  mutate(smoking = "nonsmoking")
for ( i in 1 : length(cardio$smoke)) {
  if(cardio$smoke[i] == 0) {
    cardio$smoking[i] <- "nonsmoking"
  } else {
    cardio$smoking[i] <- "smoking"
  }
}

# alcohol
cardio <- cardio %>%
  mutate (alcohol = "nondrink")
for (i in 1 : length(cardio$alco)){
  if (cardio$alco[i]==0) {
    cardio$alcohol[i] <- "nondrink"
  } else {
    cardio$alcohol[i] <- "drink"
  }
}

# Active 
cardio <- cardio %>%
  mutate (Active = "non")
for (i in 1 : length(cardio$active)) {
  if (cardio$active[i] == 0) {
    cardio$Active[i] <- "non"
  } else {
    cardio$Active[i] <- "active"
  }
}

# dataset for models
cardio2<- cardio %>%
  select(-gender,-ap_hi,-ap_lo, -gluc, -cholesterol,-smoke, -alco, -active)

print(cardio2)
write.csv(cardio2, file = 'cardio-cleaned.csv', row.names = FALSE)

cardio3 <- read_csv(file = "cardio-cleaned.csv")
cardio4 <- cardio3[1, ]
cardio4 <- cardio4 %>%
  mutate(itemID = "")
cardio4[nrow(cardio4) +1, ] <- c(cardio4$id, cardio4$age)

#create data frame
df <- data.frame("c1" = c(41, 42, 43, 44),
                 "c2" = c(45, 46, 47, 48),
                 "c3" = c(49, 50, 51, 52))
#add row
df[nrow(df) + 1,] <- c(10, 20, 30)
#print