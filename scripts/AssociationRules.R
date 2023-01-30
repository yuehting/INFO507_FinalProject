# Load the tidyverse and arules libraries
library(tidyverse)
library(arules)

# Setting working directory 
setwd("~/*COMPLETE COURSE/INFO 507/cariovascular disease ")

cardio <- read.transactions(file = "cardio-cleaned.csv",
                            header = TRUE,
                            sep = ",")

# Display a summary of cardio
summary(cardio)

# Display the first free transactions from cardio
inspect(cardio[1:3])

# Convert the frequency values in cardio into a tibble called cardioFrequency
cardioFrequency <-
  tibble(items = names(itemFrequency(cardio)),
         Frequency = itemFrequency(cardio))

print(cardioFrequency)


# Display the 3 most frequently purchased items on the console
cardioFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)


# Generate the association rules model in an object called 
AssociationRules <- 
  apriori(cardio,
          parameter = list(
            support = 0.005, 
            confidence = 0.2, 
            minlen = 2))
summary(AssociationRules)

# Display the first 10 association rules 
inspect(AssociationRules[1:10])
# Sort the association rules by lift and view the top 10 
AssociationRules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()



