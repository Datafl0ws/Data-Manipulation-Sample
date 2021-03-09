# setting the working directory
setwd("C:/Users/MainUser/Documents/week7")

#saving the data
save.image("C:/Users/Mainuser/Documents/week7/week7.RData")

install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
#Loading packages to read, write, and manipualte data
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
# loading the data into a df object
pfizer <- read_csv("pfizer.csv")
fda <- read_csv("fda.csv")

# view structure of data
str(pfizer)

# viewing the first five rows of data
head(pfizer)
head(fda)
# viewing a specified number of rows
head(pfizer, n = 10)
head(fda, n = 10)

sum(is.na(pfizer))

# Just a sample run through showing how to convert total to numberic
pfizer$total <- as.numeric(pfizer$total)

summary(pfizer)

#counting NAs by column
na_counts <- pfizer %>%
  mutate(counts = sum(is.na(pfizer)))
head(na_counts$counts) # we can see that there is 411 NA value across our data frame

# omitting NAs in a simple manner
pfizer <- na.omit(pfizer)
sum(is.na(pfizer)) # you can see as a result we now have no NAs

#removing and replacing NAs with the median value if needed
pfizer <- pfizer %>%
  mutate()

## SECTION ON MANIPULATING AND ANALYZING DATA
pfizer_sorted <- pfizer %>%
  arrange(desc(cash))
head(pfizer_sorted)

# sorting by doctor last name
pfizer_doctor_sorted <- pfizer %>%
  arrange(desc(last_name))
head(pfizer_doctor_sorted)

# selecting a subset of the data by isolating a state into a new object
pfizer_GA <- pfizer %>%
  filter(state == 'GA')
pfizer_GA

# doing a combination of manipulation steps in one pipe
pfizer_manipulated <- pfizer %>%
  select(first_name, last_name, city, state, category, cash) %>%
  filter(state == 'GA' & city == 'ATLANTA') %>% #Cap sensitive matters
  arrange(desc(cash)) %>%
  group_by(last_name, first_name) %>%
  mutate(total = sum(cash))

head(pfizer_manipulated)

