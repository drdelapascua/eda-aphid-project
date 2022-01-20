### Eda and Danielle - TM Aphid fieldwork data prep and LM

# libraries----
library(tidyverse)
library(dplyr)
library(lubridate)

# set working directory ----
setwd("~/GitHub/eda-aphid-project")

# pull data & clean ----
d <- read.csv("TM_aphid_herbiv_data.csv")
d <- d[-c(29:61), ] #drops the 'NA' rows
d <- d[,-c(15)] #drops the last row

# > looking at response variables ----

# question 1 - herbivory ~ aphid #
# categorical data
ggplot(d, aes(x = leaf_dmg)) +
  geom_bar()

ggplot(d, aes(x = stem_dmg)) +
  geom_bar()

ggplot(d, aes(x = flwr_dmg)) +
  geom_bar()

ggplot(d, aes(x = fruit_dmg)) +
  geom_bar()

# how do we plan to analyze this? we could try to combine, truncate, etc the above, or we could select one type of herbivory. 

#could combine all into one "herbivore pressure" function, which may assign 'points' to each type of herbivory. a = 1 point, b = 2. Add up all the values from leaves, stem, fruit, flowers, and have one continuous "herbivore pressure" variable. we may also want to reformat the data to be easier to work with.

# question 2 - aphid count ~ plant size
hist(x = d$aphid_count)

# > looking at fixed effects ----

#question 1 fixed effect is the same as the response variable for q2 above
hist(x = d$aphid_count)

#question 2
hist(x = d$stem_thickness)
hist(x = d$height)

# Linear models

#Question 1: herbivory ~ 

#Question 2: