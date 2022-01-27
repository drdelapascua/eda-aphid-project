### Eda and Danielle - TM Aphid fieldwork data prep and LM

# Libraries ----
library(tidyverse)
library(dplyr)
library(nlme)
library(lme4)
library(MuMIn)
library(ggplot2)

# Set working directory ----
setwd("~/GitHub/eda-aphid-project")

# Pull data & clean ----
d <- read.csv("TM_aphid_herbiv_data.csv") 
d <- d[-c(29:61), ] #drops the 'NA' rows
d <- d[,-c(15)] #drops the last row

# Data visualization ----

# > looking at response variables ----

# question 1 - herbivory ~ aphid density
# categorical data
ggplot(d, aes(x = leaf_dmg)) +
  geom_bar()

ggplot(d, aes(x = stem_dmg)) +
  geom_bar()

ggplot(d, aes(x = flwr_dmg)) +
  geom_bar()

ggplot(d, aes(x = fruit_dmg)) +
  geom_bar()

#it would be interesting to see these ^ combined, for example, do the individuals that have one type of herbivory have the other, etc

# how do we plan to analyze this? we could try to combine, truncate, etc the above, or we could select one type of herbivory. 

#could combine all into one "herbivore pressure" function, which may assign 'points' to each type of herbivory. a = 1 point, b = 2. Add up all the values from leaves, stem, fruit, flowers, and have one continuous "herbivore pressure" variable. we may also want to reformat the data to be easier to work with.

# question 2 - aphid count ~ plant size
hist(x = d$aphid_count)
d$log_aphid <- log(d$aphid_count) %>%
  mutate(log_aphid = replace("-inf", 0)) #error here

# > looking at fixed effects ----

#question 1 fixed effect is the same as the response variable for q2 above
hist(x = d$aphid_count)


#question 2
hist(x = d$stem_thickness)
hist(x = d$height)

# Linear models ----

# > Question 1: herbivory ~ aphid density ----

# > Question 2: aphid density ~ plant size ----
mod.2 <- lm(aphid_count ~ height, data = d)
mod.log2 <- lm(log_aphid ~ height, data = d)

# Model diagnostics ----

# > 1 variance homogeneity ----
plot(mod.2, 1)

### here you want to see a starrynight, no variation

# > 2 normality of errors ----
plot(mod.2, 2) #shows a qq plot

###if all points fall on line, then normal, if big tail where falling off the line, not normal
hist(resid(mod.2))


# > 3 residual variance homogeneity ----
plot(resid(mod.2) ~ d$height) #violin shape is bad
boxplot(resid(mod.2) ~ d$height)

# > 4 plot predicted values ----
plot(aphid_count ~ height, data = d)
