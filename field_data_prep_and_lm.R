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

#plotting relationship
ggplot(data = d, aes(x = leaf_dmg, y = aphid_count)) +
  geom_boxplot()



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
mod.h <- lm(aphid_count ~ leaf_dmg, data = d)
summary(mod.h)
anova(mod.h)

mod.stem <- lm(aphid_count ~ stem_dmg, data = d)
summary(mod.stem)
# > Question 2: aphid density ~ plant size ----

mod.2 <- lm(aphid_count ~ height, data = d)


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

# aphid_count ~ size metrics

# aphid ~ height

### can use base R
plot(aphid_count ~ height, data = d)
plot(aphid_count ~ stem_thickness, data = d)
plot(height ~ stem_thickness, data = d)
### ... or ggplot
ggplot(data = d, aes(x = height, y = aphid_count)) + 
  geom_point() +
  stat_smooth(method = "lm") #here, looks like we do not have a linear distribution. Looks like a quadratic curve

# aphid count ~ thickness
ggplot(data = d, aes(x = stem_thickness, y = aphid_count)) + 
  geom_point() +
  stat_smooth(method = "lm")#same here, medium plants have the most aphids

# > using a quadratic 

# Model Selection ----

#playing around, mega-model
lm.all <- lm(aphid_count ~ fruit_count + height + stem_thickness, data = d)
summary(lm.all)
plot(lm.all, 1)
plot(lm.all, 2) #the high residual-quantile value from this qq plot seems like the model is overestimating aphid count at high fixed affect values  - we need a different dist!

# LM with quadratic distribution ----

#model without quadratic

mod.2 <- lm(aphid_count ~ height, data = d)
summary(mod.2)

#midel with quadratic

mod.q <- lm(aphid_count ~ height + height^2, data = d)
summary(mod.q)
anova(mod.q)

plot(mod.q, 1)
plot(mod.q, 2)

# Other plots for data exploration ----

ggplot(data = d, aes(x = aphid_count, y = fruit_count)) +
  geom_point() +
  stat_smooth(method = "lm") 
#here, we would think more aphids would lead to less fruit. the ggplot is not consistent with this. 

# are size and fruit number correlated?
ggplot(data = d, aes(x = fruit_count, y = height)) +
  geom_point() +
  stat_smooth(method = "lm")


#yes, size and fruit number are correlated. So, we should include size as a covariate if we want to assess whether this affects fitness.

# lm with aphid_count ~ size & fitness
lm.3<- lm(aphid_count ~ height + fruit_count, data = d)
summary(lm.3) #super low r2, which mean our model is explaining a very small part of the variation

plot(lm.3, 1) # variance homogeneity 
plot(lm.3, 2) # residual normality 

anova(lm.3)