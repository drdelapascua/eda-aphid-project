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

# looking at response variables


# looking at fixed effects

