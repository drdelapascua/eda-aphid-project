# EVE 298 - Linear mixed modelins in Ecology and Evolution 
# Danielle De La Pascua

###
### Lab 1 ----
###

#libraries
library(janitor)
library(dplyr)
'use three dots in files page to navigate to course files'

setwd("~/GitHub/linear-mized-modeling-eve298")

clams <- read.table("Datasets/Clams.txt")
clams <- clams %>% 
  row_to_names(row_number = 1)

### Lab 2 ----
clams$fMONTH <- as.factor(clams$MONTH)
str(clams)
levels(clams$fMONTH)
'linear model with categoriical predictor'
mod.cat <- lm(AFD ~ LENGTH + fMONTH, data = clams)
'linear model with month as continuous predictor'
mod.cont <- lm(AFD ~ LENGTH + MONTH, data = clams)

'model diagnostics'

#1 variance homogeneity
plot(mod.cat, 1)
#you want to see a starrynight, no variation, but this does not show this, it shows an arc (trumpet plot) -- log linearity
#usually this tests variance homogeneity

#2 normality of errors
plot(mod.cat, 2) #shows a qq plot
#if all points fall on line, then normal, if big tail where falling off the line
hist(resid(mod.cat))

#3 variance homogeneity
plot(resid(mod.cat) ~ clams$LENGTH) #violin is bad
boxplot(resid(mod.cat) ~ clams$fMONTH)

#4 plot predicted values

plot(AFD ~ LENGTH, data = clams)

summary(mod.cat) # B0, estimate of (intercept), Length estimate is the slope of that line (B1), etc

#colorcoding
plot(AFD ~ LENGTH, data = clams, type = "n")
points(AFD[fMONTH == "2"] ~ LENGTH[fMONTH == "2"], data = clams, col = "red") #only grab the AFD from month 2 by length at month 2, color in red

coef(mod.cat)[1] #coefficients from categorical model but only the first one

abline(a = coef(mod.cat)[1], b = coef(mod.cat)[2], col = "red")

# DO THIS FOR THE OTHER FIVE MONTHS # ----

'add in data & predicted values month 3'
points(AFD[fMONTH == "3"] ~ LENGTH[fMONTH == "3"], data = clams, col = "blue")
abline(a = coef(mod.cat)[1] + coef(mod.cat)[3], b = coef(mod.cat)[2], col = "blue")

#month 4
'add in data & predicted values month 3'
points(AFD[fMONTH == "4"] ~ LENGTH[fMONTH == "4"], data = clams, col = "purple")
abline(a = coef(mod.cat)[1] + coef(mod.cat)[4], b = coef(mod.cat)[2], col = "purple")

#month 9
'add in data & predicted values month 3'
points(AFD[fMONTH == "9"] ~ LENGTH[fMONTH == "9"], data = clams, col = "yellow")
abline(a = coef(mod.cat)[1] + coef(mod.cat)[5], b = coef(mod.cat)[2], col = "yellow")

str(clams)

#Validate and plot predicted values 
mod.ln <- lm(LNAFD ~ LNLENGTH + fMONTH, data = clams)

#Validate and plot predicted values - different slopes

mod.ln <- lm(LNAFD ~ LNLENGTH*fMONTH, + fmoth, data = clams)

#1 variance homogeneity
plot(mod.ln, 1)

#2 normality of errors
plot(mod.ln, 2)

#3 variance homogeneity
plot(resid(mod.ln))

#4 plot predicted values
plot(LNAFD ~ LNLENGTH, data = clams)

summary(mod.ln) # B0, estimate of (intercept), Length estimate is the slope of that line (B1), etc
plot(LNAFD ~ LNLENGTH, data = clams, type = "n")
points(LNAFD[fMONTH == "2"] ~ LENGTH[fMONTH == "2"], data = clams, col = "red") #only grab the AFD from month 2 by length at month 2, color in red

coef(mod.ln)[1] #coefficients from categorical model but only the first one

abline(a = coef(mod.ln[1], b = coef(mod.ln)[2], col = "red"))

'add in data & predicted values month 3'
points(LNAFD[fMONTH == "3"] ~ LNLENGTH[fMONTH == "3"], data = clams, col = "blue")
abline(a = coef(mod.ln)[1] + coef(mod.ln)[3], b = coef(mod.ln)[2], col = "blue")

#month 4
'add in data & predicted values month 3'
points(LNAFD[fMONTH == "4"] ~ LNLENGTH[fMONTH == "4"], data = clams, col = "purple")
abline(a = coef(mod.ln)[1] + coef(mod.ln)[4], b = coef(mod.ln)[2], col = "purple")

#month 9
'add in data & predicted values month 3'
points(LNAFD[fMONTH == "9"] ~ LNLENGTH[fMONTH == "9"], data = clams, col = "yellow")
abline(a = coef(mod.ln)[1] + coef(mod.ln)[5], b = coef(mod.ln)[2], col = "yellow")

####
#### LAB 3 ----
#### data exploration & guided model selection

loyn <- read.table("loyn.txt", header = T)
head(loyn)
tail(loyn)
str(loyn)

# > data exploration ----

'maybe we want graze as a categorical predictor'
loyn$fGRAZE <- as.factor(loyn$GRAZE)
str(loyn)

'lets do some data exploration!'

# response variable
hist(loyn$ABUND)
dotchart(loyn$ABUND)
dotchart(loyn$ABUND, color = loyn$fGRAZE)

# predictor variables
hist(loyn$AREA)
dotchart(loyn$AREA)
dotchart(log10(loyn$AREA))

hist(loyn$DIST)
dotchart(loyn$DIST)
dotchart(log10(loyn$DIST))

hist(loyn$LDIST)
dotchart(loyn$LDIST)
dotchart(log10(loyn$LDIST))

'all three variables are measures of size/distance which often follow log-linear relationships, you can see that log10 transforming them helps spread out little values and pull in big values'

pairs(loyn)
pairs(loyn[,2:7]) # subsetting dataframes [rows, columns]
pairs(loyn[c(2, 4, 6)])
pairs(loyn[,c("AREA", "DIST", "ABUND", "GRAZE")])

loyn$L.AREA <- log10(loyn$AREA)
loyn$L.DIST <- log10(loyn$DIST)
loyn$L.LDIST <- log10(loyn$LDIST)

pairs(loyn[,c("ABUND", "L.AREA", "L.DIST", "L.LDIST", "YR.ISOL", "ALT")])

boxplot(loyn$ABUND ~ loyn$fGRAZE)

library(ggplot2)

ggplot(loyn, aes(x = L.AREA, y = ABUND)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_grid(~fGRAZE)

'if you want to test an interaction between two fixed effects, like a categorical and a continuous - need to make sure that the spread of your continuous variable is roughly similar across all levels of the categorical'

# > anova v drop1 v summary ----

mod <- lm(ABUND ~ AREA + DIST + ALT + YR.ISOL + fGRAZE, data = loyn)

plot(mod, 1) # variance homogeneity
plot(mod, 2) # residual normality

plot(resid(mod) ~ loyn$AREA)
plot(resid(mod) ~ loyn$DIST)
plot(resid(mod) ~ loyn$ALT)

boxplot(resid(mod) ~ loyn$fGRAZE)

summary(mod)
anova(mod) # USES TYPE I SS - SEQUENTIAL 

# change order of predictor variables
mod2 <- lm(ABUND ~ fGRAZE + AREA + DIST + ALT + YR.ISOL , data = loyn)
anova(mod2)

'anova() uses Type I SS which are sequential and thus the order of fixed effects can completely change the interpretation!! 

Better to use Type III SS'


car::Anova(mod, type = "3")
car::Anova(mod2, type = "3")

# regular linear models
drop1(mod, test = "F")
# mixed models
drop1(mod, test = "Chisq")
anova.lme(mod, type = "marginal")

# > guided mixed model ----

rikz <- read.table("rikz.txt", header = T)
head(rikz)
str(rikz)

'you should do some data exploration'

dotchart(rikz$Richness)
dotchart(rikz$NAP)

rikz$fExp <- as.factor(rikz$Exposure)
str(rikz)

table(rikz$fExp)

'turn factor == 8 into 10 to make 2 levels'
rikz$fExp[rikz$fExp == "8"] <- "10"
rikz$fExp <- factor(rikz$fExp, levels = c(10,11))
str(rikz)

# >> random structure ----
library(nlme)

mod.full <- lme(Richness ~ NAP*fExp, 
                random = ~1|Beach, 
                data = rikz)

summary(mod.full)

mod.test <- gls(Richness ~ NAP*fExp, data = rikz)
anova(mod.full, mod.test)

mod.test2 <- lm(Richness ~ NAP*fExp, data = rikz)
anova(mod.full, mod.test2)

### 
# > guided mixed model ----

rikz <- read.table("rikz.txt", header = T)
head(rikz)
str(rikz)

'you should do some data exploration'

dotchart(rikz$Richness)
dotchart(rikz$NAP)

rikz$fExp <- as.factor(rikz$Exposure)
str(rikz)

table(rikz$fExp)

'turn factor == 8 into 10 to make 2 levels'
rikz$fExp[rikz$fExp == "8"] <- "10"
rikz$fExp <- factor(rikz$fExp, levels = c(10,11))
str(rikz)

# .. random structure ----
library(nlme)

mod.full <- lme(Richness ~ NAP*fExp, 
                random = ~1|Beach,
                method = "REML", 
                data = rikz)

summary(mod.full)

'removing beach to test significance'
mod.test <- gls(Richness ~ NAP*fExp, method = "REML", data = rikz)
anova(mod.full, mod.test)

mod.test2 <- lm(Richness ~ NAP*fExp, data = rikz)
anova(mod.full, mod.test2)


# .. fixed structure ------

mod.full <- lme(Richness ~ NAP + fExp + NAP:fExp , 
                random = ~1|Beach,
                method = "ML", 
                data = rikz)

mod.twoway <- lme(Richness ~ NAP + fExp, 
                  random = ~1|Beach,
                  method = "ML", 
                  data = rikz)

# testing the significance of the NAP x fExp interaction
# report LLR and p-value from here
anova(mod.full, mod.twoway)
'two-way interaction is significant, LLR = 4.6, p = 0.03'

# Assume two-way interaction is NOT significant

mod.full2 <- lme(Richness ~ NAP + fExp, 
                 random = ~1|Beach,
                 method = "ML",
                 data = rikz)

mod.nap <- lme(Richness ~ fExp, 
               random = ~1|Beach,
               method = "ML",
               data = rikz)

mod.exp <- lme(Richness ~ NAP , 
               random = ~1|Beach,
               method = "ML",
               data = rikz)

# test effect of nap
anova(mod.full2, mod.nap)
'very significant effect of nap, LLR = 22.67, p < 0.001'

# test effect of exposure
anova(mod.full2, mod.exp)
'very significant effect of exposure, LLR = 7.07, p = 0.008'

# get parameter estimates from here
mod.final <- lme(Richness ~ NAP + fExp + NAP:fExp, 
                 random = ~1|Beach,
                 method = "REML",
                 data = rikz)

summary(mod.start)


####
#### LAB 4 --------
#### model selection with owls


owls <- read.table("Owls.txt", header = T)
head(owls)
tail(owls)
str(owls)

# explore data

hist(owls$NegPerChick)
dotchart(owls$NegPerChick)
dotchart(owls$ArrivalTime)

boxplot(owls$NegPerChick ~ owls$Nest)
boxplot(owls$NegPerChick ~ owls$SexParent)
plot(owls$NegPerChick ~ owls$ArrivalTime)

coplot(NegPerChick ~ ArrivalTime|FoodTreatment, data = owls)
coplot(NegPerChick ~ ArrivalTime|SexParent, data = owls)

'data looks like good spread! no obvious outliers or unexpected patterns
might have problems with Response as it looks quite right skewed - log10 transform?'

# model selection
# . random structure

mod.ref <- lme(LogNeg ~ SexParent*FoodTreatment + ArrivalTime*SexParent, 
               random = ~1|Nest, 
               method = "REML",
               data = owls)

mod.rand <- gls(LogNeg ~ SexParent*FoodTreatment + ArrivalTime*SexParent, 
                method = "REML",
                data = owls)

anova(mod.ref, mod.rand)

'we need to include Nest anyway, but yes can see that it accounts
for significant portion of the variation'

# . fixed structure

mod.ref <- lme(LogNeg ~ SexParent + FoodTreatment + ArrivalTime + 
                 SexParent:FoodTreatment + ArrivalTime:SexParent, 
               random = ~1|Nest, 
               method = "ML",
               data = owls)


'first need to test both two-way interactions'

mod.sex.food <- lme(LogNeg ~ SexParent + FoodTreatment + ArrivalTime + 
                      ArrivalTime:SexParent, 
                    random = ~1|Nest, 
                    method = "ML",
                    data = owls)

mod.arrive.sex <- lme(LogNeg ~ SexParent + FoodTreatment + ArrivalTime + 
                        SexParent:FoodTreatment , 
                      random = ~1|Nest, 
                      method = "ML",
                      data = owls)

anova(mod.ref, mod.sex.food)
'Sex:Food interaction is NOT significant, LLR = 0.12, p = 0.73'

anova(mod.ref, mod.arrive.sex)
'Arrive:Sex interaction is NOT significant, LLR = 0.14, p = 0.71'

'so now can build new reference model that removes both of the two-way interactions'

mod.ref2 <- lme(LogNeg ~ SexParent + FoodTreatment + ArrivalTime,
                random = ~1|Nest, 
                method = "ML",
                data = owls)

'test main effect of Sex'
mod.sex <- lme(LogNeg ~ FoodTreatment + ArrivalTime,
               random = ~1|Nest, 
               method = "ML",
               data = owls)
anova(mod.ref2, mod.sex)

'test main effect of Food'
mod.food <- lme(LogNeg ~ SexParent  + ArrivalTime,
                random = ~1|Nest, 
                method = "ML",
                data = owls)
anova(mod.ref2, mod.food)

'test main effect of Arrival'
mod.arr <- lme(LogNeg ~ SexParent + FoodTreatment,
               random = ~1|Nest, 
               method = "ML",
               data = owls)
anova(mod.ref2, mod.arr)


# final model 

mod.ref3 <- lme(LogNeg ~ FoodTreatment + ArrivalTime,
                random = ~1|Nest, 
                method = "ML",
                data = owls)

'reported LLR for Food'
mod.food.final <- lme(LogNeg ~  ArrivalTime,
                      random = ~1|Nest, 
                      method = "ML",
                      data = owls)
anova(mod.ref3, mod.food.final) # LLR = 71.58, p < 0.001

'reported LLR for Arrival'
mod.arr.final <- lme(LogNeg ~ FoodTreatment,
                     random = ~1|Nest, 
                     method = "ML",
                     data = owls)
anova(mod.ref3, mod.arr.final) # LLR = 35.72, p <0.001

'significance of random effect'
mod.nest <- gls(LogNeg ~ FoodTreatment + ArrivalTime,
                method = "ML",
                data = owls)
anova(mod.ref3, mod.nest) # LLR = 30.57, p <0.001


mod.final <-lme(LogNeg ~ FoodTreatment + ArrivalTime,
                random = ~1|Nest, 
                method = "REML",
                data = owls)

#validate
plot(mod.final)
plot(resid(mod.final) ~ owls$Nest)
plot(resid(mod.final) ~ owls$FoodTreatment)
plot(resid(mod.final) ~ owls$ArrivalTime)

coplot(resid(mod.final) ~ ArrivalTime|FoodTreatment, data = owls)

'reported parameter estimates'
summary(mod.final)

'now that you know how to do this the real way, here is a sneaky bit of code'

anova.lme(mod.ref, type = "marginal")

### LAB 5 ----

# > nested vs crossed effects ----

whales <- read.table("Datasets/Cetaceans.txt", header = T)
str(whales)

#can i use individual or stains as random effects?
xtabs(~DolphinID + Species, data = whales)#tells how many you have of each comparison, ID has only a few obs for one category of each...
xtabs(~Stain + Location, data = whales) #completely crossed
xtabs(~ Species + Location, data = whales) #not completely crosses

'syntax for nested random effects'
library(nlme)
mod.start <- lme(Age ~ Stain*Location + Stain*Sex, 
                 random = ~1|Species/DolphinID,
                 data = whales)

'crossed effects - cannot be run in lme, need to use lmer!
make blieve that location is a random effect (its not really)'

library (lme4)
mod.cross <- lmer(Age ~ Stain*Sex + (1|Species/DolphinID), 
                  data = whales)
summary(mod.start)

# > repeatability ----

mod.start <- lme(Age ~ Stain*Location + Stain*Sex, 
                 random = ~1|Species/DolphinID,
                 data = whales)

'to estimate repeatability - variance in RE/total variance not explained by fixed effects'
'here we havetwo re, id and species'

'repeatability of species'

.770^2/(.770^2 + 5.559^2 + .835^2)
VarCorr(mod.start)
vcov <- as.numeric(VarCorr(mod.start))
vcov

rpt.species <- vcov[2]/(vcov[2] + vcov[4] + vcov[5]) #gets same number as abov equation


#interpreted: differences among species accounts for 1.8% of variation not explained by fixed effects - small

rpt.indv <- vcov[4]/(vcov[2] + vcov[4] + vcov[5]) 
rpt.indv

'both'
rpt.both<- (vcov[2]+vcov[4])/(vcov[2] + vcov[4] + vcov[5])
rpt.both

install.packages("rptR")

# > R-squared ----

rikz <- read.table("Datasets/RIKZ.txt", header = T)
rikz$fExp <- rikz$Exposure
rikz$fExp[rikz$fExp == 8] <- 10
rikz$fExp <- factor(rikz$fExp, levels = c(10,11))

mod.final <- lme(Richness ~ NAP + fExp,
                 random = ~1|Beach,
                 data = rikz)

summary(mod.final)

fixef(mod.final)
model.matrix(mod.final, data = rikz) # predictor values

'y = B0 
    + B1*NAP 
    + B2*Exp'
fix.matrix <- model.matrix(mod.final, data = rikz)
#this is the linear model, first term is the intercept, then beta one multiplied by value of predictor, and beta two multiplied by the actual value of the predictor
fixed <- (fixef(mod.final)[1] + 
            fixef(mod.final)[2]*fix.matrix[ ,2] +
            fixef(mod.final)[3]*fix.matrix[ ,3])

varF <- var(fixed) #variance in fixed effects

var.est <- as.numeric(VarCorr(mod.final))
varR <-var.est[1] #variance in RE - aka variance among beaches
varE <- var.est[2] # residual variance

'marginal R2 - the variance explained by fixed effects only'
varF/(varF + varR + varE)
#R2 is ~ .4899 - so fixed effects is explaining about half the variation in the data, which is pretty good for ecology studies

'conditional R2 - variance explained by fixed and random effects'
(varF+varR)/(varF + varR + varE)

#you should report two R2 for your models

#remember, R2 is effect size

#R2 library
library(MuMIn)
r.squaredGLMM(mod.final)

varNAP <- var(fixef(mod.final)[2]*fix.matrix[,2])
varExp <- var(fixef(mod.final)[3]*fix.matrix[,3])

'variance explained by NAP'
varNAP/(varF + varR + varE)

'variance explained by Exp'
varExp/(varF + varR + varE)

####
#### Lab 6 ----
####

rikz <- read.table("Datasets/RIKZ.txt", header = T)
rikz$fExp <- rikz$Exposure
rikz$fExp[rikz$fExp == 8] <- 10
rikz$fExp <- factor(rikz$fExp, levels = c(10,11))

mod.intercept <- lme(Richness ~ NAP + fExp, random = ~1|Beach,
                     data = rikz)

mod.slope <- lme(Richness ~ NAP + fExp, random = ~1 + NAP|Beach,
                 data = rikz)
summary(mod.intercept)
summary(mod.slope) #variance in slopes and intercept between slopes. negatively correlated, so beaches with a higher overall intercept have a more negative slope
#if running RE have to use REML, like above
anova(mod.intercept, mod.slope)
#in this case, including random slopes did not make the model better

'you can pull out the estimate (BLUPs) of th eintercepts and slopes using this'
ranef(mod.intercept)
fixef(mod.intercept)

var(ranef(mod.slope)[,1])

# > standardizing ----

str(rikz)
mod.raw <- lme(Richness ~ NAP + fExp, random = ~ 1|Beach, data = rikz)

'scale will divide everything by 1 sd, center will subtract mean from everything if you do both you get a z-score'
rikz$NAP.scale <- scale(rikz$NAP, scale = T, center = T)

rikz.scale.2sd <- (rikz$NAP - mean(rikz$NAP)) / (2*sd(rikz$NAP))
dotchart(rikz$NAP)
dotchart(rikz$NAP.scale)

mod.scale <- lme(Richness ~ rikz.scale.2sd + fExp, random = ~1|Beach,
                 data = rikz)
summary(mod.scale)

# > centering ----

clams <- read.table("Datasets/clams.txt", header = T)
clams$LNLENGTH.cen <- scale(clams$LNLENGTH, scale = F, center = T)
plot(LNAFD ~ LNLENGTH, data = clams)
plot(LNAFD ~ LNLENGTH.cen, data = clams)

#### LAB 7 ----

# > variance heterogeneity ----
setwd("~/GitHub/linear-mized-modeling-eve298")

squid <- read.table("Datasets/Squid.txt", header = T)
head(squid)
str(squid)
squid$fMONTH <- as.factor(squid$MONTH)
hist(squid$Testisweight)

dotchart(squid$DML)
hist(squid$DML)
boxplot(Testisweight ~ fMONTH, data = squid)

'run model with normal assumptions'

mod.norm <- gls(Testisweight ~ DML*fMONTH, data = squid)
plot(mod.norm, 1) #plot we had in the notes - if you see this fix it before anything else.

'can we figure out if variance het is related to some predictor?'
plot(resid(mod.norm) ~ squid$DML) #variance in body side within residuals
boxplot(resid(mod.norm) ~ squid$fMONTH) #variance by month in residuals


'fixed variance structure'
require(nlme)

mod.fix <- gls(Testisweight ~ DML*fMONTH, 
              weights = varFixed(~DML),
              data = squid) #if we had a random factor, do it in lme, but use gls for only fixed effects. lmer does not allow you to break variance/covariece structure

summary(mod.fix)
anova(mod.norm, mod.fix)

'identity variacne structure'
mod.ident <- gls(Testisweight ~ DML*fMONTH,
                 weights = varIdent(form = ~1|fMONTH),
                 data = squid)
summary(mod.ident)
AIC(mod.norm, mod.fix, mod.ident)

'power variance structure'
mod.power <- gls(Testisweight ~ DML*fMONTH,
                 weights = varPower(form = ~DML),
                 method = "ML",
                 data  = squid)
summary(mod.power)
AIC(mod.norm, mod.fix, mod.ident, mod.power)

mod.power2 <- gls(Testisweight ~ DML + fMONTH,
                 weights = varPower(form = ~DML),
                 method = "ML",
                 data  = squid)

anova(mod.power, mod.power2)
# AIC score takes into account that there is a difference in df

# > temporal auto-correlation ----

hawaii <- read.table("Datasets/Hawaii.txt", header = T)
head(hawaii)
str(hawaii)

'data exploration'
hist(hawaii$Moorhen.Kauai)#right skewed, count data
dotchart(hawaii$Moorhen.Kauai) # lots of little numbers, a few really big numbers
hist(hawaii$Rainfall)
dotchart(hawaii$Rainfall)

'initial model'
require(nlme)
mod1a <- gls(sqrt(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
            na.action = na.omit) #cannot test for an interaction 
plot(mod1) #trumpet shape, not good!! classic autocorrelation pattern

mod1b <- gls(log10(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
             na.action = na.omit)
plot(mod1b)

mod.cs <- gls(sqrt(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
              correlation = corCompSymm(form = ~Year),
              na.action = na.omit) #making compound signature according to year

summary(mod1)
summary(mod.cs) # all data points across all years, correlated with each other by Rho (this is essentially zero)
AIC(mod1a, mod.cs) #no difference, omit mod.cs because it is not improved

acf(resid(mod1a)) #incorrect because we have missing data, need to know if have missing values in your data. also saying that timesteps are autocorrelated above and below the blue dotted line

'we have missing values in response so need to re-insert these into the residuals'

'use this for your model t see if it correcting the bars'
E <- resid(mod1a, type = "normalized") # our residuals
I <- !is.na(hawaii$Moorhen.Kauai) #vector that says missing values at a data point
Efull <- NA #combining above, first make empty vector
Efull[I] <- E #everywhere there is a true value, it will put in the resid value, wherver there is none it leaves it as NA
Efull
acf(Efull, na.action = na.pass) #the plots look extrodinarily similar, but some differences. only three missing data points

'same thing for cs model'
E <- resid(mod.cs, type = "normalized") # our residuals
I <- !is.na(hawaii$Moorhen.Kauai) #vector that says missing values at a data point
Efull <- NA #combining above, first make empty vector
Efull[I] <- E #everywhere there is a true value, it will put in the resid value, wherver there is none it leaves it as NA
Efull
acf(Efull, na.action = na.pass)

'AR1'
mod.ar <- gls(sqrt(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
              correlation = corAR1(form = ~Year),
              na.action = na.omit)
summary(mod.ar) #autoregressive parameter pretty high

E <- resid(mod.ar, type = "normalized") # our residuals
I <- !is.na(hawaii$Moorhen.Kauai) #vector that says missing values at a data point
Efull <- NA #combining above, first make empty vector
Efull[I] <- E #everywhere there is a true value, it will put in the resid value, wherver there is none it leaves it as NA
Efull
acf(Efull, na.action = na.pass)
AIC(mod1a, mod.cs, mod.ar)
#AR1 is the best!

"ARMAs"
#AR1 has 1 p and 0 q's
#ARMA(2,0)
mod.arma2 <- gls(sqrt(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
              correlation = corARMA(c(0.2, 0.2), p=2, q=0), #estimate P's and Qs', starting values, how its going to fit phi and rho params
              na.action = na.omit)
summary(mod.arma2) #phi 1 and ph2 are estimates of relationship between e's - which are the timepoint indicators multiplied by a residual - this is what the arma model is doing
AIC(mod.ar, mod.arma2)

#ARMA(2,2)
mod.arma4 <- gls(sqrt(Moorhen.Kauai) ~ Year + Rainfall, data = hawaii,
                 correlation = corARMA(c(0.2, 0.2, 0.3, -0.3), p=2, q=2), #estimate P's and Qs', starting values, how its going to fit phi and rho params
                 na.action = na.omit)

AIC(mod.ar, mod.arma2, mod.arma4)

E <- resid(mod.arma4, type = "normalized") # our residuals
I <- !is.na(hawaii$Moorhen.Kauai) #vector that says missing values at a data point
Efull <- NA #combining above, first make empty vector
Efull[I] <- E #everywhere there is a true value, it will put in the resid value, wherver there is none it leaves it as NA
Efull
acf(Efull, na.action = na.pass)
