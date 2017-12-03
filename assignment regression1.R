# load packages
library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl) # for scatter3d


##############
#  model 1   #
##############
#data
mydata = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

### check data set for invalid data (e.g. coding errors)
# descriptive statistics
summary(mydata)


# according to the summary, there is an participant whose sex is "3", and an participant whose mindfulness value is negative. 
# use which function to find these participants and get rid of them
which(mydata$sex=="3")
which(mydata$mindfulness<0)
mydata1=mydata[-c(15,24),]
summary(mydata1)
describe(mydata1)
#mydata1$sex=factor(mydata1$sex,levels=c(0,1),labels=c("female","male"))


# plot the relationship of pain and sex
# sex is a factor type vector 
plot(mydata1$pain ~ factor(mydata1$sex))
plot(mydata1$pain~mydata1$age)
abline(lm(pain ~ age, data = mydata1))

## model 1
mod_pain1 = lm(pain ~ sex + age, data = mydata1)  
mod_pain1  #pain=8.460+0.166sex-0.087age
summary(mod_pain1) ### (F(2,155)=9.947, p<0.001), and the adjust r^2 equals 0.10232
#AIC
AIC(mod_pain1) # AIC(mod_pain1) = 553.0365 (AIC is used to compare different models, the lower AIC, the better the model is.)
### Which predictor had the most unique information added to the model?
lm.beta(mod_pain1)  #standardized the beta value for each predictor to be able to compare them
#the 95% CI 
confint(mod_pain1) 


###########
# model 2 #
###########

mod_pain2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                , data = mydata1) # pain = 2.304 + 0.119sex - 0.061age - 0.008anxiety + 0.086pain catastrophizing  + 0.001cortisol_serum + 0.674cortisol_saliva - 0.187mindfulness
summary(mod_pain2)  # (F(7,150)= 25.79, p<0.001), and the adjust r^2 equals 0.525
AIC(mod_pain2)  #AIC = 457.284

lm.beta(mod_pain2)  #cortisol_saliva was most influencial predictor
#the 95% CI 
confint(mod_pain2) 



### Comparing models
#anova
anova(mod_pain1, mod_pain2)
# or compare with AIC
# smaller AIC means more information
# if the difference in AIC of the two models is smaller than 2, they contain
# roughly the same information about the outcome
AIC(mod_pain1)
AIC(mod_pain2)




### Model diagnostics

# checking for influential outliers
plot(pain ~ factor(sex), data = mydata1)
plot(pain ~ age, data = mydata1)
plot(mod_pain1, which = 4)  #cook's distance
plot(mod_pain1, which = 5)  #residuals vs leverage
plot(mod_pain2, which = 4)  #cook's distance
plot(mod_pain2, which = 5)  #residuals vs leverage


## checking assumptions
# normality assumption
# QQ plot
plot(mod_pain1, which = 2)
plot(mod_pain2, which = 2)
# skew and kurtosis
describe(residuals(mod_pain1))
describe(residuals(mod_pain2))
# histogram
hist(residuals(mod_pain2), breaks = 20)


## linearity assumption
# predicted values against actual values
pred <- predict( object = mod_pain2 )
plot( x = pred, y = mydata1$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_pain2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_pain2)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_pain2, which = 3)
ncvTest(mod_pain2)
# multicollinearity
vif(mod_pain2)

# do same for model 1
pred <- predict( object = mod_pain1 )
plot( x = pred, y = mydata1$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
plot(mod_pain1, which = 1)
residualPlots(mod_pain1)
plot(mod_pain1, which = 3)
ncvTest(mod_pain1)
vif(mod_pain1)













