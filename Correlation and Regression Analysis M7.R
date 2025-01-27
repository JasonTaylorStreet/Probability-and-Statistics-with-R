# Task 1 Correlation Test
# Load the WHO.csv data set
life.who <- read.csv(file = "C:/Users/jasst/OneDrive/Desktop/William & Mary/Prob and Stats with R/data/WHO.csv")
#  1A-Life and Mortality Rate
#     Step 1:
#       H0: There is no relationship between Life and Mortality Rate (=)
#       HA: There is a relationship between Life and Mortality Rate (!=)
#     Steps 2 & 3:
cor.test(life.who$Life, life.who$MortalityRate)
#     Steps 4 & 5:
#       This test shows a strong negative correlation of -0.9571292 (< -.8)
#       which is significant (p<.05). This suggests that we reject the null
#       hypothesis and support the alternative that a correlation is present.
#  1B-Life and Underweight
#     Step 1:
#       H0: There is no relationship between Life and Underweight (=)
#       HA: There is a relationship between Life and Underweight (!=)
#     Steps 2 & 3:
cor.test(life.who$Life, life.who$Underweight)
#     Steps 4 & 5:
#       This test shows a moderately negative correlation of -0.6645974
#       (>-.8 and <-.5) which is significant (p<.05). This suggests that we
#       reject the null hypothesis and support the alternative that a
#       correlation is present.
#  1C-Life and Overweight
#     Step 1:
#       H0: There is no relationship between Life and Overweight (=)
#       HA: There is a relationship between Life and Overweight (!=)
#     Steps 2 & 3:
cor.test(life.who$Life, life.who$Overweight)
#     Steps 4 & 5:
#       This test shows a moderately positive correlation of 0.6233315
#       (>.5 and <.8) which is significant (p<.05). This suggests that we reject
#       the null hypothesis and support the alternative that a correlation is
#       present.
#  1D-Life and BMI
#     Step 1:
#       H0: There is no relationship between Life and BMI (=)
#       HA: There is a relationship between Life and BMI (!=)
#     Steps 2 & 3:
cor.test(life.who$Life, life.who$BMI)
#     Steps 4 & 5:
#       This test shows a moderately positive correlation of 0.5081365
#       (>.5 and <.8) which is significant (p<.05). This suggests that we reject
#       the null hypothesis and support the alternative that a correlation is
#       present.
#  1E-Life and Alcohol
#     Step 1:
#       H0: There is no relationship between Life and Alcohol(=)
#       HA: There is a relationship between Life and Alcohol(!=)
#     Steps 2 & 3:
cor.test(life.who$Life, life.who$Alcohol)
#     Steps 4 & 5:
#       This test shows a weak positive correlation of 0.4299928
#       (>.2 and <.5) which is significant (p<.05). This suggests that we reject
#       the null hypothesis and support the alternative that a correlation is
#       present.

################################################################################
# Task 2 Simple Regression
#  2A-Create 2 separate simple regressions
#    1) Life & BMI
#      Step 1:
#        H0: The slope of the line is equal to zero (=)
#        HA: The slope of the line is not equal to zero (!=)
#      Steps 2 & 3:
simple.bmi<-lm(Life ~ BMI, data=life.who, na.action = na.exclude)
summary(simple.bmi)
#      Steps 4 & 5:
#        A slope of 1.7563 (p<.05), indicates the slope is significantly
#        different from zero and thus rejecting the null.
#    2) Life & Air Pollution related to Child Deaths
#      Step 1:
#        H0: The slope of the line is equal to zero (=)
#        HA: The slope of the line is not equal to zero (!=)
#      Steps 2 & 3:
simple.air<-lm(Life ~ AirPollutionChildDeaths, data=life.who, na.action = na.exclude)
summary(simple.air)
#      Steps 4 & 5:
#        A slope of -0.0003003 (p<.05), indicates the slope is significantly
#        different from zero and thus rejecting the null.
#  2B-Predict Life expectancy (Life) when BMI is at 28.9
#       Result: 77.60706
predict(simple.bmi, data.frame(BMI=28.9))
#  2C-Predict Life expectancy (Life) when AirPollutionChildDeaths is at 0
#  #       Result: 72.40872
predict(simple.air, data.frame(AirPollutionChildDeaths=0))
#  2D-What could cause differences in your predictions when using these two
#     different variables and predicting data like that from the US?
#  Predictions are based on the data collected thus far. These predictions can
#  be be affected if there is real world changes that change the variances of
#  these variables. For example, war, drought, economic changes, etc...

################################################################################
# Task 3 Multiple Regerssion
#  3A
life.who.mortality<-life.who[,-c(1,2,14,15,16,17,18)]
#    Mortality to all other continuous variables
mortality.multiple<-lm(MortalityRate~., data=life.who.mortality)
summary(mortality.multiple)
#    Remove immunization due to p=.55185
mortality.multiple<-lm(MortalityRate~.-immunization, data=life.who.mortality)
summary(mortality.multiple)
#    Remove BMI due to p=.53395
mortality.multiple<-lm(MortalityRate~.-immunization-BMI, data=life.who.mortality)
summary(mortality.multiple)
#    Remove GDP due to p=.57015
mortality.multiple<-lm(MortalityRate~.-immunization-BMI-GDP, data=life.who.mortality)
summary(mortality.multiple)
#    Remove Overweight due to p=.267349
mortality.multiple<-lm(MortalityRate~.-immunization-BMI-GDP-Overweight, data=life.who.mortality)
summary(mortality.multiple)

#  3B-Interpret the final model
#    In the final model, it was found the best predictors for mortality rate
#  were the Underweight, Alcohol, Population, GDPperCapita,
#  AirPollutionChildDeaths, and PerWarnCigarettes variables (all variables with
#  p<.05). There was a positive correlation with Underweight, Alcohol, and
#  AirPollutionChildDeaths variables. The remaing were negativly correlated. All
#  of these variables makeup 51.44% of the explained variance.

#  3C-Predict mortality AFTER adding new data (vice at same time)
#    Result 190.5298
new.3Cdata<-data.frame(Underweight=4.7,	Overweight=48, BMI=26.7, Alcohol=7.55, GDP=1510084751, Population=95426, GDPperCapita=15825, immunization=0, AirPollutionChildDeaths=0, PerWarnCigarettes=0)
predict(mortality.multiple,new.3Cdata)
