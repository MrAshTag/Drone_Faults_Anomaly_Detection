

# IV. ANALYSIS


library("gmodels")
library("tidyr")
library("dplyr")
library("readxl")



# Stepwise Regression


drone_diagnostics <- read_xlsx('/Users/bekimbaya/Documents/Jean/woz-u/Program Final Project/drone_diag_data.xlsx')
View(drone_diagnostics)


# Creating a linear model for potential predictors in the dataset with the function FitAll

FitAll = lm(outcomeR ~ ., data = drone_diagnostics)

summary(FitAll)

#According to the outcome of this model, these predictors are responsible for 29% of the response variable

# We can see that the overall p-value at the bottom is pretty significant but our goal is to determine
# The most significant predictors.


# DOING A BACKWARD ELIMINATION with step() function

step(FitAll, direction = 'backward')

#--> The model with the lowest AIC (Akaike Information Criteria)

# --> comparing all the AIC's our best model for this dataset,
#  the best model is comprised of Rx total bytes, Tx speed, Tx total bytes,
#  Received packets, Rx speed, EPV (m), Satellites visible, and heartbeats since startup

# The coefficients are not going to mean much since our response variables was orginally categorical and was recoded to a numeric variable



# LET's CREATE A MODEL WITH JUST THE VARIABLES RETAINED, to see if this would be a better model

fitsome = lm(outcomeR ~ `Rx total bytes` + `Tx total bytes` + `Received packets` + `Rx speed` + `EPV (m)` + 
               `Satellites visible` + `Tx speed` + `Heartbeats since startup`, data = drone_diagnostics)

summary(fitsome)
#According to this model, these predictors are reponsible for 27.9% of the outcome, our p-value is significant, which shows that is a good model

# 27.9% impact might not be enough for a machine learning model to be able to predict faults and anomalies with high accuracy

# ---> Since we used only continuous variables for this model, the remaining 63% influence on the response variable can be 
#  causded by the left out categorical variable.


# The next step would be to used recoded categorical variables to find  better model



