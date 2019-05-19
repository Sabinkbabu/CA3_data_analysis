# demo uses the dataset "women" in R
#  containing height and weight of 15 women

simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
# weight = -87.52 + 3.45 * height

plot(women$height, women$weight,
     xlab = "height (inches)",
     ylab = "weight (lbs)",
     main = "Scatter plot")
abline(simple_linear_model)     

summary(simple_linear_model)

# residuals  - provide a quick view of the distributions of the residuals,
# which by definition have a mean zero
# therefore the median should not be far from zero, and the min and max
# should be roughly equal in absolute value
# residual standard error (RSS), R^2 and f- statistics are metrics
# that are used to check how the model fits the data

# The standard error (SE) defines the accuracy of the beta coefficients
# for a given beta coefficients, the SE reflects how the coefficients
# varies under repeat sampling. It can be used to compute the CI and
# t statistic.

# the correlation coefficient measures the level of association between
# 2 variables and ranges from -1 (perfect neg correlation)
# to +1  (perfect pos correlation)
# A value close to 0 = weak relationship
# A low correlation (0.2<x<0.2) indicates that a lot of the variation 
# of the outcome (y) against a predictor (x) is unexplained and
# we should then look for better predictor variables

cor(women$height, women$weight)
confint(simple_linear_model)

install.packages("CARS")
library(CARS)
head(cars)

scatter.smooth(x = cars$speed,
               y = cars$dist,
               xlab = "Car Speed",
               ylab = "Distance",
               main = "Speed vs Distance")

# Boxplots will show outliers in the data
par((mfrow = c(1,2)) # divide graph are into 2 cols
boxplot(cars$speed, main = "speed",
        sub = paste("outlier rows :", boxplot.stats(cars$speed)$out))

boxplot(cars$dist, main = "distance",
        sub = paste("outlier rows :", boxplot.stats(cars$dist)$out))

# Skewness function to examine the normality

install.packages("e1071")
library(e1071)

# density plot for speed
plot(density(cars$speed), main = "density plot : speed",
     ylab = "frequency",
     sub = paste("skewness :", round(e1071::skewness(cars$speed, 2))))
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "density plot : dist",
          ylab = "frequency",
          sub = paste("skewness :", round(e1071::skewness(cars$dist, 2))))
polygon(density(cars$dist), col = "red")


cor(cars$speed, cars$dist)     

# Build the model on full data

linear_model <- lm(dist ~ speed, data = cars)
linear_model
summary(linear_model)


set.seed(200)
nrow(cars)
head(cars, 10)
# choose a random sample from 1:all records in cars DS, 80% of rows

random_sample <- sample(1:nrow(cars), 0.8 * nrow(cars))
random_sample

# model training data
training_data <- cars[random_sample,]
nrow(training_data)
#model testing data
test_data <- cars[-random_sample,]
nrow(test_data)

# build the model on the training data

lr_model <- lm(speed ~ dist, training_data)
lr_model
summary(lr_model)

distance_predicted <- predict(lr_model, test_data)
distance_predicted

actual_predicted <- data.frame(cbind(actuals = test_data$dist, 
                                     predicted = distance_predicted))
actual_predicted

corelation_accuracy <- cor(actual_predicted)
corelation_accuracy  

# min- max accuracy

min_max_accuracy <- mean(apply(actual_predicted, 1, min)/
                           apply(actual_predicted, 1, max))

# MAPE
mape <- mean(abs((actual_predicted$predicted - actual_predicted$actuals)) / actual_predicted$actuals)
min_max_accuracy
mape

#----------------------------

# use the state.x77 dataset we'll explore the relationship between
# murder rate and other charcteristics including population, illetracy,
# income and frost

# first step in multiple regression is to examine the relatioships
# among the variables 2 at a time
# the scatterplot matrix uses the car package

states <- as.data.frame((state.x77[, c("Murder", 
                                       "Population", 
                                       "Illiteracy", 
                                       "Income", "Frost")]))
cor(states)

library(car)
scatterplotMatrix(states, spread = FALSE,
            smoothScatter.args = list(lty = 2), 
            main = "scatter plot matrix")
# Lets fit the multiple linear regression model

multiple_linear_model <- lm(Murder ~ Population + 
                              Illiteracy + Income + 
                              Frost, data = states)
summary(multiple_linear_model)

# Multiple linear regression with interaction

str(mtcars)
multiple_linear_model <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(multiple_linear_model)

install.packages("effects")
library(effects)
plot(effect("hp:wt", multiple_linear_model,,
            list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)
# evaluating the statistical assumptions in regression analysis
# the most common ways to do this is to plot the object returned by lm
# this creates 4 charts that are useful for evaluating model fit
model <- lm(weight ~height, data = women)
# plot 4 charts in 2x2 output
par(mfrow = c(2, 2))
plot(model)

# diagnostic plot for the quadratic fit
model2 <- lm(weight ~ height + I(height ^2), data = women)
plot(model2)

# drop points 13 and 15 
new_model <- lm(weight ~ height + I(height ^2), data = women[-c(13, 15),])
plot(new_model)

multiple_linear_model <- lm(Murder ~ Population + 
                              Illiteracy + Income + 
                              Frost, data = states)
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(multiple_linear_model)
summary(gvmodel)

# create training and testing data for the states dataset
# select a random sample with 80/20 split
set.seed(200)
sample_records <- sample(1:nrow(states), 0.8 * nrow(states))
# model training data
training_data <- states[sample_records,]
# model testing data
testing_data <- states[-sample_records,]

# build the model on the training data
lr_model <- lm(Murder ~ Illiteracy, data = training_data)

# predict distance from testing data
murder_predicted <- predict(lr_model, testing_data)

# create actuals vs predicted data frame
actuals_predicted <- data.frame(cbind(actuals = testing_data$Murder,
                                      predicted = murder_predicted))
correlation_accuracy <- cor(actuals_predicted)
correlation_accuracy

# min-max accuracy
min_max_accuracy <- mean(apply(actuals_predicted, 1, min)/
                           apply(actuals_predicted, 1, max))

min_max_accuracy
# Mape

mape <- mean(actuals_predicted$predicted - actuals_predicted$actuals/
               actuals_predicted$actuals)
mape
