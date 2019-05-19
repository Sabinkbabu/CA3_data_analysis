# Sabin K babu
# L00144505
# MSc in big data analytics
# CA3 - Data Analysis
#----------------------------------------------------------------------------------------------------------------------


# Loading the data to R frame

rainfall_data <- read.csv('rainfall_data.csv', header = TRUE)

# Adding appropriate attribute titles
column_names <- c("Date", "Indicator", "Precipitation", 
                  "Indicator1", "Temperature", 
                  "Indicator2", "Wet bulb temperature", 
                  "Dew point temperature", "Vapour pressure", 
                  "Relative humidity", "Mean sea level pressure",
                  "Indicator3", "Mean wind speed", "Indicator4",
                  "Predominant wind direction", "Synop code for present weather",
                  "Synop code for past weather", "Sunshine duration",
                  "Visibility", "Cloud height", "Cloud amount")
colnames(rainfall_data) <- column_names

# To check the structure of the data
str(rainfall_data)

# Adding NA to empty attribute values
rainfall_data[rainfall_data == ""] <- NA

# Finding the sum and mean of missing values
sapply(rainfall_data, function(missing_data) sum(is.na(missing_data)))
sapply(rainfall_data, function(mean_missing_values) mean(is.na(mean_missing_values)))

# Removing the null values from
# the attributes because 
# when we use plot() function for normality 
# it will not take null values

rainfall_data <- subset(rainfall_data, !is.na(Precipitation), select = )
sum(is.na(rainfall_data$Precipitation))

rainfall_data <- subset(rainfall_data, !is.na(Temperature), select = )
sum(is.na(rainfall_data$Temperature))


# we can study the impact of the parent distribution
# of any sample data, by using quantile plots.
# Q-Q plot to check the normality

qqnorm(rainfall_data$`Temperature`)
qqline(rainfall_data$`Temperature`, col = "red")

qqnorm(rainfall_data$`Precipitation`)
qqline(rainfall_data$`Precipitation`, col = "red")

# Skewness function to check the normality

install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))

plot(density(rainfall_data$Precipitation), main = "density plot : Precipitation",
     ylab = "frequency",
     sub = paste("skewness :", round(e1071::skewness(rainfall_data$Precipitation, 2))))
polygon(density(rainfall_data$Precipitation), col = "red")

plot(density(rainfall_data$Temperature), main = "density plot : Temperature",
     ylab = "frequency",
     sub = paste("skewness :", round(e1071::skewness(rainfall_data$Temperature, 2))))
polygon(density(rainfall_data$Temperature), col = "red")

# Checking the skewness and kutosis
# of distributed data
# the skewness and kurtosis is available in moments package
install.packages("moments")
library(moments)

skewness(rainfall_data$Temperature)
kurtosis(rainfall_data$Temperature)

# the result showing that the distribution 
# is skewed slighlty to the left and
# kurtosis shows that it is platykurtic
# skewness and kurtosis gives an idea of distribution


# Applying shapiro test to check normality
shapiro.test(rainfall_data$Temperature)

# Formal test to check the normality
# Perform the Anderson-Darling test 
# for the composite hypothesis of normality

install.packages("nortest")
library(nortest)
ad.test(rainfall_data$Temperature)

# p value tells about the chances 
# the data sample comes from normal distribution 
# The p value is lesser than .05 here 
# which shows that the data is not normally distributed

library(pwr)
library(dplyr)

# Here we examining two attributes
# Precipitation Amount and Air Temperature 
# trying to find out the realtionship between two variables
# so will use sprearmans correaltion test as the data is non parametric
# here the effect size is assuming as the medium 
# which is r = 0.3 and finding
# how many samples we need to continue the process
# for that we are doing power test
power_analysis1 <- pwr.r.test(n = NULL, 
                             r = 0.3, 
                             sig.level = 0.05, 
                             power = 0.90, 
                             alternative = "two.sided")
power_analysis1
# From the result it 
# is understood that we need 112 samples 
# from each group inorder to find an effect
# size of 0.3 (medium) with 95% certainity and 10% chance of concluding that diffrence exists
# when in fact, it doesn't
plot(power_analysis1)

power_analysis2 <- pwr.r.test(n = NULL, 
                             r = 0.1, 
                             sig.level = 0.01, 
                             power = 0.95, 
                             alternative = "two.sided")
power_analysis2
# From the result it is understood that we need 1772 samples 
# from each group inorder to find an effect
# size of 0.1 (small) with 99% certainity and 5% chance of concluding that diffrence exists
# when in fact, it doesn't
plot(power_analysis2)

cohen.ES(test = "r", size = "medium")

# Spearman's rank-order correlation is the nonparametric version 
# of the Pearson product-moment correlation
# Spearman's correlation coefficient, (ρ/rho) measures the strength
# and direction of association between two ranked variables

res <- cor.test(rainfall_data$Precipitation, rainfall_data$Temperature,
                method = "spearman", exact = FALSE)
res

# It is understood that the p value is lesser than .01 
# so we can reject the null hypothesis
# and there exists alternative hypothesis
# tells us that there is a relationship between two variables

# The reaserch question is "how the precipitation rate 
# is affected by change in temperature"
# and it is clear from the test that there exists a relation
# between two variable which are precipitation and temperature.

#----------------------------------------------------------------------------------------------------------------------------------
# to check the correlation and for getting more idea on correaltion of data..
# not done as a part of ca3 
install.packages("Hmisc")
library(Hmisc)
rcorr(rainfall_data$Temperature, rainfall_data$Precipitation,type="pearson")

#	Spearman correlation matrix with pairwise deletion
# of missing data

# to check the correlation
mydata.cor = cor(rainfall_data$Precipitation, rainfall_data$Temperature)
mydata.cor

# finding rho factor for spearman correlation 
mydata.cor = cor(rainfall_data$Precipitation, rainfall_data$Temperature, method = c("spearman"))
mydata.cor

mydata.rcorr = rcorr(as.matrix(rainfall_data$Precipitation, rainfall_data$Temperature))
mydata.rcorr

t.test(Temperature~Precipitation, data = rainfall_data)

wilcox.test(rainfall_data$Precipitation, rainfall_data$Temperature)
