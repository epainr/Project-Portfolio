library(ggplot2)
library(dplyr)
library(geosphere)
library(reshape2)
library(stringr)
library(car)
library(MASS)
library(psych)
library(tidyr)

# DATA CLEANING

listings <- read.csv("listings.csv")

# Eliminate all non-numeric type variables except for the select few 
# non-numeric variables that we intend to manipulate.
airbnb <- dplyr::select(listings, where(is.numeric) | 
                          all_of(c('amenities', 'price', 'security_deposit', 
                                   'cleaning_fee', 'zipcode', 'host_since', 
                                   'host_response_rate')))

# NOTE: host_listings_count and host_total_listings_count are equal
#       for 48,846 observations, so we will only consider
#       host_total_listings_count.

# NOTE: minimum and maximum nights can vary based on condition
#       (higher minimum during busy times, lower maximum during specific
#        seasons, etc) and so there are both typical minimums and maximums
#        and maximum minimums, minimum minimums, maximum maximums, and
#        minimum maximums. Most listings just have one minimum and one 
#        maximum, but for the sake of accuracy, we will use the
#        minimum and maximum averages.

# Eliminate all numeric variables that we're not interested in examining.
airbnb <- dplyr::select(airbnb,
                        -all_of(c('id', 'scrape_id', 'host_id', 
                                  'host_listings_count', 'maximum_nights', 
                                  'minimum_minimum_nights',
                                  'maximum_minimum_nights',
                                  'minimum_maximum_nights',
                                  'maximum_maximum_nights', 'minimum_nights',
                                  'availability_30', 'availability_60', 
                                  'availability_90', 'number_of_reviews_ltm',
                                  'review_scores_accuracy',
                                  'review_scores_cleanliness',
                                  'review_scores_checkin',
                                  'review_scores_communication',
                                  'review_scores_location',
                                  'review_scores_value',
                                  'calculated_host_listings_count',
                                  'calculated_host_listings_count_entire_homes',
                                  'calculated_host_listings_count_private_rooms',
                                  'calculated_host_listings_count_shared_rooms')))

# Show square footage is largely NA and should be removed from the list of
# potential predictors, Figure 1.1.1
ggplot(data = data.frame(is.na(airbnb$square_feet))) +
  geom_bar(aes(x = is.na.airbnb.square_feet.)) +
  labs(title = "Count of Observations with NA Square Footage",
       y = "Count",
       x = "Is Square Footage Field NA?")

# Convert price, cleaning_fee, and security_deposit to numeric type.
airbnb$price <- gsub('\\$|,', '', airbnb$price)
airbnb$price <- as.numeric(airbnb$price)

airbnb$security_deposit <- gsub('\\$|,', '', airbnb$security_deposit)
airbnb$security_deposit <- as.numeric(airbnb$security_deposit)

airbnb$cleaning_fee <- gsub('\\$|,', '', airbnb$cleaning_fee)
airbnb$cleaning_fee <- as.numeric(airbnb$cleaning_fee)

# Standardize the zipcode field.
airbnb$zipcode = gsub('NY|-| ', '', airbnb$zipcode)
airbnb$zipcode = substring(airbnb$zipcode, first = 1, last = 5)
airbnb$zipcode <- as.factor(airbnb$zipcode)

# Create a distance variable that measures the distance in miles from the 
# listing to Times Square.
for (i in 1:48864) {
  airbnb$distance[i] <- (distm(c(airbnb$longitude[i], airbnb$latitude[i]), 
                               c(-73.985130, 40.758896),
                               distHaversine) / 1609.34)[1,1]
}

# Create a days_hosting variable that measures the number of days each host
# has used Airbnb as a host.
airbnb$host_since <- as.Date(airbnb$host_since)
airbnb$days_hosting <- as.Date('2022-01-01') - airbnb$host_since

# Convert response rate to numeric type. There are many NA values introduced
# during the coercion but all of the NAs are appropriately tagged "N/A" in 
# the original dataset or are entirely blank (so also NA).
airbnb$host_response_rate = gsub('%| ', '', airbnb$host_response_rate)
airbnb$host_response_rate = as.numeric(airbnb$host_response_rate)
airbnb$host_response_rate = airbnb$host_response_rate / 100

# Create a variable to track the number of amenities offered for each listing
airbnb$num_amenities <- str_count(airbnb$amenities, ',') + 1

# Drop the non-numeric variables now that we have derived the information we
# wanted from them, as well as the square footage variable since it is
# largely NA.
airbnb <- dplyr::select(airbnb, -all_of(c('amenities', 'latitude', 
                                          'longitude', 'host_since', 
                                          'square_feet')))

# Subset the data down to only those listings in Upper East Side Manhattan.
airbnb <- airbnb[airbnb$zipcode == 10065 |
                   airbnb$zipcode == 10128 |
                   airbnb$zipcode == 10075 |
                   airbnb$zipcode == 10028 |
                   airbnb$zipcode == 10021, ]

# Remove two observations with erroneous zipcode tagging, as latitude and 
# longitude reveal these two listings are not located in Manhattan.
airbnb <- airbnb[airbnb$distance < 6, ]

# Reassign NAs in the security_deposit and cleaning_fee field to zero.
airbnb$security_deposit <- airbnb$security_deposit %>% replace_na(0)
airbnb$cleaning_fee <- airbnb$cleaning_fee %>% replace_na(0)

# Redefine the price of a one-night stay in the given listing to include
# its cleaning fee.
airbnb$price <- airbnb$price + airbnb$cleaning_fee

# Drop the cleaning fee column.
airbnb <- dplyr::select(airbnb, -cleaning_fee)

# Reorder columns.
airbnb <- airbnb[, c(13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
                     10, 11, 12, 16, 17, 18, 19, 15)]

names(airbnb) <- c('price', 'security_deposit', 'total_listings', 
                   'accommodates', 'bathrooms', 'bedrooms', 'beds', 
                   'num_guests', 'min_nights', 'max_nights', 
                   'year_availability', 'num_reviews', 'review_score', 
                   'reviews_per_month', 'response_rate', 'distance', 
                   'days_hosting', 'num_amenities', 'zipcode')

# Remove rows with NA values.
airbnb <- na.omit(airbnb)

# EXPLORATORY ANALYSIS

# Create scatterplots of price against each potential predictor variable
individual_plots <- melt(airbnb[, 1:18], id.vars = 'price', na.rm = TRUE)

# Figure 1.3.1
ggplot(data = individual_plots) +
  geom_jitter(mapping = aes(x = value, y = price, colour = variable)) +
  geom_smooth(mapping = aes(x = value, y = price, colour = variable), 
              method = lm, se = FALSE) +
  facet_wrap(~variable, scales = "free_x")

# Plot a histogram of the price variable, Figure 1.3.2
ggplot(data = airbnb, mapping = aes(x = price)) +
  geom_histogram(breaks = seq(0, 2500, 100), 
                 color = "#91383b", fill = '#ff5a60') +
  labs(title = 'Price Distribution', y = 'Count', x = 'Price')

# MODELING

# Create our first full linear regression model, Figure 2.1.1
full <- lm(price ~ security_deposit + total_listings + accommodates + 
             bathrooms + bedrooms + beds + num_guests + min_nights + 
             max_nights + year_availability + num_reviews + review_score +
             reviews_per_month + response_rate + distance + days_hosting + 
             num_amenities, data = airbnb)

summary(full)

# Take only those variables that proved to be significant when tested against
# a significance level of 0.10—Figure 2.1.2
reduced <- lm(price ~ security_deposit + total_listings + accommodates + 
                bathrooms + beds + num_guests + min_nights + 
                year_availability + num_reviews + review_score + 
                reviews_per_month +distance + num_amenities, 
              data = airbnb)

summary(reduced)

# Figure 2.1.3
summary(lm(price ~ security_deposit + total_listings + accommodates + 
             bathrooms + beds + num_guests + min_nights + year_availability + 
             num_reviews + review_score + distance + num_amenities, 
           data = airbnb))

# Compare the two models. The p value is significantly greater than our
# significance level of 0.10, meaning that the additional predictor variables
# of the full model do not improve the precision of our model and that our 
# reduced model is a better fit of the data.
anova(reduced, full)

# No evidence of multicollinearity in our model.
vif(reduced)

# RESIDUAL ANALYSIS PRE TRANSFORMATION

# Standardized
range(stdres(reduced))

# Figure 2.2.1
barplot(height = stdres(reduced), names.arg = 1:854,
        main = "Standardized Residuals - Pre Transformation", xlab = "Index",
        ylab = "Standardized Residual", ylim = c(-11, 11))

abline(h = 3, col = "#ff5a60", lwd = 2)
abline(h = -3, col = "#ff5a60", lwd = 2)

# Nine significant data points
sum(abs(stdres(reduced)) > 3)

# Studentized
range(studres(reduced))

# Figure 2.2.1
barplot(height = studres(reduced), names.arg = 1:854,
        main = "Studentized Residuals - Pre Transformation", xlab = "Index",
        ylab = "Studentized Residual", ylim = c(-11, 11))

abline(h = 3, col = "#ff5a60", lwd = 2)
abline(h = -3, col = "#ff5a60", lwd = 2)

# Nine significant data points
sum(abs(studres(reduced)) > 3)

# R-Student
range(rstudent(reduced))

# Figure 2.2.1
barplot(height = rstudent(reduced), names.arg = 1:854,
        main = "R-Student Residuals - Pre Transformation", xlab = "Index",
        ylab = "R-Student Residual", ylim=c(-11, 11))

abline(h = qt(0.10 / (2 * 854), 839, lower.tail = FALSE), 
       col = "#ff5a60", lwd = 2)
abline(h = -qt(0.10 / (2 * 854), 839, lower.tail = FALSE), 
       col = "#ff5a60", lwd = 2)

# Five significant data points
sum(abs(rstudent(reduced)) > qt(0.10 / (2 * 854), 839, lower.tail = FALSE))

# Normal Residual Plot pre, Figure 2.2.2
hist(studres(reduced), breaks = seq(-5, 12, 0.5), freq = FALSE, col = "#ff5a60", 
     border = "#91383b", cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
     main = "Pre Transformation", xlab = 'Studentized Residuals', xlim = c(-13, 13))

# QQ Plot pre, Figure 2.2.3
qqPlot(reduced, col.lines = "#ff5a60", envelope = c(style = 'lines'),
       ylab = 'Studentized Residuals - Pre Transformation')

# Fitted against residuals pre, Figure 2.2.4
residualPlot(reduced, type = "rstudent", quadratic = FALSE, col = "#ff5a60",
             pch = 16, cex = 0.75, cex.axis = 1.5, cex.lab = 1.5, 
             ylab = "R-Student Residuals")

# Residuals against regressors pre
residualPlots(reduced, type = "rstudent", fitted = FALSE, quadratic = FALSE, 
              col = "#ff5a60", pch = 16, cex = 0.75, cex.axis = 1,
              cex.lab = 1.3, ylab = "R-Student Residuals")

# TRANSFORMATION

# Find ideal lambda value according to boxcox power transformation
boxCox(reduced)$x[which.max(boxCox(reduced)$y)]

# Our plot of the residuals against the fitted values showed nonconstant error
# variance, so we will transform the response, price, using the square root
# method.
transformed <- lm(sqrt(price) ~ security_deposit + total_listings + 
                    accommodates + bathrooms + beds + num_guests + 
                    min_nights + year_availability + num_reviews + 
                    review_score + reviews_per_month + distance + 
                    num_amenities, data = airbnb)

summary(transformed)

# RESIDUAL ANALYSIS POST TRANSFORMATION

# Standardized
range(stdres(transformed))

# Figure 2.2.8
barplot(height = stdres(transformed), names.arg = 1:854,
        main = "Standardized Residuals - Post Transformation", xlab = "Index",
        ylab = "Standardized Residual", ylim = c(-4, 4))

abline(h = 3, col = "#ff5a60", lwd = 2)
abline(h = -3, col = "#ff5a60", lwd = 2)

# Six significant data points
sum(abs(stdres(transformed)) > 3)

# Studentized
range(studres(transformed))

# Figure 2.2.8
barplot(height = studres(transformed), names.arg = 1:854,
        main = "Studentized Residuals - Post Transformation", xlab = "Index",
        ylab = "Studentized Residual", ylim = c(-4, 4))

abline(h = 3, col = "#ff5a60", lwd = 2)
abline(h = -3, col = "#ff5a60", lwd = 2)

# Six significant data points
sum(abs(studres(transformed)) > 3)

# R-Student
range(rstudent(transformed))

# Figure 2.2.8
barplot(height = rstudent(transformed), names.arg = 1:854,
        main = "R-Student Residuals - Post Transformation", xlab = "Index",
        ylab = "R-Student Residual", ylim=c(-4, 4))

abline(h = qt(0.10 / (2 * 854), 839, lower.tail = FALSE), 
       col = "#ff5a60", lwd = 2)
abline(h = -qt(0.10 / (2 * 854), 839, lower.tail = FALSE), 
       col = "#ff5a60", lwd = 2)

# One significant data point
sum(abs(rstudent(transformed)) > qt(0.10 / (2 * 854), 839, lower.tail = FALSE))

# Normal Residual Plot post, Figure 2.2.7
hist(studres(transformed), breaks = seq(-4, 4, 0.5), freq = FALSE, col = "#ff5a60", 
     border = "#91383b", cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
     main = "Post Transformation", xlab = 'Studentized Residuals')

# QQ plot post, Figure 2.2.6
qqPlot(transformed, col.lines = "#ff5a60", envelope = c(style = 'lines'),
       ylab = 'Studentized Residuals - Post Transformation')

# Fitted against residuals post, Figure 2.2.5
residualPlot(transformed, type = "rstudent", quadratic = FALSE, col = "#ff5a60",
             pch = 16, cex = 0.75, cex.axis = 1.5, cex.lab = 1.5, 
             ylab = "R-Student Residuals")

# Residuals against regressors post
residualPlots(transformed, type = "rstudent", fitted = FALSE, quadratic = FALSE, 
              col = "#ff5a60", pch = 16, cex = 0.75, cex.axis = 1,
              cex.lab = 1.3, ylab = "R-Student Residuals")

# INFLUENTIAL POINTS ANALYSIS

inf_measures <- influence.measures(transformed)
inf_measures_df <- data.frame(summary(inf_measures))

# Plot dfbetas, Figure 2.3.1
dfbetasPlots(transformed, intercept = TRUE, col = "#ff5a60", pch = 16, 
             cex = 0.75, layout = c(4, 4))

# Plot Cook's D and Hat Values, Figure 2.3.2
influenceIndexPlot(transformed, vars = c('Cook', 'hat'), pch = 16, 
                   cex = 0.75, col = "#ff5a60", id = list(method = "y", n = 2, 
                                                          cex = 0.50, 
                                                          col = carPalette()[1], 
                                                          location = "lr"))

# p = 14, n = 854, n - p = 840

# How many points have significant COVRATIO values? 57
sum(inf_measures_df$cov.r > (1 + (3 * 14 / 840)) | inf_measures_df$cov.r < (1 - (3 * 14 / 840)))

# How many points have significant DFFITS values? 18
sum(abs(inf_measures_df$dffit) > 3 * sqrt(14 / 840))

# How many points have significant Cook's D values? 0
sum(inf_measures_df$cook.d > qf(0.5, 14, 840))

# How many points have significant hat values? 31
sum(inf_measures_df$hat > 3 * 14 / 854)
