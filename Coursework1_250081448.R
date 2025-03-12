#' ---
#' title: Sunspot Time Series Project
#' author: Lizzie Gilmartin
#' date: Mar 6, 2025
#' Data Set: https://www.kaggle.com/datasets/abhinand05/daily-sun-spot-data-1818-to-2019
#' ---

# 1. Loading and Installing Packages ------------------------------------------------------------
install.packages("prophet")
install.packages("remotes")
remotes::install_github('facebook/prophet@*release', subdir='R')

## 2. Preparing Data for Analysis----------------------------------------------------------
# 2.1 Reading CSV File containing data
SunData = read.csv("data/sunspot_data.csv")
head(SunData)
tail(SunData)

# 2.2 filter out dates with no data (-1 for number of sunspots and also start from year 1900)
filtered_SunData = SunData[SunData$Number.of.Sunspots >= 0,]
print(filtered_SunData) #check filter worked

# 2.3 Grouping and Averaging Data by Year
#install.packages("dplyr")
library(dplyr)
AverageSunspotDataYearly = filtered_SunData %>% #stores our grouped data in a new data frame
    group_by(Year) %>% #this groups the data by year
    summarise(mean_Sunspots = mean(Number.of.Sunspots)) #this averages the data in each group which was grouped by year

# trying to do monthly
# Group by Year and Month, then calculate the average of 'Value'
AveSunspotsMonthly <- filtered_SunData %>%
    group_by(Year, Month) %>%  # Group by Year and Month
    summarise(AverageValue = mean(Number.of.Sunspots))  # Compute the average of 'Value'
AveSunspotsMonthly$Date <- as.Date(paste(AveSunspotsMonthly$Year, AveSunspotsMonthly$Month, "01", sep = "-"))
# View the result
AveSunspotsMonthly

# 3. Visualizing Data -----------------------------------------------------------------------------
# 3.1 Entire Data set
plot(filtered_SunData$Date.In.Fraction.Of.Year, filtered_SunData$Number.of.Sunspots,
     type='line', lwd = 0.2,col="red", main = 'Number of Sunspots Observed 1818-2019', xlab = "Date of Observation", ylab = "Number of Sunspots")
SunData_1900s = filtered_SunData[filtered_SunData$Year >= 1900,]
plot(filtered_SunData$Date.In.Fraction.Of.Year, filtered_SunData$Number.of.Sunspots,
     type='l', lwd = 0.2,col="red", main = "Number of Sunspots Observed After 1900", xlab = "Date of Observation", ylab = "Number of Sunspots")
points(AverageSunspotDataYearly, col="blue") #visualize output
lines(ts_AveSunspotsMonthly, col="green")
legend("topright",
       legend = c("All Data", "Data Averaged per Year"),  # Text for each item in the legend
       col = c("red", "black"),      # Color of the points/lines in the plot
       cex = 0.6,                  # Control text size (smaller text)
       lty = 1)              # Line type (if it's a line plot)
plot(SunData_1900s$Date.In.Fraction.Of.Year, SunData_1900s$Standard.Deviation,
     type='line', col="green", main = "Standard Deviation of Number of Sunspots", xlab = "Date", ylab = "Standard Deviation",)

## 4 USING PROPHET TO FORECAST-----------------------------------
# 4.1 Predicting Using All Data in the Time Series
# store data in a data fram so that prophet can read it
SunSpots.df = data.frame(ds = zoo::as.yearmon(filtered_SunData$Date.In.Fraction.Of.Year),
    y=filtered_SunData$Number.of.Sunspots)
SunSpots.df$cap = 100  # Set the cap (upper bound of sunspots)
SunSpots.df$floor = 0  # Set the floor (lower bound of sunspots)
m = prophet::prophet(SunSpots.df, changepoint.prior.scale = 0.05, growth = 'logistic')
f = prophet::make_future_dataframe(m, periods = 10, freq = 'year') #make future dates for prediction
f$cap = 200  # Cap for the future predictions needed fro logistic prediction
f$floor = 0.5   # Floor for the future predictions needed fro logistic prediction
Prediction = predict(m,f)
prophet::prophet_plot_components(m, Prediction)
plot(m,Prediction, ,type = 'line', xlab="Year", ylab='Number of Sun Spots')
# Prophet has a difficult time predicting given all of the data. And it seems
#to highly depend on what the cap is set to. So we might want to use the data
#averages each year to see if this helps
# 4.2 Prediction After Averaging Over Years--------------------------------------------
#first select dates we want to use in prediction
FilteredAverageSunspotDataYearly = AverageSunspotDataYearly[AverageSunspotDataYearly$Year>=1818,]
#store our data in a data frame prophet can read
AveSunspotsYearly.df = data.frame(ds = zoo::as.yearmon(FilteredAverageSunspotDataYearly$Year),
                         y=FilteredAverageSunspotDataYearly$mean_Sunspots)
AveSunspotsYearly.df$cap = 500  # Set the cap (upper bound of sunspots)
AveSunspotsYearly.df$floor = 0  # Set the floor (lower bound of sunspots)
# calculate the trend
m <- prophet::prophet( yearly.seasonality = FALSE, monthly.seasonality=FALSE, daily.seasonality = FALSE)
m = prophet::add_seasonality(m, name = "Sun.Cycle", period = 3985, fourier.order = 10)
m = prophet::
m = prophet::prophet(AveSunspotsYearly.df, changepoint.prior.scale = 0.05, growth = 'logistic')
#calculate future dates fro prophet to use in its prediction
f = prophet::make_future_dataframe(m, periods = 20, freq = 'year') #make future dates for prediction
f$cap = 200  # Cap for the future predictions needed fro logistic prediction
f$floor = 0.5   # Floor for the future predictions needed fro logistic prediction
#run a prediction of future number of sunspots
Prediction = predict(m,f)
# plot the data and prediction
plot(m,Prediction, ,type = 'line', main = 'Prophet', xlab="Year", ylab='Number of Sun Spots')
prophet::prophet_plot_components(m, Prediction)

#It seems Like Prophet does a better job of recognizing the overall trend in
#this case, but it still does not pick up on the periodic behavior of sunspots
#I think this is because prophet only has yearly, monthly and daily seasonality

# 5 Learning How to Use Spectral Density to Estimate Seasonality -------------------
# first plot the yearly average to visualize the date more simply
plot(AverageSunspotDataYearly[AverageSunspotDataYearly$Year>=1950,], type="line", xlab="Year", ylab="Number of Sunspots", main = "Avergae Number of Sunspots Measured Per Year") #visualize output

# Convert to time series
ts_AveSunspotsMonthly = ts(AveSunspotsMonthly$AverageValue, start = c(1818,1), frequency = 12)
class(ts_AveSunspotsMonthly) #check that the data was correclty converted to a ts

plot(ts_AveSunspotsMonthly, type = 'l',col = 'red', main = "Time Series of Average Sunspots with Fitted Line", ylab = "Average Sunspot Number", xlab = "Time")
#lines (AveSunspotsMonthly$Date, m , col="black")
decompositionMonthly = stats::decompose(ts_AveSunspotsMonthly)
plot(decompositionMonthly)
seasonal_component = decompositionMonthly$seasonal
plot(seasonal_component, type = 'l', col = 'blue', main = "Seasonal Component - 11-Year Cycle", ylab = "Seasonal Component", xlab = "Time")

# You can further analyze the seasonal component for an 11-year pattern, for example:
# You can compute the periodogram to investigate periodicity and confirm the 11-year cycle
stats::spectrum(ts_AveSunspotsMonthly)

k = kernel("daniell", 4)
mvspec(AveSunspotsMonthly$AverageValue, k, log="no")
abline(v=1/132, lty="dotted")


