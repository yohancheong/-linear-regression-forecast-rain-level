library(weatherData)

# Check Data Availability for Sydney Airport (Kingsford Smith Airport)
data_available <- checkDataAvailability('SYD', '2015-01-01', station_type = 'airportCode')
if(data_available == 0) {
  stop('Data is not available')
}
  
# Check Data Availability for Date Range
data_avilable_for_date <- checkDataAvailabilityForDateRange('SYD', '2015-01-01', '2015-04-04', station_type = 'airportCode')
if(data_avilable_for_date == 0){
  stop('Data is not available for date range')
}

# Show Available Variables for Date Range
showAvailableColumns(station_id = 'SYD', 
                     start_date = '2015-01-01', 
                     end_date = '2015-04-04',
                     station_type = 'airportCode', 
                     opt_detailed = TRUE)

# Example:
# columnNumber            columnName
# 1             1              TimeAEDT
# 2             2          TemperatureC
# 3             3            Dew_PointC
# 4             4              Humidity
# 5             5 Sea_Level_PressurehPa
# 6             6          VisibilityKm
# 7             7        Wind_Direction
# 8             8        Wind_SpeedKm_h
# 9             9        Gust_SpeedKm_h
# 10           10       Precipitationmm
# 11           11                Events
# 12           12            Conditions
# 13           13        WindDirDegrees
# 14           14               DateUTC

# Get Weather Data for Date Range
df <- getWeatherForDate(station_id = 'SYD', 
                        start_date = '2015-01-01', 
                        end_date = '2015-04-04',
                        station_type = 'airportCode', 
                        opt_detailed = TRUE,
                        opt_custom_columns = TRUE,
                        custom_columns = c(1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 13))
df$Wind_SpeedKm_h <- as.numeric(as.character(df$Wind_SpeedKm_h))

# Add New Numeric Variable for Rain Level i.e.
# 0 >= (No Rain)
# 1 (Light Rain)
# 2 (Rain)
# 3 (Heavy Rain)

df <- cbind(df, RainLevel= 0)
df[grep('Rain', df$Conditions),]$RainLevel <- 1
df[grep('Light Rain', df$Conditions),]$RainLevel <- 2
df[grep('Heavy Rain', df$Conditions),]$RainLevel <- 3


# Calculate Pearson's correlation between Rain Level & Etc
# cor(df$RainLevel, df$Sea_Level_PressurehPa, method='pearson')

# Fit Linear Regression Model
fit <- lm(RainLevel ~ TemperatureC + Sea_Level_PressurehPa + Humidity + Wind_SpeedKm_h + Dew_PointC, data = df)


summary(fit)
confint(fit, conf.level=0.95) # (95% confidence interval)

# Diagnostic Plots to examin 1.Linearity assumption 2. Residuals normally distributed
par(mfrow=c(2,2))
plot(fit)

dateToForecast <- '2015-12-06' # (Date to Forecast Rain Level e.g. Sys.Date())

df2 <- getWeatherForDate('SYD', 
                         start_date = dateToForecast, 
                         end_date = dateToForecast,
                         station_type = 'airportCode', 
                         opt_detailed = TRUE,
                         opt_custom_columns = TRUE,
                         custom_columns = c(1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 13))
df2$Wind_SpeedKm_h <- as.numeric(as.character(df2$Wind_SpeedKm_h))

# Forecast Rain Level using Regression Model
predict(fit, newdata = data.frame(TemperatureC=df2$TemperatureC,
                                   Sea_Level_PressurehPa=df2$Sea_Level_PressurehPa,
                                   Humidity=df2$Humidity,
                                   Wind_SpeedKm_h=df2$Wind_SpeedKm_h,
                                   Dew_PointC=df2$Dew_PointC))

