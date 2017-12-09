#### ---------------------------------------------------------------------------
#### Packages

library(readr)
library(dplyr)
library(tibbletime)

#### ---------------------------------------------------------------------------
#### Parameters

# Assumed time zone (no concrete answer)
tz <- "UTC"

# Start date for the analysis
start_date <- as.POSIXct("2016-01-01 00:00:00", tz = tz)

# End date
end_date <- as.POSIXct("2017-10-19 23:59:59", tz = tz)

# Forecast start date
forecast_start_date <- as.POSIXct("2017-06-30 23:55:00", tz = tz)

parameters <- tibble(tz, start_date, end_date, forecast_start_date)

#### ---------------------------------------------------------------------------
#### Import data 

prices_raw <- read_csv("data/raw/bitstampUSD_1-min_data_2012-01-01_to_2017-10-20.csv")

#### ---------------------------------------------------------------------------
#### Clean data 

prices <- prices_raw %>%
  # Only relevant columns
  transmute(date     = as.POSIXct(Timestamp, tz = tz, origin = "1970-01-01"),
            adjusted = Weighted_Price) %>%
  # Date range filter
  filter(date >= start_date, date <= end_date) %>%
  # Aggregate to 5 minutes
  as_tbl_time(date) %>%
  as_period(5~M)

prices_minutely <- prices_raw %>%
  # Only relevant columns
  transmute(date     = as.POSIXct(Timestamp, tz = tz, origin = "1970-01-01"),
            adjusted = Weighted_Price) %>%
  # Date range filter
  filter(date >= start_date, date <= end_date)

#### ---------------------------------------------------------------------------
#### Save data 

write_csv(prices, path = "data/cleaned/cleaned-bitstamp.csv")

write_csv(prices_minutely, path = "data/cleaned/cleaned-bitstamp-minutely.csv")

write_rds(parameters, path = "data/cleaned/parameters.rds")
