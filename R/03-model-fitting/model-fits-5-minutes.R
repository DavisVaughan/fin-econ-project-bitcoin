#### ---------------------------------------------------------------------------
#### Load packages

library(readr)
library(tidyquant)
library(tidyverse)
library(rugarch)
library(rlang)

#### ---------------------------------------------------------------------------
#### Parameters 

parameters <- read_rds("data/cleaned/parameters.rds")

#### ---------------------------------------------------------------------------
#### Load data

prices <- read_csv("data/cleaned/cleaned-bitstamp.csv")

#### ---------------------------------------------------------------------------
#### Calculate returns

# 5 minute returns
returns <- prices %>%
  mutate(return = log(adjusted) - log(lag(adjusted))) %>%
  slice(-1)

#### ---------------------------------------------------------------------------
#### GARCH Model set up

# Garch(1,1) models
# Assume return mean is 0
# Fit using normal and student t distributions

models <- tibble(
  distribution = c("Normal", "Student-t"),
  spec = list(
    
    # Garch(1,1) Normal
    ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm", fixed.pars = list(omega = 0)
    ),
    
    # Garch(1,1) Student t distribution
    ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "std", fixed.pars = list(omega = 0)
    )
  )
)

# Join data to models
models <- models %>%
  mutate(data = list(returns))

#### ---------------------------------------------------------------------------
#### GARCH fitting, in sample

# Add xts format data
models <- models %>%
  mutate(
    data_xts = map(
      .x = data, 
      .f = ~xts::xts(.x$return, order.by = .x$date)
    )
  )

# Number of points past the forecast start. Using xts filtering syntax
n_out_sample <- nrow(filter(returns, date > parameters$forecast_start_date))

garch_fitter <- partial(ugarchfit, out.sample = n_out_sample)

# Fit the in sample garch models
models <- models %>%
  mutate(
    fit = map2(
      .x = spec, 
      .y = data_xts,
      .f = ~garch_fitter(spec = .x, data = .y)
    )
  )

#### ---------------------------------------------------------------------------
#### GARCH Forecasting - With refit

# 1) Forecast 1 day's worth of 5 minute returns
# 2) Drop the oldest day of returns
# 3) Add the newest day of real returns
# 4) Refit the model
# 5) Forecast the next 1 day's worth of 5 minute returns

# Collapse a date column to the end of the day
collapse_to_daily <- function(x) {
  x %>% 
    mutate(year = year(date),
           month = month(date),
           day   = day(date)) %>% 
    group_by(year, month, day, add = TRUE) %>%
    mutate(date = max(date)) %>%
    group_by(!!! groups(x)) %>%
    select(-year, -month, -day)
}

# Custom function to perform daily forecasts
daily_rolling_forecaster <- function(spec, data, forecast_start) {
  
  # All day endpoints. Used to determine oldest day to drop
  day_ends <- tibble(date = index(data)) %>%
    collapse_to_daily() %>%
    pull(date) %>%
    unique()
  
  # Always include the first date as an endpoint as well
  # (ugly workaround to losing UTC timezone with the c() function)
  day_ends <- c(0, as.numeric(day_ends)) %>%
    as.POSIXct(tz = parameters$tz, origin = "1970-01-01") %>%
    unique()
  
  # New day to add
  new_days <- day_ends[day_ends >= parameters$forecast_start_date]
  
  # Number of refits (don't actually refit the final time, so remove 1)
  n_refits <- seq_along(new_days[-1])
  
  # Set up tibble to hold forecasts
  forecast_tbl <- tribble(~date, ~sigma_5min_forecast)
 
  for(i in n_refits) {
    
    # Filter the dataset. Drop the oldest day, and add the newest day
    # Actually adds the next two days, but the second isn't used in the fit (out.sample)
    # its instead used to denote the out of sample points
    current_data_subsetter <- paste0(as.character(day_ends[i] + 1), "/", as.character(new_days[i+1]))
    fit_data <- data[current_data_subsetter]
    
    # Number of observations to forecast 
    # (changes per day, only real trading days)
    next_day_subsetter <- paste0(as.character(new_days[i] + 1), "/", as.character(new_days[i+1]))
    n_ahead <- nrow(data[next_day_subsetter])
    
    print(paste0("Predicting: ", new_days[i+1]))
    # if(new_days[i+1] == as.POSIXct("2017-07-21 23:55:00", tz = "UTC")) {
    #   browser()
    # }

    # Refit the data on the newest dataset
    new_fit <- ugarchfit(spec, fit_data, out.sample = n_ahead, solver = "hybrid")
    new_forecast <- ugarchforecast(new_fit, n.ahead = n_ahead)
    
    # Save forecasts, add to previous results
    forecast_tbl <- bind_rows(forecast_tbl,
                              tibble(date = new_days[i+1], 
                                     sigma_5min_forecast = sigma(new_forecast)[,1]))
  } 
  
  forecast_tbl
}

# Perform the rolling forecasts
forecast <- models %>%
  mutate(
    sigma_forecast= map2(
      .x = spec,
      .y = data_xts,
      .f = ~daily_rolling_forecaster(.x, .y, forecast_start)
    )
  )


#### ---------------------------------------------------------------------------
#### Extract daily forecasts

forecast <- forecast %>%
  mutate(
    daily_forecast = map(
      .x = sigma_forecast,
      .f = ~.x %>%
        group_by(date) %>%
        summarise(daily_variance_forecast = sum(sigma_5min_forecast ^ 2))
    )
  )

# Extract only variance forecasts
daily_forecast <- forecast %>%
  unnest(daily_forecast)

#### ---------------------------------------------------------------------------
#### Save models and forecasts

# Save all model information
saveRDS(forecast, "models/garch-model-5-min.rds")

# Save all forecast information
saveRDS(daily_forecast, "forecasts/forecast-5-min.rds")
