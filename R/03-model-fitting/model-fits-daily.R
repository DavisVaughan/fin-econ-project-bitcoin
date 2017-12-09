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

# Minute data used to get the most accurate daily return
prices <- read_csv("data/cleaned/cleaned-bitstamp-minutely.csv")

#### ---------------------------------------------------------------------------
#### Calculate returns

# Daily returns
returns <- prices %>%
  tq_transmute(adjusted, dailyReturn, type = "log", col_rename = "return") %>%
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

garch_roller <- partial(
  ugarchroll,
  forecast.length = n_out_sample,
  n.ahead = 1, 
  refit.every = 1, 
  refit.window = "moving"
)

forecast <- models %>%
  mutate(
    forecast_refit = map2(
      .x = spec, 
      .y = data_xts, 
      .f = ~garch_roller(spec = .x, data = .y)
    )
  )

#### ---------------------------------------------------------------------------
#### Extract daily forecasts

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

# For some reason sigma() doesn't extract sigma from the roll objects
# Add an s3 method to do so and return as a tibble
sigma.uGARCHroll <- function(x, ...) {
  tibble(sigma = x@forecast$density$Sigma)
}

# Extract the sigma forecasts
forecast <- forecast %>%
  mutate(
    sigma_forecast = map2(
      .x = forecast_refit, 
      .y = data,
      .f = ~sigma(.x) %>%
        mutate(date = filter(.y, date > parameters$forecast_start_date) %>% pull(date))
    )
  ) 

# Change to daily variance forecasts
forecast <- forecast %>%
  mutate(
    daily_forecast = map(
      .x = sigma_forecast, 
      .f = ~group_by(.x, date) %>% 
        summarise(daily_variance_forecast = sum(sigma ^ 2))
    )
  )

# Extract only variance forecasts
daily_forecast <- forecast %>%
  unnest(daily_forecast)

#### ---------------------------------------------------------------------------
#### Save models and forecasts

# Save all model information
saveRDS(forecast, "models/garch-model-daily.rds")

# Save all forecast information
saveRDS(daily_forecast, "forecasts/forecast-daily.rds")
