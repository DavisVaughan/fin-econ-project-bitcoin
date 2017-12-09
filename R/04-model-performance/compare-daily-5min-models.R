#### ---------------------------------------------------------------------------
#### Load packages

library(readr)
library(tidyquant)
library(tidyverse)
library(rlang)

theme_set(theme_minimal())

#### ---------------------------------------------------------------------------
#### Parameters 

parameters <- read_rds("data/cleaned/parameters.rds")

#### ---------------------------------------------------------------------------
#### Load data

prices_5min <- read_csv("data/cleaned/cleaned-bitstamp.csv")
prices_min <- read_csv("data/cleaned/cleaned-bitstamp-minutely.csv")

daily_forc_from_5min  <- read_rds("forecasts/forecast-5-min.rds")
daily_forc_from_daily <- read_rds("forecasts/forecast-daily.rds") %>%
  # Remove this 1st day, it would end up sticking around downstream
  filter(date > as.POSIXct("2017-06-30 23:59:00", tz = "UTC"))

all_forecasts <- bind_rows(
  daily_forc_from_5min %>%
    mutate(forc_style = "5min"),
  daily_forc_from_daily %>%
    mutate(forc_style = "daily") %>%
    # Floor so we can combine with 5min
    mutate(date = lubridate::floor_date(date, "5 min"))
)

#### ---------------------------------------------------------------------------
#### Calculate OOS Returns

# 5min and daily OOS returns
returns_oos <- bind_rows(
  
  # 5 minute returns from 5-minute data
  prices_5min %>%
    mutate(return = log(adjusted) - log(lag(adjusted))) %>%
    slice(-1) %>%
    select(-adjusted) %>%
    filter(date > parameters$forecast_start_date) %>%
    mutate(return_type = "5min"),
  
  # Daily returns from minutely data
  prices_min %>%
    tq_transmute(adjusted, dailyReturn, type = "log", col_rename = "return") %>%
    slice(-1) %>%
    filter(date > parameters$forecast_start_date) %>%
    mutate(return_type = "daily") %>%
    mutate(date = lubridate::floor_date(date, "5 min"))
)

#### ---------------------------------------------------------------------------
#### Calculate variance proxies over the forecast period

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

# Calculate daily variance proxy from 5min data as realized variance
# Quadratic variance increments
realized_daily_variance <- returns_oos %>%
  filter(return_type == "5min") %>%
  collapse_to_daily() %>%
  group_by(date) %>%
  summarise(realized_variance = sum(return ^ 2))

# Calculate daily variance proxy as squared daily returns
squared_daily_return <- returns_oos %>%
  filter(return_type == "daily") %>%
  transmute(
    date = date,
    squared_return = return ^ 2)

#### ---------------------------------------------------------------------------
#### A comparison of Realized Variance and Squared Returns

# Comparing the two proxies

compare_proxies <- realized_daily_variance %>% 
  left_join(squared_daily_return, by = "date") %>%
  gather(type, value, -date) %>%
  mutate(type = recode(type, "realized_variance" = "Realized Variance", "squared_return" = "Squared Return")) %>%
  select(Proxy = type, everything()) %>%
  ggplot(aes(x=date, y=value, color = Proxy)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d(option = "D", end = .7) +
  labs(x = "", y = "Daily variance", color = "") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.margin = margin(t = -5)) 
  
ggsave("visualizations/compare_proxies.png", compare_proxies, width = 10, height = 5)

# Show the noisy-ness of the squared returns
realized_daily_variance %>% 
  left_join(squared_daily_return, by = "date") %>%
  gather(type, value, -date) %>%
  group_by(type) %>%
  summarise(sd = sd(value))
  
#### ---------------------------------------------------------------------------
#### Forecast performance plots

# Compare forecasts OOS of 5 min VS daily for Normal

compare_normal_forecasts <- all_forecasts %>%
  left_join(realized_daily_variance, by = "date") %>%
  filter(distribution == "Normal") %>%
  spread(forc_style, daily_variance_forecast) %>%
  gather(key = "Metric", value = "variance_estimate", -date, -distribution) %>%
  mutate(Metric = recode(Metric, "5min" = "GARCH(1,1)-N 5-min", 
                         "daily" = "GARCH(1,1)-N daily", "realized_variance" = "Realized Variance")) %>%
  ggplot(aes(x = date, y = variance_estimate, color = Metric)) + 
  geom_line() +
  geom_point() +
  labs(color = "", x = "", y = "") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.margin = margin(t = -5)) +
  scale_color_viridis_d(option = "D", end = .8)

ggsave("visualizations/compare_normal_forecasts.png", compare_normal_forecasts, width = 10, height = 5)

# Compare forecasts of Normal VS Student t for 5 min

compare_normal_vs_student_forecasts <- all_forecasts %>%
  left_join(realized_daily_variance, by = "date") %>%
  filter(forc_style == "5min") %>%
  spread(distribution, daily_variance_forecast) %>%
  gather(key = "Metric", value = "variance_estimate", -date, -forc_style) %>%
  mutate(Metric = recode(Metric, "Normal" = "GARCH(1,1)-N", 
                         "Student-t" = "GARCH(1,1)-T", "realized_variance" = "Realized Variance")) %>%
  ggplot(aes(x = date, y = variance_estimate, color = Metric)) + 
  geom_line() +
  geom_point() +
  labs(color = "", x = "", y = "") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.margin = margin(t = -5)) +
  scale_color_viridis_d(option = "D", end = .8)

ggsave("visualizations/compare_normal_vs_student_forecasts.png", compare_normal_vs_student_forecasts, width = 10, height = 5)

#### ---------------------------------------------------------------------------
#### MSE

# Besides mse/rmse, it looks like the agg of 5 min results did well
# Large mse likely due to the few outlier errors

all_forecasts %>%
  left_join(realized_daily_variance, by = "date") %>%
  group_by(distribution, forc_style) %>%
  summarise(mse  = mean((realized_variance - daily_variance_forecast) ^ 2),
            rmse = sqrt(mse),
            mae  = mean(abs(realized_variance - daily_variance_forecast)),
            mape = mean(abs(realized_variance - daily_variance_forecast) / sqrt(realized_variance)))

all_forecasts %>%
  left_join(squared_daily_return, by = "date") %>%
  group_by(distribution, forc_style) %>%
  summarise(mse  = mean((squared_return - daily_variance_forecast) ^ 2),
            rmse = sqrt(mse),
            mae  = mean(abs(squared_return - daily_variance_forecast)),
            mape = mean(abs(squared_return - daily_variance_forecast) / sqrt(squared_return)))

#### ---------------------------------------------------------------------------
#### Minzer - Zarnowitz regression

# For realized Variance 

MZ_realized_variance <- all_forecasts %>%
  left_join(realized_daily_variance, by = "date") %>%
  #regression results greatly improve with removal of that point
  #filter(date != as.POSIXct("2017-07-21 23:55:00", tz = parameters$tz)) %>%
  group_by(distribution, forc_style) %>%
  nest() %>%
  mutate(lm_fit = map(data, ~lm(realized_variance ~ daily_variance_forecast, data = .x)),
         tidy = map(lm_fit, broom::tidy),
         glance = map(lm_fit, broom::glance)) 

MZ_realized_variance %>% 
  unnest(tidy, .drop = TRUE)

MZ_realized_variance %>% 
  unnest(glance, .drop = TRUE)

# For squared returns

MZ_squared_returns <- all_forecasts %>%
  left_join(squared_daily_return, by = "date") %>%
  #filter(date != as.POSIXct("2017-07-21 23:55:00", tz = parameters$tz)) %>%
  group_by(distribution, forc_style) %>%
  nest() %>%
  mutate(lm_fit = map(data, ~lm(squared_return ~ daily_variance_forecast, data = .x)),
         tidy = map(lm_fit, broom::tidy),
         glance = map(lm_fit, broom::glance)) 

MZ_squared_returns %>% 
  unnest(tidy, .drop = TRUE)

MZ_squared_returns %>% 
  unnest(glance, .drop = TRUE)
