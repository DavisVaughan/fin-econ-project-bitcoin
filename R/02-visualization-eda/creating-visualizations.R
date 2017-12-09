#### ---------------------------------------------------------------------------
#### Packages

library(readr)
library(dplyr)
library(tibbletime)
library(ggplot2)
library(ggfortify)
library(tidyquant)

theme_set(theme_minimal())

#### ---------------------------------------------------------------------------
#### Load Data

prices <- read_csv("data/cleaned/cleaned-bitstamp.csv")
parameters <- read_rds("data/cleaned/parameters.rds")

#### ---------------------------------------------------------------------------
#### Calculate 5 minute log returns and daily returns

returns <- prices %>%
  mutate(returns = log(adjusted) - log(lag(adjusted))) %>%
  slice(-1) %>%
  mutate(type = "5 Minute")

returns_daily <- prices %>%
  tq_transmute(adjusted, to.daily, drop.time = FALSE) %>%
  mutate(returns = log(adjusted) - log(lag(adjusted))) %>%
  slice(-1) %>%
  mutate(type = "Daily")

returns_total <- bind_rows(
  returns,
  returns_daily
)

#### ---------------------------------------------------------------------------
#### A few visualizations

return_vis <- cowplot::plot_grid(
  
  returns_total %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +
    labs(title = "5 minute sampling VS Daily sampling", y = "Price", x = "") +
    facet_wrap(~type, scales = "free_y") +
    theme(plot.title = element_text(hjust = 0.5)),
  
  returns_total %>%
    ggplot(aes(x = date, y = returns)) +
    geom_line() +
    labs(x = "", y = "Log return") +
    facet_wrap(~type, scales = "free_y"),
  
  returns_total %>%
    ggplot(aes(x = date, y = returns ^ 2)) +
    geom_line() +
    labs(x = "", y = "Log return ^ 2") +
    facet_wrap(~type, scales = "free_y"),
  
  returns_total %>%
    ggplot(aes(x = date, y = returns ^ 2)) +
    geom_line() +
    labs(x = "", y = "abs(Log return)") +
    facet_wrap(~type, scales = "free_y"),
  
  nrow = 4
)

cowplot::save_plot("visualizations/returns.png", return_vis, 
                   ncol = 2, nrow = 4,
                   base_aspect_ratio = 1.5)

#### ---------------------------------------------------------------------------
#### ACF visualizations

acf_vis <- cowplot::plot_grid(
  autoplot(acf(returns$returns)) +
    labs(title = "ACF of log returns"),
  
  autoplot(acf(returns_daily$returns)) +
    labs(title = "ACF of log returns"),
  
  autoplot(acf(returns$returns ^ 2)) +
    labs(title = "ACF of squared log returns"),
  
  autoplot(acf(returns_daily$returns ^ 2)) +
    labs(title = "ACF of squared log returns"),
  
  autoplot(acf(abs(returns$returns))) +
    labs(title = "ACF of absolute log returns"),
  
  autoplot(acf(abs(returns_daily$returns))) +
    labs(title = "ACF of absolute log returns"),
  
  nrow = 3
)

cowplot::save_plot("visualizations/acf.png", acf_vis, 
                   ncol = 2, nrow = 3,
                   base_aspect_ratio = 1.5)
