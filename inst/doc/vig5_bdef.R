## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## -----------------------------------------------------------------------------
library(bage, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
swe_infant

## ----fig.height = 6-----------------------------------------------------------
ggplot(swe_infant, aes(x = time, y = deaths / births)) +
  facet_wrap(vars(county)) +
  geom_line()

## -----------------------------------------------------------------------------
mod <- mod_binom(deaths ~ county + time,
                 data = swe_infant,
                 size = births)
mod

## -----------------------------------------------------------------------------
mod <- mod |>
  fit()
mod

## -----------------------------------------------------------------------------
aug_init <- mod |>
  augment()
aug_init

## -----------------------------------------------------------------------------
aug_init <- aug_init |>
  mutate(draws_ci(.fitted))
aug_init

## ----fig.height = 7-----------------------------------------------------------
ggplot(aug_init, aes(x = time)) +
  facet_wrap(vars(county)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  geom_point(aes(y = .observed),
             color = "red",
	     size = 0.1) +
  xlab("") +
  ylab("")

## -----------------------------------------------------------------------------
comp_init <- mod |>
  components()
comp_init  

## ----fig.height = 1.2---------------------------------------------------------
intercept <- comp_init |>
  filter(term == "(Intercept)") |>
  mutate(draws_ci(.fitted))

ggplot(intercept, aes(y = level)) +
  geom_pointrange(aes(xmin = .fitted.lower,
                      x = .fitted.mid,
                      xmax = .fitted.upper))

## -----------------------------------------------------------------------------
county_effect <- comp_init |>
  filter(term == "county",
         component == "effect") |>
  rename(county = level) |>
  mutate(draws_ci(.fitted))
  
ggplot(county_effect, aes(y = county)) +
  geom_pointrange(aes(xmin = .fitted.lower,
                      x = .fitted.mid,
		      xmax = .fitted.upper))

## -----------------------------------------------------------------------------
time_effect <- comp_init |>
  filter(term == "time",
         component == "effect") |>
  rename(time = level) |>
  mutate(time = as.integer(time)) |>
  mutate(draws_ci(.fitted))

ggplot(time_effect, aes(x = time)) +
  geom_ribbon(aes(ymin = .fitted.lower,
   	             ymax = .fitted.upper),
	      fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue")

## -----------------------------------------------------------------------------
comp_init |>
  filter(component == "hyper")

## -----------------------------------------------------------------------------
mod_ident <- mod_binom(deaths ~ county + time,
                       data = swe_infant,
	 	       size = births) |>
  set_prior(time ~ RW(sd = 0)) |>
  fit()

time_effect_ident <- mod_ident |>
  components() |>
  filter(term == "time",
         component == "effect") |>
  rename(time = level) |>
  mutate(time = as.integer(time)) |>
  mutate(draws_ci(.fitted))

ggplot(time_effect_ident, aes(x = time)) +
  geom_ribbon(aes(ymin = .fitted.lower,
   	             ymax = .fitted.upper),
	      fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue")

## -----------------------------------------------------------------------------
rep_data <- replicate_data(mod)
rep_data

## ----fig.height = 3-----------------------------------------------------------
calculate_slope <- function(df) coef(lm(rate ~ time, data = df))[["time"]]
slopes <- rep_data |>
  mutate(rate = deaths / births) |>
  group_by(.replicate, county) |>
  nest() |>
  mutate(slope = sapply(data, calculate_slope))

ggplot(slopes, aes(x = .replicate, y = slope)) +
  geom_point(position = position_jitter(width = 0.1)) +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90))

## -----------------------------------------------------------------------------
prob_25 <- aug_init |>
  mutate(prob = prob(.fitted < 0.0025))

ggplot(prob_25, aes(x = time, y = prob)) +
  facet_wrap(vars(county)) +
  geom_line()

## -----------------------------------------------------------------------------
vals_forecast <- mod |>
  forecast(labels = 2016:2025,
           include_estimates = TRUE)
vals_forecast

## ----fig.height = 7-----------------------------------------------------------
vals_forecast <- vals_forecast |>
  mutate(draws_ci(.fitted))
  
ggplot(vals_forecast, aes(x = time)) +
  facet_wrap(vars(county)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
             fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  geom_point(aes(y = .observed),
             color = "red",
	     size = 0.1) +
  xlab("") +
  ylab("")

