## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(0)

## ----setup--------------------------------------------------------------------
library(bage)
library(poputils)
library(rvec)

## -----------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
dth <- bage::isl_deaths
dth

## -----------------------------------------------------------------------------
tail(dth, n = 3)

## -----------------------------------------------------------------------------
dth |>
  count(deaths) |>
  mutate(percent = round(100 * n / sum(n)),
         cumulative_percent = cumsum(percent)) |>
  head()

## ----fig.width = 7, fig.height = 6--------------------------------------------
dth |>
  filter(time %in% c(1998, 2010, 2022)) |>
  mutate(rate = deaths / popn) |>
  ggplot(aes(x = age_mid(age), y = rate)) + ## 'age_mid()' returns the mid point
  facet_grid(vars(sex), vars(time)) +       ## of the age group, which is useful
  geom_point(alpha = 0.5) +                 ## for plotting
  scale_y_log10() +
  ggtitle("Direct estimates of mortality rates")

## -----------------------------------------------------------------------------
dth |>
  filter(deaths > 0 & popn == 0)

## -----------------------------------------------------------------------------
dth <- dth |>
  mutate(popn = if_else(deaths > 0 & popn == 0, 0.5, popn))

## -----------------------------------------------------------------------------
mod_base <- mod_pois(deaths ~ age * sex + time,
                 data = dth,
                 exposure = popn)
mod_base

## -----------------------------------------------------------------------------
mod_base <- fit(mod_base)
mod_base

## -----------------------------------------------------------------------------
aug_base <- augment(mod_base)
aug_base

## -----------------------------------------------------------------------------
rates_base <- aug_base |>
  filter(time %in% c(1998, 2010, 2022)) |>
  select(age, sex, time, .observed, .fitted) |>
  mutate(draws_ci(.fitted))
rates_base

## ----fig.width = 7, fig.height = 5--------------------------------------------
ggplot(rates_base, aes(x = age_mid(age), 
             ymin = .fitted.lower,
             y = .fitted.mid,
             ymax = .fitted.upper)) +
  facet_grid(vars(sex), vars(time)) +
  geom_ribbon(fill = "lightblue") +
  geom_line(col= "darkblue") +
  geom_point(aes(y = .observed),
             size = 0.2) +
  scale_y_log10() +
  ggtitle("Modelled and direct estimates of mortality rates - base model")

## -----------------------------------------------------------------------------
comp_base <- components(mod_base)
comp_base

## -----------------------------------------------------------------------------
age_effect <- comp_base |>
  filter(component == "effect",
         term == "age") |>
  mutate(draws_ci(.fitted))

## -----------------------------------------------------------------------------
ggplot(age_effect,
       aes(x = age_mid(level),
           y = .fitted.mid,
           ymin = .fitted.lower,
           ymax = .fitted.upper)) +
  geom_ribbon(fill = "lightblue") +
  geom_line() +
  ggtitle("Age effect")

## -----------------------------------------------------------------------------
mod_hmd <- mod_pois(deaths ~ age:sex + time,
                    data = dth,
                    exposure = popn) |>
  set_prior(age:sex ~ SVD(HMD))
mod_hmd

## -----------------------------------------------------------------------------
mod_hmd <- fit(mod_hmd)
mod_hmd

## ----fig.width = 7, fig.height = 5--------------------------------------------
aug_hmd <- augment(mod_hmd)

rates_hmd <- aug_hmd |>
  filter(time %in% c(1998, 2010, 2022)) |>
  select(age, sex, time, .observed, .fitted) |>
  mutate(draws_ci(.fitted))

ggplot(rates_hmd, aes(x = age_mid(age), 
             ymin = .fitted.lower,
             y = .fitted.mid,
             ymax = .fitted.upper)) +
  facet_grid(vars(sex), vars(time)) +
  geom_ribbon(fill = "lightblue") +
  geom_line(col= "darkblue") +
  geom_point(aes(y = .observed),
             size = 0.2) +
  scale_y_log10() +
  ggtitle("Modelled and direct estimates of mortality rates - HMD model")

## -----------------------------------------------------------------------------
comp_hmd <- components(mod_hmd)

age_sex_interact <- comp_hmd |>
  filter(component == "effect",
         term == "age:sex") |>
  separate_wider_delim(level, delim = ".", names = c("age", "sex")) |>
  mutate(draws_ci(.fitted))

ggplot(age_sex_interact,
       aes(x = age_mid(age),
           y = .fitted.mid,
           ymin = .fitted.lower,
           ymax = .fitted.upper)) +
  geom_ribbon(aes(fill = sex),
              alpha = 0.3) +
  geom_line(aes(col = sex)) +
  ggtitle("Age-sex interaction")

## ----fig.width = 7, fig.height = 7--------------------------------------------
rep_data_base <- replicate_data(mod_base, condition_on = "expected")

data <- rep_data_base |>
  filter(time == 2022) |>
  select(-popn) |>
  pivot_wider(names_from = sex, values_from = deaths) |>
  mutate(diff = Female - Male)

## ----fig.width = 7, fig.height = 7--------------------------------------------
ggplot(data, aes(x = age_mid(age), y = diff)) +
  facet_wrap(vars(.replicate)) +
  geom_point(size = 0.2)

## -----------------------------------------------------------------------------
lifeexp_hmd <- mod_hmd |>
  augment() |>
  lifeexp(mx = .fitted,
          by = c(time, sex))
lifeexp_hmd

## -----------------------------------------------------------------------------
lifeexp_hmd <- mod_hmd |>
  augment() |>
  lifeexp(mx = .fitted,
          by = c(time, sex))
lifeexp_hmd

