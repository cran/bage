## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(bage)
  library(dplyr)
  library(ggplot2)
})


## -----------------------------------------------------------------------------
kor_births

## -----------------------------------------------------------------------------
mod_gdp_dens <- mod_pois(births ~ (age + region + time)^2,
                         data = kor_births,
                         exposure = popn) |>
  set_covariates(~ gdp_pc_2023 + dens_2020) |>
  fit()
mod_gdp_dens

## -----------------------------------------------------------------------------
mod_gdp_dens |>
  components() |>
  filter(term == "covariates")

## -----------------------------------------------------------------------------
births <- kor_births |>
  mutate(is_dragon_year = time == 2012)
mod_dragon <- mod_pois(births ~ (age + region + time)^2,
                      data = births,
                      exposure = popn) |>
  set_covariates(~ is_dragon_year) |>
  fit()

mod_dragon |>
  components() |>
  filter(term == "covariates")

## -----------------------------------------------------------------------------
births <- births |>
  mutate(is_dragon_year_age = if_else(time == 2012, age, "baseline"),
         is_dragon_year_age = factor(is_dragon_year_age, 
                                     levels = c("baseline", unique(age))))
births |>
  filter(time %in% 2011:2013)

## -----------------------------------------------------------------------------
mod_dragon_age <- mod_pois(births ~ (age + region + time)^2,
                         data = births,
                         exposure = popn) |>
  set_covariates(~ is_dragon_year_age) |>
  fit()
mod_dragon_age

## -----------------------------------------------------------------------------
mod_dragon_age |>
  components() |>
  filter(term == "covariates") |>
  mutate(age = sub("is_dragon_year_age", "", level)) |>
  select(age, .fitted)

## -----------------------------------------------------------------------------
mod_gdp_dens |>
  forecast(labels = 2024:2025)

## -----------------------------------------------------------------------------
newdata <- expand.grid(age = unique(kor_births$age),
                       region = unique(kor_births$region),
                       time = 2024:2025) |>
  mutate(is_dragon_year = FALSE)
head(newdata)

## -----------------------------------------------------------------------------
mod_dragon |>
  forecast(newdata = newdata)

