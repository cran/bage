## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(bage)
library(poputils)
library(dplyr)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
head(nzl_injuries)

## -----------------------------------------------------------------------------
nzl_injuries |>
  filter(year %in% c(2000, 2006, 2012, 2018)) |>
  ggplot(aes(x = age_mid(age), y = injuries / popn, color = sex)) +
  facet_grid(vars(ethnicity), vars(year)) +
  geom_line() +
  xlab("age") +
  theme(legend.position = "top",
        legend.title = element_blank())

## -----------------------------------------------------------------------------
mod <- mod_pois(injuries ~ age * sex + age * ethnicity + year,
                data = nzl_injuries,
                exposure = popn)

## ----echo=FALSE, out.width="90%", fig.cap = "Structure of model. Rectangles denote data, ellipses denote unknown quantities that are inferred within the model, solid arrows denote probabilistic relationships, and dashed arrows denote deterministic relationships."----
knitr::include_graphics("vig1_dag.png")

## -----------------------------------------------------------------------------
mod

## -----------------------------------------------------------------------------
mod <- mod |>
  fit()

## -----------------------------------------------------------------------------
mod

## -----------------------------------------------------------------------------
aug <- mod |>
  augment()
aug

## -----------------------------------------------------------------------------
aug |>
  select(.observed, .fitted, .expected) 

## -----------------------------------------------------------------------------
comp <- mod |>
  components()
comp

## -----------------------------------------------------------------------------
age_effect <- comp |>
  filter(term == "age",
         component == "effect") |>
  select(age = level, .fitted)
age_effect

## -----------------------------------------------------------------------------
data_plot <- aug |>
  filter(year == 2018) |>
  mutate(draws_ci(.fitted))
data_plot |>
  select(starts_with(".fitted"))

## -----------------------------------------------------------------------------
ggplot(data_plot, aes(x = age_mid(age))) +
  facet_grid(vars(sex), vars(ethnicity)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  geom_point(aes(y = .observed),
             color = "red") +
  xlab("age")

## -----------------------------------------------------------------------------
mod <- mod |>
  set_prior(year ~ AR1())

## -----------------------------------------------------------------------------
is_fitted(mod)

## -----------------------------------------------------------------------------
mod <- mod |>
  fit()

## -----------------------------------------------------------------------------
mod_births <- mod_pois(births ~ age * region + age * time,
                       data = kor_births,
                       exposure = popn) |>
  set_prior(age ~ SVD(HFD)) |>
  set_prior(age:time ~ SVD_RW(HFD)) |>
  fit()
mod_births

## -----------------------------------------------------------------------------
aug_forecast <- mod |>
  forecast(labels = 2019:2028)
names(aug_forecast)

## -----------------------------------------------------------------------------
comp_forecast <- mod |>
  forecast(labels = 2019:2028,
           output = "components")
comp_forecast

## -----------------------------------------------------------------------------
data_forecast <- mod |>
  fit() |>
  forecast(labels = 2019:2028,
           include_estimates = TRUE) |>
  filter(sex == "Female",
         age %in% c("10-14", "25-29", "40-44")) |>
  mutate(draws_ci(.fitted))

ggplot(data_forecast, aes(x = year)) +
  facet_grid(vars(age), vars(ethnicity)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  geom_point(aes(y = .observed),
             color = "red")  

## -----------------------------------------------------------------------------
years_mis <- 2010:2014

injuries_mis <- nzl_injuries |>
  mutate(injuries = if_else(year %in% years_mis, NA, injuries))

## -----------------------------------------------------------------------------
mod_mis <- mod_pois(injuries ~ age * sex + age * ethnicity + year,
                    data = injuries_mis,
                    exposure = popn) |>
  fit()

## -----------------------------------------------------------------------------
mod_mis |>
  augment() |>
  filter(year %in% years_mis)

## -----------------------------------------------------------------------------
data_plot_mis <- mod_mis |>
  augment() |>
  filter(age == "20-24") |>
  mutate(draws_ci(.fitted))

ggplot(data_plot_mis, aes(x = year)) +
  facet_grid(vars(sex), vars(ethnicity)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  geom_point(aes(y = .observed),
             color = "red") +
  xlab("age")

## -----------------------------------------------------------------------------
mod <- mod |>
  set_datamod_outcome_rr3() |>
  fit()

## -----------------------------------------------------------------------------
mod |>
  augment()

## -----------------------------------------------------------------------------
rep_data <- mod |>
  replicate_data()
rep_data

## -----------------------------------------------------------------------------
sex_ratio <- rep_data |>
  count(.replicate, year, sex, wt = injuries) |>
  pivot_wider(names_from = sex, values_from = n) |>
  mutate(ratio = Male / Female)
sex_ratio

## -----------------------------------------------------------------------------
ggplot(sex_ratio, aes(x = year, y = ratio)) +
  facet_wrap(vars(.replicate)) +
  geom_line()

## -----------------------------------------------------------------------------
set.seed(0)

## Create simulated data
fake_data <- data.frame(year = 2001:2010, 
                        population = NA)

## Define the true data-generating model
mod_rw <- mod_pois(population ~ year,
                   data = fake_data,
                   exposure = 1) |>
  set_prior(`(Intercept)` ~ NFix(sd = 0.1)) |>
  set_prior(year ~ RW(s = 0.1, sd = 0.1))

## Define the estimation model
mod_rw2 <- mod_pois(population ~ year,
                    data = fake_data,
                    exposure = 1) |>
  set_prior(year ~ RW2())

## Run the simulation
report_sim(mod_est = mod_rw2, mod_sim = mod_rw)

## -----------------------------------------------------------------------------
mod |>
  unfit() |>
  components()

