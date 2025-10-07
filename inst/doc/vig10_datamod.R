## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(0)

## ----include = FALSE----------------------------------------------------------
library(bage)
library(dplyr)
library(poputils)
library(ggplot2)
library(patchwork)
births <- kor_births |>
  filter(region == "Ulsan",
         time == 2023) |>
  select(age, births, popn)

## -----------------------------------------------------------------------------
births

## ----echo = FALSE, fig.width = 3, fig.height = 3------------------------------
ggplot(births, aes(x = age_mid(age))) +
  geom_point(aes(y = births),
             color = "darkblue") +
  xlab("Age") +
  ylab("Birth count")

## -----------------------------------------------------------------------------
library(bage)
library(dplyr)
mod_base <- mod_pois(births ~ age,
                     data = births,
                     exposure = popn) |>
  fit()
mod_base

## ----fig.width = 3, fig.height = 3, echo = FALSE, message = FALSE-------------
aug_base <- mod_base |>
  augment() |>
  mutate(draws_ci(.fitted))

ggplot(aug_base, aes(x = age_mid(age))) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

## -----------------------------------------------------------------------------
prob_under <- data.frame(mean = 0.8, disp = 0.02)

## ----message = FALSE----------------------------------------------------------
mod_under <- mod_base |>
  set_datamod_undercount(prob = prob_under) |>
  fit()
mod_under

## ----messages = FALSE---------------------------------------------------------
mod_under |>
  augment()

## ----echo = FALSE, fig.width = 6, fig.height = 3, message = FALSE, warning = FALSE----
aug_under <- mod_under |>
  augment() |>
  mutate(draws_ci(.births)) |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Base = aug_base, 
                  Undercount = aug_under,
                  .id = "model") |>
  mutate(.births.mid = if_else(is.na(.births.mid), births, .births.mid))

ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

## ----echo = FALSE, fig.width = 6, fig.height = 3, message = FALSE, warning = FALSE----
ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_point(aes(y = .births.mid),
             color = "darkblue") +
  geom_linerange(aes(ymin = .births.lower,
                     ymax = .births.upper),
		 data = filter(data, model == "Undercount"),
	         color = "darkblue") +
  xlab("Age") +
  ylab("Birth count")

## -----------------------------------------------------------------------------
mod_under |>
  components() |>
  filter(term == "datamod")

## -----------------------------------------------------------------------------
rate_over <- data.frame(mean = 0.1, disp = 0.05)

## -----------------------------------------------------------------------------
mod_over <- mod_base |>
  set_datamod_overcount(rate = rate_over)

## ----echo = FALSE, fig.width = 6, fig.height = 6, message = FALSE-------------
mod_over <- mod_over |>
  fit()

aug_over <- mod_over |>
  augment(quiet = TRUE) |>
  mutate(draws_ci(.births)) |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Base = aug_base, 
                  Overcount = aug_over,
                  .id = "model") |>
  mutate(.births.mid = if_else(is.na(.births.mid), births, .births.mid))

p_rate <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

p_count <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_point(aes(y = .births.mid),
             color = "darkblue") +
  geom_linerange(aes(ymin = .births.lower,
                     ymax = .births.upper),
		 data = filter(data, model == "Overcount"),
	         color = "darkblue") +
  xlab("Age") +
  ylab("Birth count")

p_rate / p_count

## ----message = FALSE----------------------------------------------------------
mod_mis <- mod_base |>
  set_datamod_miscount(prob = prob_under,
                       rate = rate_over)

## ----echo = FALSE, fig.width = 6, fig.height = 6, message = FALSE-------------
mod_mis <- mod_mis |>
  fit()

aug_mis <- mod_mis |>
  augment(quiet = TRUE) |>
  mutate(draws_ci(.births)) |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Base = aug_base, 
                  Miscount = aug_mis,
                  .id = "model") |>
  mutate(.births.mid = if_else(is.na(.births.mid), births, .births.mid))

p_rate <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

p_count <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_point(aes(y = .births.mid),
             color = "darkblue") +
  geom_linerange(aes(ymin = .births.lower,
                     ymax = .births.upper),
		 data = filter(data, model == "Miscount"),
	         color = "darkblue") +
  xlab("Age") +
  ylab("Birth count")

p_rate / p_count

## -----------------------------------------------------------------------------
mod_mis |>
  components() |>
  filter(term == "datamod")

## ----message = FALSE----------------------------------------------------------
mod_noise <- mod_base |>
  set_disp(mean = 0) |>
  set_datamod_noise(s = 50)

## ----echo = FALSE, fig.width = 6, fig.height = 6, message = FALSE-------------
mod_noise <- mod_noise |>
  fit()

aug_noise <- mod_noise |>
  augment(quiet = TRUE) |>
  mutate(draws_ci(.births)) |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Base = aug_base, 
                  Noise = aug_noise,
                  .id = "model") |>
  mutate(.births.mid = if_else(is.na(.births.mid), births, .births.mid))

p_rate <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

p_count <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_point(aes(y = .births.mid),
             color = "darkblue") +
  geom_linerange(aes(ymin = .births.lower,
                     ymax = .births.upper),
		 data = filter(data, model == "Noise"),
	         color = "darkblue") +
  xlab("Age") +
  ylab("Birth count")

p_rate / p_count

## ----message = FALSE----------------------------------------------------------
mod_expose <- mod_base |>
  set_disp(mean = 0) |>
  set_datamod_exposure(cv = 0.05)

## ----echo = FALSE, fig.width = 6, fig.height = 6, message = FALSE-------------
mod_expose <- mod_expose |>
  fit()

aug_expose <- mod_expose |>
  augment(quiet = TRUE) |>
  mutate(draws_ci(.popn)) |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Base = aug_base, 
                  Expose = aug_expose,
                  .id = "model") |>
  mutate(.popn.mid = if_else(is.na(.popn.mid), popn, .popn.mid))

p_rate <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

p_popn <- ggplot(data, aes(x = age_mid(age))) +
  facet_wrap(vars(model)) +
  geom_point(aes(y = .popn.mid),
             color = "darkblue") +
  geom_linerange(aes(ymin = .popn.lower,
                     ymax = .popn.upper),
		 data = filter(data, model == "Expose"),
	         color = "darkblue") +
  ylim(0, NA) +	 
  xlab("Age") +
  ylab("Population")

p_rate / p_popn

## ----include = FALSE----------------------------------------------------------
births_time <- kor_births |>
  filter(region == "Ulsan",
         time %in% 2021:2023) |>
  select(age, time, births, popn)

## -----------------------------------------------------------------------------
births_time

## -----------------------------------------------------------------------------
prob_under_time <- data.frame(time = c(2021, 2022, 2023),
                              mean = c(0.99, 0.8,  0.99),
			                        disp = c(0.01, 0.02, 0.01))

## -----------------------------------------------------------------------------
mod_timeconst <- mod_pois(births ~ age + time,
                          data = births_time,
			                    exposure = popn) |>
  set_datamod_undercount(prob = prob_under)

mod_timevarying <- mod_timeconst |>
  set_datamod_undercount(prob = prob_under_time)

## ----echo = FALSE, fig.width = 6, fig.height = 6, message = FALSE, warning = FALSE----
aug_timeconst <- mod_timeconst |>
  fit() |>
  augment() |>
  mutate(draws_ci(.fitted))

aug_timevarying <- mod_timevarying |>
  fit() |>
  augment() |>
  mutate(draws_ci(.fitted))

data <- bind_rows(Constant = aug_timeconst, 
                  Varying = aug_timevarying,
                  .id = "model")

ggplot(data, aes(x = age_mid(age))) +
  facet_grid(vars(model), vars(time)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

## -----------------------------------------------------------------------------
births_time <- births_time |>
  mutate(agegp = if_else(age %in% c("10-14", "15-19",
                                    "20-24", "25-29",
                                    "30-34"),
                         "10-34",
                         "35+"))                             

prob_under_agetime <- data.frame(
  time =  c(2021,    2022,    2023,    2021,  2022,  2023),
  agegp = c("10-34", "10-34", "10-34", "35+", "35+", "35+"),
  mean =  c(0.95,    0.95,    0.95,    0.95,  0.5,  0.95),
  disp =  c(0.01,    0.01,    0.01,    0.01,  0.1,  0.01))

mod_agetime <- mod_pois(births ~ age + time,
                        data = births_time,
			                  exposure = popn) |>
  set_datamod_undercount(prob = prob_under_agetime)

## ----echo = FALSE, fig.width = 6, fig.height = 3, message = FALSE, warning = FALSE----
aug_agetime <- mod_agetime |>
  fit() |>
  augment() |>
  mutate(draws_ci(.fitted))

ggplot(aug_agetime, aes(x = age_mid(age))) +
  facet_wrap(vars(time)) +
  geom_ribbon(aes(ymin = .fitted.lower,
                  ymax = .fitted.upper),
              fill = "lightblue") +
  geom_line(aes(y = .fitted.mid),
            color = "darkblue") +
  xlab("Age") +
  ylab("Birth rate")

## -----------------------------------------------------------------------------
mod_timeconst |>
  fit() |>
  forecast(label = 2024)

## -----------------------------------------------------------------------------
newdata_births <- data.frame(
  age = c("10-14", "15-19", "20-24", "25-29", "30-34",
          "35-39", "40-44", "45-49", "50-54"),
  time = rep(2024, 9),
  popn = c(27084, 25322, 23935, 28936, 30964,
           31611, 44567, 41774, 51312))

mod_timeconst |>
  fit() |>
  forecast(newdata = newdata_births)

## -----------------------------------------------------------------------------
prob_under_time_ext <- rbind(
  prob_under_time,
  data.frame(time = 2024,
             mean = 0.95,
             disp = 0.05))
prob_under_time_ext

## ----message = FALSE----------------------------------------------------------
mod_under_time_ext <- mod_pois(births ~ age + time,
                               data = births_time,
                               exposure = popn) |>
  set_datamod_undercount(prob = prob_under_time_ext) |>
  fit() |>
  forecast(labels = 2024)

## -----------------------------------------------------------------------------
births_rr3 <- births |>
  mutate(births = rr3(births))

mod_under_rr3 <- mod_pois(births ~ age,
                          data = births_rr3,
			  exposure = popn) |>
  set_datamod_undercount(prob = prob_under) |>
  set_confidential_rr3()

