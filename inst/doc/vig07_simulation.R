## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(0)

## ----echo=FALSE, out.width="50%", fig.cap = "Simulation study of a model"-----
knitr::include_graphics("vig07_fig.png")

## -----------------------------------------------------------------------------
library(bage)
library(dplyr, warn.conflicts = FALSE)
library(poputils)

divorces_small <- nzl_divorces |>
  filter(age_upper(age) < 40,
         time >= 2018) |>
  droplevels()

## -----------------------------------------------------------------------------
mod <- mod_pois(divorces ~ age + sex + time,
                data = divorces_small,
		            exposure = population) |>
  set_prior(`(Intercept)` ~ Known(-1)) |>
  set_prior(age ~ RW(sd = 0.05, s = 0.05)) |>
  set_prior(time ~ AR1(s = 0.05))
mod		

## ----message = FALSE----------------------------------------------------------
set.seed(0)
res <- report_sim(mod_est = mod)
res

## -----------------------------------------------------------------------------
mod_rw <- mod |>
  set_prior(time ~ RW(s = 0.05))
mod_rw	

## ----message = FALSE----------------------------------------------------------
set.seed(0)
res_rw <- report_sim(mod_est = mod, mod_sim = mod_rw)
res_rw

