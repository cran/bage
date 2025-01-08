## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, out.width="50%", fig.cap = "Simulation study of a model"-----
knitr::include_graphics("vig7_fig.png")

## -----------------------------------------------------------------------------
library(bage)
library(dplyr, warn.conflicts = FALSE)
library(poputils)

divorces_small <- nzl_divorces |>
  filter(age_upper(age) < 40,
         time >= 2016) |>
  droplevels()

mod <- mod_pois(divorces ~ age * sex + time,
                data = divorces_small,
		            exposure = population)
mod		

## -----------------------------------------------------------------------------
set.seed(0)
res <- report_sim(mod_est = mod)
res

## -----------------------------------------------------------------------------
mod_ar1 <- mod |>
  set_prior(time ~ AR1())
mod_ar1	

## -----------------------------------------------------------------------------
res_ar1 <- report_sim(mod_est = mod, mod_sim = mod_ar1)
res_ar1

