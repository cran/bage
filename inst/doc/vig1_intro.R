## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bage)
library(rvec)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
mod <- mod_pois(injuries ~ age : sex + age : ethnicity + year,
                data = injuries,
                exposure = popn) |>
  set_prior(year ~ AR1())
mod

