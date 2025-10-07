## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(0)

## ----setup--------------------------------------------------------------------
library(bage)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

## -----------------------------------------------------------------------------
draws <- generate(HFD, n_draw = 200, age_labels = 15:49) |>
  mutate(age = as.integer(age),
         value = exp(value))
ggplot(draws, aes(x = age, y = value, group = draw)) +
  geom_line(alpha = 0.1)

