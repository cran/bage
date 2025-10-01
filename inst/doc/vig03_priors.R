## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 1.5
)

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages({
  library(bage)
  library(poputils)
  library(ggplot2)
  library(dplyr)
})	

## -----------------------------------------------------------------------------
plot_exch <- function(draws) {
  ggplot(draws, aes(x = element, y = value)) +
    facet_wrap(vars(draw), nrow = 1) +
    geom_hline(yintercept = 0, col = "grey") +
    geom_point(col = "darkblue", size = 0.7) +
    scale_x_continuous(n.breaks = max(draws$element)) +
    xlab("Unit") +
    ylab("") +
    theme(text = element_text(size = 8))
}	       

## -----------------------------------------------------------------------------
plot_cor_one <- function(draws) {
  ggplot(draws, aes(x = along, y = value)) +
    facet_wrap(vars(draw), nrow = 1) +
    geom_hline(yintercept = 0, col = "grey") +
    geom_line(col = "darkblue") +
    xlab("Unit") +
    ylab("") +
    theme(text = element_text(size = 8))
}	       

## -----------------------------------------------------------------------------
plot_cor_many <- function(draws) {
  ggplot(draws, aes(x = along, y = value)) +
    facet_grid(vars(by), vars(draw)) +
    geom_hline(yintercept = 0, col = "grey") +
    geom_line(col = "darkblue") +
    xlab("Unit") +
    ylab("") +
    theme(text = element_text(size = 8))
}	       

## -----------------------------------------------------------------------------
plot_svd_one <- function(draws) {
  ggplot(draws, aes(x = age_mid(age), y = value, color = sexgender)) +
    facet_wrap(vars(draw), nrow = 1) +
    geom_line() +
    scale_color_manual(values = c("darkgreen", "darkorange")) +
    xlab("Age") +
    ylab("") +
    theme(text = element_text(size = 8),
          legend.position = "top",
          legend.title = element_blank())
}

## -----------------------------------------------------------------------------
plot_svd_many <- function(draws) {
  draws |>
    mutate(element = paste("Unit", element)) |>
    ggplot(aes(x = age_mid(age), y = value, color = sexgender)) +
    facet_grid(vars(element), vars(draw)) +
    geom_line() +
    scale_color_manual(values = c("darkgreen", "darkorange")) +
    xlab("Age") +
    ylab("") +
    theme(text = element_text(size = 8),
          legend.position = "top",
          legend.title = element_blank())
}

## -----------------------------------------------------------------------------
set.seed(0)

NFix() |>
  generate(n_element = 10, n_draw = 8) |>
  plot_exch()

## ----fig.height = 1.5---------------------------------------------------------
set.seed(0)

NFix(sd = 0.01) |>
  generate(n_element = 10, n_draw = 8) |>
  plot_exch()

## -----------------------------------------------------------------------------
set.seed(0)

N() |>
  generate(n_element = 10, n_draw = 8) |>
  plot_exch()

## ----fig.height = 1.5---------------------------------------------------------
set.seed(0)

N(s = 0.01) |>
  generate(n_element = 10, n_draw = 8) |>
  plot_exch()

## -----------------------------------------------------------------------------
set.seed(0)

RW() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW(s = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW(s = 0.01, sd = 0) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2(s = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2(s = 0.01, sd_slope = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2(s = 0.01, sd = 0, sd_slope = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

AR() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

AR(n_coef = 3) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

AR(s = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## ----fig.height = 4.5---------------------------------------------------------
set.seed(0)

AR() |>
  generate(n_along = 20, n_by = 3, n_draw = 8) |>
  plot_cor_many()

## ----fig.height = 4.5---------------------------------------------------------
set.seed(0)

AR(con = "by") |>
  generate(n_along = 20, n_by = 3, n_draw = 8) |>
  plot_cor_many()

## -----------------------------------------------------------------------------
set.seed(0)

AR1() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

AR1(s = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

AR1(s = 0.01, min = -1, max = 1) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW_Seas(n_seas = 4) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW_Seas(n_seas = 4, s = 0.01, sd = 0) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW_Seas(n_seas = 4, s = 0.01, sd = 0, s_seas = 1) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW_Seas(n_seas = 4, s = 0.01, sd = 0, sd_seas = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2_Seas(n_seas = 4) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

RW2_Seas(n_seas = 4, s = 0.01, sd = 0, sd_slope = 0.01, sd_seas = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin(s = 0) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin(mean_slope = 0.2, sd_slope = 0.1) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## ----fig.height = 4.5---------------------------------------------------------
set.seed(0)

Lin() |>
  generate(n_along = 20, n_by = 3, n_draw = 8) |>
  plot_cor_many()

## ----fig.height = 4.5---------------------------------------------------------
set.seed(0)

Lin(con = "by") |>
  generate(n_along = 20, n_by = 3, n_draw = 8) |>
  plot_cor_many()

## -----------------------------------------------------------------------------
set.seed(0)

Lin_AR() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin_AR(s = 0.1, sd_slope = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin_AR1() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin_AR1(min = -1, max = 1) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Lin_AR1(s = 0.1, sd_slope = 0.02) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Sp() |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Sp(n_comp = 5) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## -----------------------------------------------------------------------------
set.seed(0)

Sp(s = 0.01, sd = 0.01, sd_slope = 0.01) |>
  generate(n_draw = 8) |>
  plot_cor_one()

## ----fig.height = 3-----------------------------------------------------------
set.seed(0)

SVD(HMD) |>
  generate(n_draw = 8) |>
  plot_svd_one()

## ----fig.height = 2-----------------------------------------------------------
SVD(HMD, n_comp = 5) |>
  generate(n_draw = 8) |>
  plot_svd_one()

## ----fig.height = 2-----------------------------------------------------------
SVD(HMD, indep = FALSE) |>
  generate(n_draw = 8) |>
  plot_svd_one()

## ----fig.height = 4.5---------------------------------------------------------
SVD(HMD, indep = FALSE) |>
  generate(n_draw = 8, n_element = 3) |>
  plot_svd_many()

## ----fig.height = 8-----------------------------------------------------------
SVD_AR(HMD, indep = FALSE, s = 0.1) |>
  generate(n_draw = 6, n_along = 5) |>
    ggplot(aes(x = age_mid(age), 
                      y = value, 
                      color = sexgender)) +
      facet_grid(vars(draw), vars(along)) +
      geom_line() +
      scale_color_manual(values = c("darkgreen", "darkorange")) +
      xlab("Age") +
      ylab("") +
      theme(text = element_text(size = 8),
            legend.position = "top",
            legend.title = element_blank())

