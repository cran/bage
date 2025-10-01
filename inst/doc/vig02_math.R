## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
library(tibble)

models <- tibble(
  Model = c("Poisson undercount", "Poisson overcount", "Poisson miscount",
  "Poisson noise", "Poisson exposure", "Binomial undercount", "Normal noise"),
  `System model` = c(
    paste(
    "$$",
    "y_i^{\\text{true}} \\sim \\text{Poisson}(\\gamma_i w_i) \\\\",
    "\\gamma_i \\sim \\text{Gamma}(\\xi^{-1}, (\\xi \\mu_i)^{-1})",
    "$$"
    ),
    paste(
    "$$",
    "y_i^{\\text{true}} \\sim \\text{Poisson}(\\gamma_i w_i) \\\\",
    "\\gamma_i \\sim \\text{Gamma}(\\xi^{-1}, (\\xi \\mu_i)^{-1})",
    "$$"
    ),
    paste(
    "$$",
    "y_i^{\\text{true}} \\sim \\text{Poisson}(\\gamma_i w_i) \\\\",
    "\\gamma_i \\sim \\text{Gamma}(\\xi^{-1}, (\\xi \\mu_i)^{-1})",
    "$$"
    ),
    "$$ y_i^{\\text{true}} \\sim \\text{Poisson}(\\mu_i w_i) $$",
    "$$ y_i \\sim \\text{Poisson}(\\mu_i w_i^{\\text{true}}) $$",
    paste(
    "$$",
    "y_i^{\\text{true}} \\sim \\text{Binomial}(w_i, \\gamma_i) \\\\",
    "\\gamma_i \\sim \\text{Beta}(\\mu_i \\xi^{-1}, (1-\\mu_i)\\xi^{-1})",
    "$$"
    ),
    "$$ y_i^{\\text{true}} \\sim \\text{N}(\\gamma_i, w_i^{-1} \\sigma^2) $$"
  ),
  `Data model` = c(
    paste(
      "$$",
      "y_i^{\\text{obs}} \\sim \\text{Binomial}(y_i^{\\text{true}}, \\pi_{g[i]}) \\\\",
      "\\pi_g \\sim \\text{Beta}(a_g, b_g)",
      "$$"
    ),
    paste(
      "$$",
      "y_i^{\\text{obs}} = y_i^{\\text{true}} + \\epsilon_i \\\\",
      "\\epsilon_i \\sim \\text{Poisson}(\\kappa_{g[i]} \\gamma_i w_i) \\\\",
      "\\kappa_g \\sim \\text{Gamma}(a_g, b_g)",
      "$$"
    ),
    paste(
      "$$",
      "y_i^{\\text{obs}} = u_i + v_i \\\\",
      "u_i \\sim \\text{Binomial}(y_i^{\\text{true}}, \\pi_{g[i]}) \\\\",
      "v_i \\sim \\text{Poisson}(\\kappa_{h[i]} \\gamma_i w_i) \\\\",
      "\\pi_g \\sim \\text{Beta}(a_g^{(\\pi)}, b_g^{(\\pi)}) \\\\",
      "\\kappa_h \\sim \\text{Gamma}(a_h^{(\\kappa)},  b_h^{(\\kappa)})",
      "$$"
    ),
    paste(
      "$$",
      "y_i^{\\text{obs}} = y_i^{\\text{true}} + \\epsilon_i \\\\",
      "\\epsilon_i \\sim \\text{Skellam}(m_{g[i]}, m_{g[i]})",
      "$$"
    ),
    paste(
      "$$",
      "w_i^{\\text{obs}} \\sim \\text{InvGamma}(2 + d_{g[i]}^{-1}, [1 + d_{g[i]}^{-1}] w_i^{\\text{true}})",
      "$$"
    ),
    paste(
    "$$",
    "y_i^{\\text{obs}} \\sim \\text{Binomial}(y_i^{\\text{true}}, \\pi_{g[i]}) \\\\",
    "\\pi_g \\sim \\text{Beta}(a_g, b_g)",
    "$$"
    ),
    paste(
      "$$",
      "y_i^{\\text{obs}} = y_i^{\\text{true}} + \\epsilon_i \\\\",
      "\\epsilon_i \\sim \\text{N}(0, s_{g[i]}^2) ",
      "$$"
    )
  )
)

kable(models,
      format = "pipe",
      align = c("l", "c", "c"),
      escape = FALSE,
      caption = "System models and data models")

