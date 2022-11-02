pacman::p_load(tidyverse, janitor)
library(here)
library(fixest)

# data ----
wooldridge::card |> as_tibble() -> card_data


# analysis -------
card_data |> 
  skimr::skim(nearc4)

## proportion of compliers so that we can see the proportion of info
## that IV will get.
(card_data |> 
  mutate(educ12 = educ > 12) |> 
  lm(educ12 ~ nearc4, data = _) |> 
  coef() |> pluck(2) -> compliers) # (%) of compliers. It does increase
# by 12% the fact that you live near college on receiving more than 12 years of educ.

## intention to treat effect ----
(card_data |> 
  mutate(educ12 = educ > 12) |> 
  lm(lwage ~ nearc4, data = _) |> 
  coef() |> pluck(2) -> itt)
## LATE

(late <- itt/compliers)

# using the fixest package ------
## no confounders
card_data |> 
  mutate(
    educ12 = educ > 12) -> card_data
card_data |>
  feols(lwage ~ 1 |educ12 ~ nearc4, data = _)

## confounders
card_data |> 
  feols(lwage ~ exper + reg661 + reg662 + reg663 + reg664 + 
          reg665 + reg666 + reg667 + reg668|
          educ12 ~ nearc4, data = _)


# project ----------

