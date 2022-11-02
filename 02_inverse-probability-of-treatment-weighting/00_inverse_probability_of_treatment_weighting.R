pacman::p_load(tidyverse, janitor)
library(here)
library(Matching)
library(srvyr)
# lecture ----------
## data ---------
read_csv(here(
              "01_matching",
              "01_data",
              "rhc.csv")) -> coursera_data
coursera_data |> 
  rename(key = ...1) -> coursera_data

coursera_data |> 
  recipes::recipe(ca ~ ., data = _) |> 
  recipes::step_dummy(cat1, one_hot = T) |> 
  recipes::prep() |> 
  recipes::bake(new_data = NULL) |> 
  clean_names() -> coursera_data
coursera_data |> 
  mutate(
    female = as.numeric(sex == "Female"),
    died = as.numeric(death == "Yes"),
    age = age,
    treatment = as.numeric(swang1 == "RHC"),
    meanbp1 = meanbp1,
    aps = aps1
  ) -> coursera_data
coursera_data |> glimpse()

### data to use --------#
coursera_data |> 
  dplyr::select(
    starts_with("cat1"),
    age,
    female,
    meanbp1,
    aps,
    treatment,
    died
  ) -> coursera_data_to_use
#### confounder variables -------#
coursera_data_to_use |> 
  dplyr::select(
    starts_with("cat1"),
    age,
    female,
    meanbp1,
    aps,
    died
  ) |> colnames() -> confounder_variables
## propensity score modelling -----------
coursera_data_to_use |> 
  glm(treatment ~ age + female + meanbp1 + 
        cat1_arf + cat1_chf + cat1_cirrhosis + 
        cat1_colon_cancer + cat1_coma + 
        cat1_lung_cancer + cat1_mosf_w_malignancy + 
        cat1_mosf_w_sepsis + aps,
      data = _,
      family = binomial(link = "logit")) -> propensity_score_model
propensity_score_model |> broom::tidy()
propensity_score_model |> 
  broom::augment(new_data = coursera_data_to_use,
                 type.predict = "response") -> ps_data

## create weights ------------
ps_data |> 
  mutate(
    weight = ifelse(treatment == 1,
                    1/.fitted, 1/(1-.fitted))
  ) -> ps_data
### check weights ------
ps_data |> 
  skimr::skim(weight) # same as using the package so everything is ok.

## create the pseudo-population -----
survey::svydesign(
  ids = ~1,
  data = coursera_data_to_use,
  weights = ps_data |> pluck("weight")
) -> pseudo_population

### pseudo population table -----------
tableone::svyCreateTableOne(
  vars = confounder_variables,
  strata = "treatment",
  data = pseudo_population,
  test = F
) |> 
  print(smd = T) # Looks balanced!

## Marginal structure model --------
coursera_data_to_use |> 
  glm(died ~ treatment, data = _,
      weights = ps_data |> pluck("weight"),
      family = binomial(link = "identity")) -> model_causality_final

### get correct standard errors -----#
model_causality_final |> coef() -> beta_iptw
sandwich::vcovHC(model_causality_final, type = "HC0") |> 
  diag() |> sqrt() -> se
#### then you can add this to your estimates in beta_iptw!


# data project -------
MatchIt::lalonde |> as_tibble() -> lalonde_data
## confounding variables ----
lalonde_data |> 
  mutate(
    black = as.numeric(race == "black"),
    hispan = as.numeric(race == "hispan")
  ) -> lalonde_data
lalonde_data |> 
  dplyr::select(
    age, educ, black,
    hispan, married, nodegree,
    re74, re75
  ) |> colnames() -> confounder_names
## propensity score model -------
lalonde_data |> 
  glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
      data = _, family = binomial(link = "logit")) -> glm_model_2

glm_model_2 |> 
  broom::augment(
    new_data = lalonde_data,
    type.predict = "response"
  ) -> glm_model_2_data

glm_model_2_data |> 
  mutate(
    weight = ifelse(treat == 1,
                    1/.fitted, 1/(1-.fitted))
  ) -> glm_model_2_data
glm_model_2_data |> 
  skimr::skim(weight)

## standarized differences ----------
survey::svydesign(
    ids = ~1,
    data = glm_model_2_data,
    weights = glm_model_2_data |> pluck("weight")
  ) -> pseudo_population_2

pseudo_population_2 |> 
  tableone::svyCreateTableOne(
    vars = confounder_names,
    strata = "treat",
    test = F,
    data = _
  ) |> print(smd = T)

## IPW model -------
### first option -----
lalonde_data |> 
  lm(re78 ~ treat, data = _,
      weights = glm_model_2_data |> 
        pluck("weight"))
### second option ----
survey::svyglm(
  re78 ~ treat, data = lalonde_data,
  design = survey::svydesign(
    ids = ~1,
    data = lalonde_data,
    weights = glm_model_2_data |> pluck("weight")
  )
)
## truncate ------

### get the percentiles
glm_model_2_data |> 
  summarise(
    logne = quantile(weight, c(.01, .99))
  )

glm_model_2_data |> 
  filter(
    weight > 1.01 | weight < 12.6
  )

survey::svyglm(
  re78 ~ treat, data = lalonde_data,
  design = survey::svydesign(
    ids = ~1,
    data = lalonde_data,
    weights = glm_model_2_data |> pluck("weight")
  )
)
