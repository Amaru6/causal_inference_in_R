pacman::p_load(tidyverse, janitor)
library(MatchIt)
library(here)
# data --------
## training --------#
read_csv(
  "https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/trainees.csv"
) -> training_data
## mindset --------#
read_csv(
  "https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/learning_mindset.csv"
) -> mindset_data
mindset_data |> glimpse()
mindset_data |> 
  mutate(key = row_number()) |> 
  select(key, everything()) -> mindset_data
mindset_data |> glimpse()
#----------------#
mindset_data |> slice_sample(n = 5)

mindset_data |> 
  lm(achievement_score ~ intervention,data = _)

# Exact matching --------
training_data |> 
  lm(earnings ~ as.factor(trainees), data = _) # biased result

training_data |> 
  matchit(trainees ~ age, data = _,
          method = "exact", replace = T) -> resultados_matching
resultados_matching |> summary()
match.data(resultados_matching) |> 
  lm(earnings ~ as.factor(trainees),
     data = _, weights = weights)

# Coarsened exact matching ------
training_data |> 
  matchit(trainees ~ age, data = _,
          method = "cem") |> 
  match.data() |> arrange(subclass) |> View()


# Propensity score matching --------
## This will reduce the curse of dimentionality
library(tidymodels)

mindset_data |> 
  select(intervention, achievement_score,
         school_mindset,
         school_achievement,
         school_ethnic_minority,
         school_poverty,
         school_size) |> 
  glm(intervention ~ school_mindset + school_achievement + school_ethnic_minority +
          school_poverty + school_size, data = _ ,
      family = binomial(link = "logit")) -> reg_sapal

reg_sapal |> 
  broom::augment(newdata = 
            mindset_data |> 
            select(intervention, achievement_score,
                   school_mindset,
                   school_achievement,
                   school_ethnic_minority,
                   school_poverty,
                   school_size),
            type.predict = "response") -> rotar
rotar |> 
  filter(intervention == 1) |> 
  mutate(
    logne = 1/.fitted
  ) |> 
  summarise(
    logne_2 = sum(logne)
  )
rotar |> 
  nest(
    data = -intervention
  ) |> 
  mutate(logne = map2_dbl(.x = data,.y = intervention, .f = function(.x, .y){
    if(.y == 1){
      .x |> 
        mutate(logne = 1/.fitted) |> 
        summarise(
          logne_2 = sum(logne)
        ) |> pluck("logne_2")
    } else if(.y == 0){
      .x |> 
        mutate(
          logne = 1/(1 - .fitted)
        ) |> 
        summarise(
          logne_2 = sum(logne)
        ) |> pluck("logne_2")
    }} 
  ))


# coursera ---------------
library(Matching)
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
    meanbp1 = meanbp1
  ) -> coursera_data

coursera_data |> glimpse()
coursera_data |> 
  dplyr::select(
    starts_with("cat1"),
    female,
    died,
    age,
    treatment,
    meanbp1
  ) -> coursera_data

coursera_data

# Greedy Matching on Mahalanobis distance --------
## Coursera way ------------
coursera_data |> 
  (\(.)(Match(Tr = .$treatment,
            M = 1,
            X = . |> dplyr::select(
              starts_with("cat1"),"age","female", "meanbp1"
            ))))() -> greedy_match
greedy_match |> names()

greedy_match |> 
  (\(.)(keep(names(.) %in% c("index.treated",
                           "index.control"))))() # doesn't work ):
greedy_match %>%
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> unlist() -> index_data 

coursera_data[index_data, ] -> matched_data

matched_data |> 
  relocate(treatment, .before = 1) |> 
  dplyr::select(treatment, died) -> matched_data_sub

matched_data_sub |> 
  filter(treatment == 1) |> 
  pluck("died") -> y_var_1

matched_data_sub |> 
  filter(treatment == 0) |> 
  pluck("died") -> y_var_0

diff_y <- y_var_1 - y_var_0

t.test(diff_y)

## Julio way ---------
coursera_data |> 
  (\(.)(Match(Y = .$died,
              M = 1,
              X = . |> 
                dplyr::select(starts_with("cat1"),
                              "age", "female",
                              "meanbp1"),
              Tr = .$treatment
              
              )))() -> matched_effect
matched_effect |> names()
matched_effect |> str()
matched_effect |> pluck("est")
### check for balance
library(tableone)
matched_effect %>%
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> unlist() -> index_2
#### before matching -------------#
coursera_data |> 
  (\(.)(CreateTableOne(
    vars = coursera_data |> 
      dplyr::select(starts_with("cat1"),
                    "age", "female", "meanbp1") |> 
      colnames(),
    strata = "treatment",
    data = ., test = FALSE
  )))() |> 
  print(smd = T)

#### after matching ---------------#
coursera_data[index_2, ] |> 
  (\(.)(CreateTableOne(vars = coursera_data |> 
                   dplyr::select(starts_with("cat1"),
                                 "age", "female", "meanbp1") |> 
                     colnames(),
                 strata = "treatment",
                 data = ., test = FALSE)))() -> matched_tab_1
print(matched_tab_1, smd = TRUE)
### linear_reg
coursera_data |> 
  lm(died ~ treatment + cat1_arf +
       cat1_chf + cat1_cirrhosis + 
       cat1_colon_cancer + cat1_coma +
       cat1_copd + cat1_lung_cancer +
       cat1_mosf_w_malignancy +
       cat1_mosf_w_sepsis +
       age + female + meanbp1, data = _) |> 
  broom::tidy()

### kable table -----------
print(matched_tab_1$ContTable, smd = T) |> kableExtra::kbl() # Get it to kable

# Propensity score ------------
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
coursera_data |> 
  dplyr::select(starts_with("cat1"),
                age,
                female,
                meanbp1,
                aps,
                treatment,
                died,
                swang1) -> data_to_ps

data_to_ps |> glimpse()

## fit the propensity score itself ------#
data_to_ps |> 
  mutate(treatment = as.numeric(swang1 == "RHC")) |> 
  glm(treatment ~ cat1_arf + 
        cat1_chf +
        cat1_cirrhosis + 
        cat1_colon_cancer + 
        cat1_coma +
        cat1_lung_cancer + 
        cat1_mosf_w_malignancy + 
        cat1_mosf_w_sepsis + 
        age + 
        female +
        meanbp1 +
        aps, data = _,
      family = binomial(link = "logit")) -> ps_model
ps_model
ps_model |> 
  broom::augment(new_data = data_to_ps,
                 type.predict = "response") -> ps_model_data
ps_model

## check balance ---------#
ps_model_data |> 
  ggplot(aes(x = .fitted, fill = as.factor(treatment))) + 
  geom_density(alpha = .3)


## matching on the propensity score -----#
data_to_ps |> 
  (\(.)(Match(
    Y = .$died,
    X = ps_model_data$.fitted,
    M = 1,
    replace = F,
    Tr = .$treatment
  )))() -> ps_matching
ps_matching |> names()
ps_matching |> pluck("est")

## check balance_2 --------#

ps_matching %>%
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> 
  unlist() -> index_matching_ps 
data_to_ps[index_matching_ps,] |> 
  (\(.)(CreateTableOne(vars = . |> 
                       dplyr::select(starts_with("cat1"),
                              age,
                              female,
                              meanbp1,
                              aps) |> 
                         colnames(),
                     strata = "treatment",
                     data = .,
                     test = F)))() |> 
  print(smd = T) # Not totally satisfying... Maybe we can use a calliper!



## calliper to the rescue ------#
### find the calliper
ps_model |> 
  broom::augment(
    new_data = data_to_ps
  ) |> summarise(
    sd_sapal = sd(.fitted),
    calliper = .2*sd_sapal
  ) |> 
  pluck("calliper") -> calliper
###
data_to_ps |> 
  (\(.)(Match(
    Y = .$died,
    X = ps_model_data$.fitted,
    M = 1,
    replace = F,
    Tr = .$treatment,
    caliper = calliper
  )))() -> ps_matching_2

ps_matching_2 |> str()

### check balance again ------#
ps_matching_2 %>%
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> 
  unlist() -> index_treated_control_3

data_to_ps[index_treated_control_3, ] |> 
  (\(.)(CreateTableOne(vars = . |> 
                         dplyr::select(starts_with("cat1"),
                                       age,
                                       female,
                                       meanbp1,
                                       aps) |> 
                         colnames(),
                       strata = "treatment",
                       data = .,
                       test = F)))() |> 
  print(smd = T) # Some of the people we matched didn't make it because
# their distance was too much!


# data project ----------
MatchIt::lalonde |> 
  as_tibble() -> lalonde_data
lalonde_data |> 
  mutate(black = as.numeric(race == "black"),
         hispan = as.numeric(race == "hispan")) -> lalonde_data

lalonde_data |> 
  dplyr::select(
    age,
    educ,
    black,
    hispan,
    married,
    nodegree,
    re74,
    re75
  ) |> colnames() -> confounding_vars
## balance pre treatment -----#
lalonde_data |> 
  (\(.)(CreateTableOne(vars = c(confounding_vars, "re78"),
                     strata = "treat",
                     data = .,
                     test = F)))() |> 
  print(smd = T)

## diff y between treated an control ----#
lalonde_data |> 
  group_by(treat) |> 
  summarise(
    diff = mean(re78)
  ) |> 
  pivot_wider(names_from = treat,
              values_from = diff) |> 
  janitor::clean_names() |> 
  mutate(diff  = x1 - x0)

## fit a model ---#
lalonde_data |> 
  glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
      data = _,
      family = binomial("logit")) -> psm_model_project 

### tukey statistics on propensity score
psm_model_project |>   
  broom::augment(new_data = lalonde_data,
                 type.predict = "response") |> 
  skimr::skim(.fitted) 
psm_model_project |>   
  broom::augment(new_data = lalonde_data,
                 type.predict = "response") -> psm_model_project_fitted
## carry out the propensity score matching
set.seed(931139) # remember that order matters!
lalonde_data |> 
  (\(.)(Match(Y = .$re78,
            Tr = .$treat,
            X = psm_model_project_fitted$.fitted,
            M = 1,
            replace = F)))() -> matched_data_2
matched_data_2 %>% 
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> 
  unlist() -> index_project

lalonde_data[index_project, ] |> 
  (\(.)(CreateTableOne(vars = c(confounding_vars, "re78"),
                       strata = "treat",
                       data = .,
                       test = F)))() |> 
  print(smd = T)

## redo matching ------
set.seed(931139)
lalonde_data |> 
  (\(.)(Match(Y = .$re78,
              Tr = .$treat,
              X = psm_model_project_fitted$.fitted,
              M = 1,
              replace = F,
              caliper = .1)))() -> matched_data_3
matched_data_3 %>%
  keep(names(.) %in% c("index.treated",
                       "index.control")) |> 
  unlist() -> index_project_2
lalonde_data[index_project_2, ] |> count(treat) # number of pairs

## outcome analysis -------
matched_data_3 |> pluck("est")
lalonde_data[index_project_2, ] |> 
  mutate(treat = treat |> as.factor()) |>
  mutate(treat = treat |> fct_relevel("0", "1")) |> 
  (\(.)(t.test(.$re78 ~ .$treat)))()
