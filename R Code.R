---
title: "Irene's Research Rotation Code"
author: "Irene Kimura Park"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(naniar)
library(expss)
library(survey)
library(gtsummary)
```



### National Survey on Drug Use and Health 2018 and 2019 Datasets
```{r Data Prep}
# Raw Datasets
load("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/Research Rotation/2018 NSDUH Release/NSDUH_2018.RData")
load("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/Research Rotation/2019 NSDUH Release/NSDUH_2019.RData")


# Function to Process Data
process_data <- function(raw, year) {
  output <- raw %>%
    # Select Variables for Analysis
    dplyr::select(usubjid = QUESTID2,
                  medmj_law = MEDMJPA2, 
                  religion_importance = snrlgimp, 
                  religious_decisions = snrldcsn, 
                  religious_friends = snrlfrnd, 
                  age = CATAG6, 
                  sex = irsex, 
                  race = NEWRACE2, 
                  marital_status = irmarit, 
                  education = IREDUHIGHST2, 
                  cigarettes = cigever, 
                  alcohol = alcever, 
                  mj_use = mjever, 
                  cluster = verep, 
                  strata = vestr, 
                  weights = ANALWT_C) %>%
    # Select Participants 18 or Older
    dplyr::filter(age >= 2) %>%
    # Convert Missing Data to NA
    naniar::replace_with_na_all(condition = ~.x %in% c(85, 94, 97, 98, 99)) %>% 
    # Exclude Observations with NA
    tidyr::drop_na() %>%
    # Add Year Variable, Recode and Factor Variables  
    dplyr::mutate(year = as.factor({{year}}), 
                  religion_importance = dplyr::recode_factor(religion_importance, 
                                                             "4" = "Strongly Agree", 
                                                             "3" = "Agree",
                                                             "2" = "Disagree",
                                                             "1" = "Strongly Disagree"),
                  religious_decisions = dplyr::recode_factor(religious_decisions,
                                                             "4" = "Strongly Agree", 
                                                             "3" = "Agree",
                                                             "2" = "Disagree", 
                                                             "1" = "Strongly Disagree"),
                  religious_friends = dplyr::recode_factor(religious_friends,
                                                           "4" = "Strongly Agree", 
                                                           "3" = "Agree",
                                                           "2" = "Disagree", 
                                                           "1" = "Strongly Disagree"),
                  age = dplyr::recode_factor(age, 
                                             "2" = "18-25 years", 
                                             "3" = "26-34 years", 
                                             "4" = "35-49 years", 
                                             "5" = "50-64 years", 
                                             "6" = ">65 years"),
                  sex = dplyr::recode_factor(sex, 
                                             "1" = "Male", 
                                             "2" = "Female"),
                  race = dplyr::recode_factor(race, 
                                              "1" = "White", 
                                              "2" = "Black", 
                                              "7" = "Hispanic", 
                                              "3" = "Other", 
                                              "4" = "Other", 
                                              "5" = "Other", 
                                              "6" = "Other"), 
                  marital_status = dplyr::recode_factor(marital_status, 
                                                        "1" = "Married", 
                                                        "2" = "Widowed", 
                                                        "3" = "Divorced/Separated", 
                                                        "4" = "Never Married"), 
                   education = dplyr::recode_factor(education, 
                                                    "1" = "High School Not Completed", 
                                                    "2" = "High School Not Completed", 
                                                    "3" = "High School Not Completed", 
                                                    "4" = "High School Not Completed", 
                                                    "5" = "High School Not Completed", 
                                                    "6" = "High School Not Completed", 
                                                    "7" = "High School Not Completed", 
                                                    "8" = "High School Diploma", 
                                                    "9" = "Some College", 
                                                    "10" = "Associate Degree", 
                                                    "11" = "College Graduate")) %>%
    # Convert to Binary Yes/No Variables
    dplyr::mutate_at(.vars = c("medmj_law", "cigarettes", "alcohol", "mj_use"), 
                     .funs = ~dplyr::recode_factor(., "2" = "No", "1" = "Yes")) %>%
    # Reorder Variables
    dplyr::select(-cluster, -strata, -weights, everything()) %>%
    # Label Variables
    expss::apply_labels(usubjid = "Subject Identification",
                        medmj_law = "State Medical MJ Law Passed", 
                        religion_importance = "My Religious Beliefs are Very Important", 
                        religious_decisions = "My Religion Influence My Decisions", 
                        religious_friends = "Important That Friends Share Religious Beliefs",
                        age = "Age Group", 
                        sex = "Sex", 
                        race = "Race", 
                        marital_status = "Marital Status", 
                        education = "Education", 
                        cigarettes = "Ever Smoked a Cigarette", 
                        alcohol = "Ever Had a Drink of Alcohol", 
                        mj_use = "Ever Used Marijuana",
                        year = "Year of Interview", 
                        cluster = "Primary Sampling Unit", 
                        strata = "Strata", 
                        weights = "Person-Level Weights")
  
  return(output)
}



drugs2018 <- process_data(PUF2018_100819, 2018) 
drugs2019 <- process_data(PUF2019_100920, 2019) 
drugs <- rbind(drugs2018, drugs2019)
```




```{r Final Weighted Dataset}
# Create Weighted Survey Data
drugs_weighted <- survey::svydesign(id = ~cluster, 
                                    strata = ~strata, 
                                    weights = ~weights,
                                    data = drugs,
                                    nest = TRUE) 



# Table 1 - Demographic Characteristics 
table1 <- drugs_weighted %>%
  gtsummary::tbl_svysummary(
    by = medmj_law, 
    include = c(religion_importance, 
                religious_decisions, 
                religious_friends, 
                age, sex, race, marital_status, education,
                cigarettes, alcohol, mj_use, year), 
    type = gtsummary::all_dichotomous() ~ "categorical",
    percent = "column", 
    statistic = gtsummary::all_categorical() ~ "{n_unweighted} ({p}%)", 
    digits = gtsummary::all_categorical() ~ c(0, 2),
    label = list(religion_importance = "My Religious Beliefs are Very Important", 
                 religious_decisions = "My Religion Influence My Decisions", 
                 religious_friends = "Important That Friends Share Religious Beliefs",
                 age = "Age Group", 
                 sex = "Sex", 
                 race = "Race", 
                 marital_status = "Marital Status", 
                 education = "Education", 
                 cigarettes = "Ever Smoked a Cigarette", 
                 alcohol = "Ever Had a Drink of Alcohol",
                 mj_use = "Ever Used Marijuana",
                 year = "Year of Interview")
    ) %>%
  # Add Overall Column
  gtsummary::add_overall(last = TRUE) %>%
  # Calculate p-values
  gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 3)) %>%
  # Add Headers
  gtsummary::modify_header(
    update = list(
      label = "",
      stat_1 = "**State with No MML** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_2 = "**State with MML** \n  
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_0 = "**Overall** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')}")) %>%
  # Add Footnote
  gtsummary::remove_footnote_header(columns = gtsummary::everything()) %>%
  gtsummary::modify_footnote_header(footnote = "Percentages and p-values from weighted dataset.",
                                    columns = c(gtsummary::all_stat_cols(), p.value),
                                    replace = FALSE)
```  
  


### Weighted Logistic Regression
```{r}
# Function to Output Weighted Logistic Regression Results for Three Main Predictors
logistic_regression <- function(dataset, predictor){
  output <- dataset %>%
    survey::svyglm(
      stats::as.formula(paste("medmj_law ~", predictor, "+ age + sex + race + marital_status + 
                              education + cigarettes + alcohol + mj_use + year")), 
                   design = .,
                   family = "quasibinomial")

  summary <- summary(output)
  
  odds_ratio <- cbind(OR = stats::coef(output), stats::confint(output)) %>%
    exp() %>%
    round(4)
  
  return(list("Summary" = summary, "Odds Ratios" = odds_ratio))
}



# Religious Beliefs are Important
relimport <- logistic_regression(dataset = drugs_weighted, 
                                 predictor = "religious_decisions")

# Religion Influence Decisions 
reldecision <- logistic_regression(dataset = drugs_weighted, 
                                   predictor = "religion_importance")

# Important That Friends Share Religious Beliefs 
relfriend <- logistic_regression(dataset = drugs_weighted, 
                                 predictor = "religious_friends")
```



### Weighted Logistic Regression with Interaction between Race and Three Main Predictors
```{r}
# Function to Output Weighted Logistic Regression Results with Interaction Term 
interaction_logistic_regression <- function(dataset, predictor){
  output <- dataset %>%
    survey::svyglm(
      stats::as.formula(paste("medmj_law ~", predictor, "+ age + sex + race + marital_status + 
                              education + cigarettes + alcohol + mj_use + year + ", 
                              predictor, ":race")), 
                   design = .,
                   family = "quasibinomial")

  summary <- summary(output)
  
  odds_ratio <- cbind(OR = stats::coef(output), stats::confint(output)) %>%
    exp() %>%
    round(4)
  
  return(list("Summary" = summary, "Odds Ratios" = odds_ratio))
}



# Religious Beliefs are Important 
relimport_interaction <- interaction_logistic_regression(dataset = drugs_weighted,
                                                         predictor = "religious_decisions")

# Religion Influence Decisions 
reldecision_interaction <- interaction_logistic_regression(dataset = drugs_weighted, 
                                                           predictor = "religion_importance")

# Important That Friends Share Religious Beliefs 
relfriend_interaction <- interaction_logistic_regression(dataset = drugs_weighted, 
                                                         predictor = "religious_friends")
```



### Stratifying Weighted Dataset
```{r}
# Race-Stratified Datasets
drugs_white_weighted <- subset(drugs_weighted, race=="White")
drugs_black_weighted <- subset(drugs_weighted, race=="Black")
drugs_hispanic_weighted <- subset(drugs_weighted, race=="Hispanic")
drugs_other_weighted <- subset(drugs_weighted, race=="Other")



# Demographic Characteristics of Race-Stratified Datasets
demographics_race_stratified <- function(stratified_dataset){
  stratified_dataset %>%
  gtsummary::tbl_svysummary(
    by = medmj_law, 
    include = c(religion_importance, 
                religious_decisions, 
                religious_friends, 
                age, sex, marital_status, education,
                cigarettes, alcohol, mj_use, year), 
    type = gtsummary::all_dichotomous() ~ "categorical",
    percent = "column", 
    statistic = gtsummary::all_categorical() ~ "{n_unweighted} ({p}%)", 
    digits = gtsummary::all_categorical() ~ c(0, 2),
    label = list(
      religion_importance = "My Religious Beliefs are Very Important", 
      religious_decisions = "My Religion Influence My Decisions", 
      religious_friends = "Important That Friends Share Religious Beliefs",
      age = "Age Group", 
      sex = "Sex", 
      marital_status = "Marital Status", 
      education = "Education", 
      cigarettes = "Ever Smoked a Cigarette", 
      alcohol = "Ever Had a Drink of Alcohol",
      mj_use = "Ever Used Marijuana",
      year = "Year of Interview"
      )
    ) %>%
  # Add Overall Column
  gtsummary::add_overall(last = TRUE) %>%
  # Calculate p-values
  gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 3)) %>%
  # Add Headers
  gtsummary::modify_header(
    update = list(
      label = "",
      stat_1 = "**State with No MML** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_2 = "**State with MML** \n  
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_0 = "**Overall** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')}")) %>%
  # Add Footnote
  gtsummary::remove_footnote_header(columns = gtsummary::everything()) %>%
  gtsummary::modify_footnote_header(footnote = "Percentages and p-values from weighted dataset.",
                                    columns = c(gtsummary::all_stat_cols(), p.value),
                                    replace = FALSE)
}



# Descriptive Statistics of White Participants 
white_demographics <- demographics_race_stratified(stratified_dataset = drugs_white_weighted)

# Descriptive Statistics of Black Participants 
black_demographics <- demographics_race_stratified(stratified_dataset = drugs_black_weighted)

# Descriptive Statistics of Hispanic Participants 
hispanic_demographics <- demographics_race_stratified(stratified_dataset = drugs_hispanic_weighted)

# Descriptive Statistics of Other Participants 
other_demographics <- demographics_race_stratified(stratified_dataset = drugs_other_weighted)
```



### Race-Stratified Weighted Logisitic Regressions
```{r}
# Function to Output Weighted Logistic Regression Results for Three Main Predictors
stratified_logistic_regression <- function(stratified_dataset, predictor){
  output <- stratified_dataset %>%
    survey::svyglm(
      stats::as.formula(paste("medmj_law ~", predictor, "+ age + sex + marital_status + 
                              education + cigarettes + alcohol + mj_use + year")), 
                   design = .,
                   family = "quasibinomial")

  summary <- summary(output)
  
  odds_ratio <- cbind(OR = stats::coef(output), stats::confint(output)) %>%
    exp() %>%
    round(4)
  
  return(list("Summary" = summary, "Odds Ratios" = odds_ratio))
}



# Weighted Logistic Regressions for White Participants
relimport_white <- stratified_logistic_regression(stratified_dataset = drugs_white_weighted, 
                                                  predictor = "religious_decisions")
reldecision_white <- stratified_logistic_regression(stratified_dataset = drugs_white_weighted, 
                                                    predictor = "religion_importance")
relfriend_white <- stratified_logistic_regression(stratified_dataset = drugs_white_weighted, 
                                                  predictor = "religious_friends")



# Weighted Logistic Regressions for Black Participants
relimport_black <- stratified_logistic_regression(stratified_dataset = drugs_black_weighted, 
                                                  predictor = "religious_decisions")
reldecision_black <- stratified_logistic_regression(stratified_dataset = drugs_black_weighted, 
                                                    predictor = "religion_importance")
relfriend_black <- stratified_logistic_regression(stratified_dataset = drugs_black_weighted, 
                                                  predictor = "religious_friends")



# Weighted Logistic Regressions for Hispanic Participants
relimport_hispanic <- stratified_logistic_regression(stratified_dataset = drugs_hispanic_weighted, 
                                                  predictor = "religious_decisions")
reldecision_hispanic <- 
  stratified_logistic_regression(stratified_dataset = drugs_hispanic_weighted, 
                                 predictor = "religion_importance")
relfriend_hispanic <- stratified_logistic_regression(stratified_dataset = drugs_hispanic_weighted, 
                                                     predictor = "religious_friends")



# Weighted Logistic Regressions for Other Participants
relimport_other <- stratified_logistic_regression(stratified_dataset = drugs_other_weighted, 
                                                  predictor = "religious_decisions")
reldecision_other <- stratified_logistic_regression(stratified_dataset = drugs_other_weighted, 
                                                    predictor = "religion_importance")
relfriend_other <- stratified_logistic_regression(stratified_dataset = drugs_other_weighted, 
                                                  predictor = "religious_friends")
```
