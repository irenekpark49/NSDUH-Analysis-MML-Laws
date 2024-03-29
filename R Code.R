---
title: "Irene's Research Rotation Code"
author: "Irene Kimura Park"
date: ''
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(naniar)
library(expss)
library(table1)
library(aod)
library(survey)
```


# 2018 Dataset
```{r}
load("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/Research Rotation/2018 NSDUH Release/NSDUH_2018.RData")

drugs2018 <- PUF2018_100819 %>% 
#Selecting Variables for Analysis
  dplyr::select(medmj_law = MEDMJPA2, 
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
#Selecting Participants 18 or Older
  filter(age>=2) %>%
#Converting Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(85, 94, 97, 98, 99)) %>% 
#Excluding Observations with NA
  na.omit() %>% 
#Adding Year Variable & Recoding Variables  
  mutate(year = 2018, 
         religion_importance = recode_factor(religion_importance, 
                                             "4"="Strongly Agree", 
                                             "3"="Agree",
                                             "2"="Disagree", 
                                             "1"="Strongly Disagree"),
         religious_decisions = recode_factor(religious_decisions,
                                             "4"="Strongly Agree", 
                                             "3"="Agree",
                                             "2"="Disagree", 
                                             "1"="Strongly Disagree"),
         religious_friends = recode_factor(religious_friends,
                                           "4"="Strongly Agree", 
                                           "3"="Agree",
                                           "2"="Disagree", 
                                           "1"="Strongly Disagree"),
         age = recode_factor(age, 
                             "2"="18-25 years", 
                             "3"="26-34 years", 
                             "4"="35-49 years", 
                             "5"="50-64 years", 
                             "6"=">65 years"),
         sex = recode_factor(sex, "1"="Male", "2"="Female"),
         race = recode_factor(race, 
                              "1"="White", 
                              "2"="Black", 
                              "7"="Hispanic", 
                              "3"="Other", 
                              "4"="Other", 
                              "5"="Other", 
                              "6"="Other"), 
         marital_status = recode_factor(marital_status, 
                                        "1"="Married", 
                                        "2"="Widowed", 
                                        "3"="Divorced/Separated", 
                                        "4"="Never Married"), 
         education = recode_factor(education, 
                                   "1"="High School Not Completed", 
                                   "2"="High School Not Completed", 
                                   "3"="High School Not Completed", 
                                   "4"="High School Not Completed", 
                                   "5"="High School Not Completed", 
                                   "6"="High School Not Completed", 
                                   "7"="High School Not Completed", 
                                   "8"="High School Diploma", 
                                   "9"="Some College", 
                                   "10"="Associate Degree", 
                                   "11"="College Graduate")
         ) %>%
#Reassign Values to Binary Variables
  mutate_at(c("medmj_law", "cigarettes", "alcohol", "mj_use"), 
            list(~dplyr::recode(., "2"="No", .default="Yes"))) %>%
#Factoring Variables
  mutate(across(.cols=-weights, .fns=as.factor)) %>%
#Labeling Variables
  apply_labels(medmj_law = "State Medical MJ Law Passed", 
               religion_importance = "My Religious Beliefs are Very Important", 
               religious_decisions = "My Religion Influence My Decisions", 
               religious_friends = "Important That Friends Share Religious Beliefs",
               age = "Age Category", 
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

#Checking Sum of Weights Against Target Population
sum(drugs2018$weights)
```



# 2019 Dataset
```{r}
load("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/Research Rotation/2019 NSDUH Release/NSDUH_2019.RData")

drugs2019 <- PUF2019_100920 %>% 
#Selecting Variables for Analysis
  dplyr::select(medmj_law = MEDMJPA2, 
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
#Selecting Participants 18 or Older
  filter(age>=2) %>%
#Converting Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(85, 94, 97, 98, 99)) %>% 
#Excluding Observations with NA
  na.omit() %>% 
#Adding Year Variable & Recoding Variables  
  mutate(year = 2019, 
         religion_importance = recode_factor(religion_importance, 
                                             "4"="Strongly Agree", 
                                             "3"="Agree",
                                             "2"="Disagree", 
                                             "1"="Strongly Disagree"),
         religious_decisions = recode_factor(religious_decisions,
                                             "4"="Strongly Agree", 
                                             "3"="Agree",
                                             "2"="Disagree", 
                                             "1"="Strongly Disagree"),
         religious_friends = recode_factor(religious_friends,
                                           "4"="Strongly Agree", 
                                           "3"="Agree",
                                           "2"="Disagree", 
                                           "1"="Strongly Disagree"),
         age = recode_factor(age, 
                             "2"="18-25 years", 
                             "3"="26-34 years", 
                             "4"="35-49 years", 
                             "5"="50-64 years", 
                             "6"=">65 years"),
         sex = recode_factor(sex, "1"="Male", "2"="Female"),
         race = recode_factor(race, 
                              "1"="White", 
                              "2"="Black", 
                              "7"="Hispanic", 
                              "3"="Other", 
                              "4"="Other", 
                              "5"="Other", 
                              "6"="Other"), 
         marital_status = recode_factor(marital_status, 
                                        "1"="Married", 
                                        "2"="Widowed", 
                                        "3"="Divorced/Separated", 
                                        "4"="Never Married"), 
         education = recode_factor(education, 
                                   "1"="High School Not Completed", 
                                   "2"="High School Not Completed", 
                                   "3"="High School Not Completed", 
                                   "4"="High School Not Completed", 
                                   "5"="High School Not Completed", 
                                   "6"="High School Not Completed", 
                                   "7"="High School Not Completed", 
                                   "8"="High School Diploma", 
                                   "9"="Some College", 
                                   "10"="Associate Degree", 
                                   "11"="College Graduate")
         ) %>%
#Reassign Values to Binary Variables
  mutate_at(c("medmj_law", "cigarettes", "alcohol", "mj_use"), 
            list(~dplyr::recode(., "2"="No", .default="Yes"))) %>%
#Factoring Variables
  mutate(across(.cols=-weights, .fns=as.factor)) %>%
#Labeling Variables
  apply_labels(medmj_law = "State Medical MJ Law Passed", 
               religion_importance = "My Religious Beliefs are Very Important", 
               religious_decisions = "My Religion Influence My Decisions", 
               religious_friends = "Important That Friends Share Religious Beliefs",
               age = "Age Category", 
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

#Checking Sum of Weights Against Target Population
sum(drugs2019$weights)
```



# Merged Dataset
```{r}
#Merging Datasets
drugs <- rbind(drugs2018, drugs2019) %>%
#Reordering Variables
  select(-"cluster", -"strata", -"weights", everything()) %>%
#Labeling Variables
  apply_labels(medmj_law = "State Medical MJ Law Passed", 
               religion_importance = "My Religious Beliefs are Very Important", 
               religious_decisions = "My Religion Influence My Decisions", 
               religious_friends = "Important That Friends Share Religious Beliefs",
               age = "Age Category", 
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


#Descriptive Statistics
table_html <- table1(~religion_importance + religious_decisions + religious_friends + age + sex + race +  marital_status + education + cigarettes + alcohol + mj_use + year | medmj_law, data=drugs, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="Merged 2018 & 2019 Dataset")
```



# Weighted Datasets 
```{r}
drugs_weighted <- svydesign(id = ~cluster, 
                            strata = ~strata, 
                            weights = ~weights,
                            data = drugs,
                            nest = TRUE)
drugs_nomedmj_weighted <- subset(drugs_weighted, medmj_law=="No")
drugs_medmj_weighted <- subset(drugs_weighted, medmj_law=="Yes")


#Descriptive Statistics
overall_table_weighted <- lapply(names(drugs[,-c(14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_weighted))

overall_prop_weighted <- lapply(names(drugs[,-c(14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_weighted))*100, 2))


#nomedmjlaw_table 
nomedmj_table_weighted <- lapply(names(drugs[-c(1, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_nomedmj_weighted))

nomedmj_prop_weighted <- lapply(names(drugs[,-c(1, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_nomedmj_weighted))*100, 2))


#medmjlaw_table 
medmj_table_weighted <- lapply(names(drugs[-c(1, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_medmj_weighted))

medmj_prop_weighted <- lapply(names(drugs[,-c(1, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_medmj_weighted))*100, 2))


#Chi-Squared Tests
chisq_weighted <- lapply(names(drugs[-c(1, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_weighted))
pvalues_weighted <- lapply(names(drugs[-c(1, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_weighted)$p.value)
```



# Weighted Dataset Logistic Regression
```{r}
#Religious Beliefs are Important
relimport_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_weighted, family="binomial")

summary(relimport_weighted)
round(exp(cbind(OR = coef(relimport_weighted), confint(relimport_weighted))),4)



#Religion Influence Decisions 
reldecision_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_weighted, family="binomial")

summary(reldecision_weighted)
round(exp(cbind(OR = coef(reldecision_weighted), confint(reldecision_weighted))),4) 



#Important That Friends Share Religious Beliefs 
relfriend_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_weighted, family="binomial")

summary(relfriend_weighted)
round(exp(cbind(OR = coef(relfriend_weighted), confint(relfriend_weighted))),4)
```



# Checking Interaction with Race in Weighted Dataset
```{r}
#Religious Beliefs are Important 
relimport_race_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:race, design=drugs_weighted, family="binomial")

summary(relimport_race_weighted)
round(exp(cbind(OR = coef(relimport_race_weighted), confint(relimport_race_weighted))),4)



#Religion Influence Decisions 
reldecision_race_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:race, design=drugs_weighted, family="binomial")

summary(reldecision_race_weighted)
round(exp(cbind(OR = coef(reldecision_race_weighted), confint(reldecision_race_weighted))),4)



#Important That Friends Share Religious Beliefs 
relfriend_race_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_friends:race, design=drugs_weighted, family="binomial")

summary(relfriend_race_weighted)
round(exp(cbind(OR = coef(relfriend_race_weighted), confint(relfriend_race_weighted))),4)
```



# Stratifying Weighted Dataset
```{r}
#Race-Stratified Datasets
drugs_white_weighted <- subset(drugs_weighted, race=="White")
drugs_black_weighted <- subset(drugs_weighted, race=="Black")
drugs_hispanic_weighted <- subset(drugs_weighted, race=="Hispanic")
drugs_other_weighted <- subset(drugs_weighted, race=="Other")


#Race:White Descriptive Statistics
white_table_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_white_weighted))
white_prop_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_white_weighted))*100, 2))

nomedmj_white_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_white_weighted, medmj_law=="No")))
nomedmj_white_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_white_weighted, medmj_law=="No")))*100, 2))

medmj_white_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_white_weighted, medmj_law=="Yes")))
medmj_white_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_white_weighted, medmj_law=="Yes")))*100, 2))

chisq_white_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_white_weighted))
pvalues_white_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_white_weighted)$p.value)



#Race:Black Descriptive Statistics
black_table_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_black_weighted))
black_prop_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_black_weighted))*100, 2))

nomedmj_black_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_black_weighted, medmj_law=="No")))
nomedmj_black_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_black_weighted, medmj_law=="No")))*100, 2))

medmj_black_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_black_weighted, medmj_law=="Yes")))
medmj_black_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_black_weighted, medmj_law=="Yes")))*100, 2))

chisq_black_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_black_weighted))
pvalues_black_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_black_weighted)$p.value)



#Race:Hispanic Descriptive Statistics
hispanic_table_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_hispanic_weighted))
hispanic_prop_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_hispanic_weighted))*100, 2))

nomedmj_hispanic_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_hispanic_weighted, medmj_law=="No")))
nomedmj_hispanic_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_hispanic_weighted, medmj_law=="No")))*100, 2))

medmj_hispanic_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_hispanic_weighted, medmj_law=="Yes")))
medmj_hispanic_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_hispanic_weighted, medmj_law=="Yes")))*100, 2))

chisq_hispanic_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_hispanic_weighted))
pvalues_hispanic_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_hispanic_weighted)$p.value)



#Race:Other Descriptive Statistics
other_table_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_other_weighted))
other_prop_weighted <- lapply(names(drugs[,-c(7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_other_weighted))*100, 2))

nomedmj_other_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_other_weighted, medmj_law=="No")))
nomedmj_other_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_other_weighted, medmj_law=="No")))*100, 2))

medmj_other_table_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(drugs_other_weighted, medmj_law=="Yes")))
medmj_other_prop_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(drugs_other_weighted, medmj_law=="Yes")))*100, 2))

chisq_other_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_other_weighted))
pvalues_other_weighted <- lapply(names(drugs[-c(1, 7, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs_other_weighted)$p.value)
```



# Race-Stratified Weighted Analysis
```{r}
#Race:White Logistic Regressions
relimport_white_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_white_weighted, family="binomial")
summary(relimport_white_weighted)
round(exp(cbind(OR = coef(relimport_white_weighted), confint(relimport_white_weighted))),4)

reldecision_white_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_white_weighted, family="binomial")
summary(reldecision_white_weighted)
round(exp(cbind(OR = coef(reldecision_white_weighted), confint(reldecision_white_weighted))),4) 

relfriend_white_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_white_weighted, family="binomial")
summary(relfriend_white_weighted)
round(exp(cbind(OR = coef(relfriend_white_weighted), confint(relfriend_white_weighted))),4)



#Race:Black Logistic Regressions
relimport_black_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_black_weighted, family="binomial")
summary(relimport_black_weighted)
round(exp(cbind(OR = coef(relimport_black_weighted), confint(relimport_black_weighted))),4)

reldecision_black_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_black_weighted, family="binomial")
summary(reldecision_black_weighted)
round(exp(cbind(OR = coef(reldecision_black_weighted), confint(reldecision_black_weighted))),4)

relfriend_black_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_black_weighted, family="binomial")
summary(relfriend_black_weighted)
round(exp(cbind(OR = coef(relfriend_black_weighted), confint(relfriend_black_weighted))),4)



#Race:Hispanic Logistic Regressions
relimport_hispanic_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_hispanic_weighted, family="binomial")
summary(relimport_hispanic_weighted)
round(exp(cbind(OR = coef(relimport_hispanic_weighted), confint(relimport_hispanic_weighted))),4)

reldecision_hispanic_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_hispanic_weighted, family="binomial")
summary(reldecision_hispanic_weighted)
round(exp(cbind(OR = coef(reldecision_hispanic_weighted), confint(reldecision_hispanic_weighted))),4)

relfriend_hispanic_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_hispanic_weighted, family="binomial")
summary(relfriend_hispanic_weighted)
round(exp(cbind(OR = coef(relfriend_hispanic_weighted), confint(relfriend_hispanic_weighted))),4)



#Race:Other Logistic Regressions
relimport_other_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_other_weighted, family="binomial")
summary(relimport_other_weighted)
round(exp(cbind(OR = coef(relimport_other_weighted), confint(relimport_other_weighted))),4)

reldecision_other_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_other_weighted, family="binomial")
summary(reldecision_other_weighted)
round(exp(cbind(OR = coef(reldecision_other_weighted), confint(reldecision_other_weighted))),4)

relfriend_other_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, design=drugs_other_weighted, family="binomial")
summary(relfriend_other_weighted)
round(exp(cbind(OR = coef(relfriend_other_weighted), confint(relfriend_other_weighted))),4)
```




