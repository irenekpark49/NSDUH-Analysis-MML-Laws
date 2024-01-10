---
title: "Appendix"
author: "Irene Kimura Park"
date: ""
output:
  pdf_document: default
  html_document: default
---

### 2018 Dataset
```{r}
#Descriptive Statistics
table2018_html <- table1(~religion_importance + religious_decisions + religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use | medmj_law, data=drugs2018, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="2018 Dataset")

table2018_nomedmjlaw <- lapply(drugs2018[drugs2018$medmj_law=="No",2:12], 
       function(x) round(prop.table(table(x))*100, 2))
table2018_medmjlaw <- lapply(drugs2018[drugs2018$medmj_law=="Yes",2:12], 
       function(x) round(prop.table(table(x))*100, 2))
table2018_overall <- lapply(drugs2018[,-13], 
       function(x) round(prop.table(table(x))*100, 2))


#Inspecting Dataset
lapply(drugs2018, summary)
by(drugs2018, drugs2018$medmj_law, summary)
sum(is.na(drugs2018))
glimpse(drugs2018)
head(drugs2018)


#Chi-Squared Tests
table2018_chisq <- lapply(drugs2018[,2:12], 
       function(x) chisq.test(table(x, drugs2018$medmj_law)))
table2018_pvalues <- lapply(drugs2018[,2:12], 
       function(x) chisq.test(table(x, drugs2018$medmj_law))$p.value)
```



### 2019 Dataset
```{r}
#Descriptive Statistics
table2019_html <- table1(~religion_importance + religious_decisions + religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use | medmj_law, data=drugs2019, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="2019 Dataset")

table2019_nomedmjlaw <- lapply(drugs2019[drugs2019$medmj_law=="No",2:12], 
       function(x) round(prop.table(table(x))*100, 2))
table2019_medmjlaw <- lapply(drugs2019[drugs2019$medmj_law=="Yes",2:12], 
       function(x) round(prop.table(table(x))*100, 2))
table2019_overall <- lapply(drugs2019[,-13], 
       function(x) round(prop.table(table(x))*100, 2))

#Inspecting Dataset
lapply(drugs2019, summary)
by(drugs2019, drugs2018$medmj_law, summary)
sum(is.na(drugs2019))
glimpse(drugs2019)
head(drugs2019)

#Chi-Squared Tests
table2019_chisq <- lapply(drugs2019[,2:12], 
       function(x) chisq.test(table(x, drugs2019$medmj_law)))
table2019_pvalues <- lapply(drugs2019[,2:12], 
       function(x) chisq.test(table(x, drugs2019$medmj_law))$p.value)
```



### Merged Dataset
```{r}
#Inspecting Dataset
lapply(drugs, summary)
by(drugs, drugs$medmj_law, summary)
sum(drugs$weights)



#Descriptive Statistics
nomedmjlaw_table <- lapply(drugs[drugs$medmj_law=="No", -c(1, 14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
medmjlaw_table <- lapply(drugs[drugs$medmj_law=="Yes", -c(1, 14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
overall_table <- lapply(drugs[,-c(14:16)], function(x) round(prop.table(table(x))*100, 2))



#Chi-Squared Tests
chisq <- lapply(drugs[,-c(1, 14:16)], function(x) chisq.test(table(x, drugs$medmj_law)))
pvalues <- lapply(drugs[,-c(1, 14:16)], 
       function(x) chisq.test(table(x, drugs$medmj_law))$p.value)
```



# Weighted Datasets 
```{r}
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



### 2018 Dataset Logistic Regression
```{r}
###Confounders
religion2018_confounders <- glm(medmj_law ~ age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2018, family="binomial")



#Saturated Model
religion2018 <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2018, family="binomial")

summary(religion2018)
round(exp(cbind(OR = coef(religion2018), confint(religion2018))),4)



###Religious Beliefs are Important
relimport2018 <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2018, family="binomial")
summary(relimport2018)
round(exp(cbind(OR = coef(relimport2018), confint(relimport2018))),4)

relimport2018_crude <- glm(medmj_law ~ religion_importance, data=drugs2018, family="binomial")
summary(relimport2018_crude)
exp(cbind(OR = coef(relimport2018_crude), confint.default(relimport2018_crude))) 

#Global Model Test - Likelihood Ratio Test
relimport2018_nulldev <- summary(relimport2018)$null.deviance
relimport2018_residdev <- summary(relimport2018)$deviance
relimport2018_chi <- relimport2018_nulldev - relimport2018_residdev
pchisq(relimport2018_chi, df=9, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(relimport2018), Sigma=vcov(relimport2018), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2018_confounders, relimport2018)
relimport2018_anova_chi <- anova(religion2018_confounders, relimport2018)$Deviance
pchisq(relimport2018_anova_chi, df=1, lower.tail=FALSE)



###Religion Influence Decisions 
reldecision2018 <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2018, family="binomial")
summary(reldecision2018)
round(exp(cbind(OR = coef(reldecision2018), confint(reldecision2018))),4)

reldecision2018_crude <- glm(medmj_law ~ religious_decisions, data=drugs2018, family="binomial")
summary(reldecision2018_crude)
exp(cbind(OR = coef(reldecision2018_crude),confint.default(reldecision2018_crude)))

#Global Model Test - Likelihood Ratio Test
reldecision2018_nulldev <- summary(reldecision2018)$null.deviance
reldecision2018_residdev <- summary(reldecision2018)$deviance
reldecision2018_chi <- reldecision2018_nulldev - reldecision2018_residdev
pchisq(reldecision2018_chi, df=9, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(reldecision2018), Sigma=vcov(reldecision2018), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2018_confounders, reldecision2018)
reldecision2018_anova_chi <- anova(religion2018_confounders, reldecision2018)$Deviance
pchisq(reldecision2018_anova_chi, df=1, lower.tail=FALSE)



###Important That Friends Share Religious Beliefs
relfriend2018 <- glm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2018, family="binomial")
summary(relfriend2018)
round(exp(cbind(OR = coef(relfriend2018), confint(relfriend2018))),4) 

relfriend2018_crude <- glm(medmj_law ~ religious_friends, data=drugs2018, family="binomial")
summary(relfriend2018_crude)
exp(cbind(OR = coef(relfriend2018_crude), confint.default(relfriend2018_crude))) 

#Global Model Test - Likelihood Ratio Test
relfriend2018_nulldev <- summary(relfriend2018)$null.deviance
relfriend2018_residdev <- summary(relfriend2018)$deviance
relfriend2018_chi <- relfriend2018_nulldev - relfriend2018_residdev
pchisq(relfriend2018_chi, df=9, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(relfriend2018), Sigma=vcov(relfriend2018), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2018_confounders, relfriend2018)
relfriend2018_anova_chi <- anova(religion2018_confounders, relfriend2018)$Deviance
pchisq(relfriend2018_anova_chi, df=1, lower.tail=FALSE)
```



### 2019 Dataset Logistic Regression
```{r}
###Confounders
religion2019_confounders <- glm(medmj_law ~ age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2019, family="binomial")



#Saturated Model
religion2019 <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2019, family="binomial")

summary(religion2019)
round(exp(cbind(OR = coef(religion2019), confint(religion2019))),4)



#Religious Beliefs are Important
relimport2019 <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2019, family="binomial")
summary(relimport2019)
round(exp(cbind(OR = coef(relimport2019), confint(relimport2019))),4)

relimport2019_crude <- glm(medmj_law ~ religion_importance, data=drugs2019, family="binomial")
summary(relimport2019_crude)
exp(cbind(OR = coef(relimport2019_crude), confint.default(relimport2019_crude))) 

#Global Model Test - Likelihood Ratio Test
relimport2019_nulldev <- summary(relimport2019)$null.deviance
relimport2019_residdev <- summary(relimport2019)$deviance
relimport2019_chi <- relimport2019_nulldev - relimport2019_residdev
pchisq(relimport2019_chi, df=8, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(relimport2019), Sigma=vcov(relimport2019), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2019_confounders, relimport2019)
relimport2019_anova_chi <- anova(religion2019_confounders, relimport2019)$Deviance
pchisq(relimport2019_anova_chi, df=1, lower.tail=FALSE)



#Religion Influence Decisions 
reldecision2019 <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2019, family="binomial")
summary(reldecision2019)
round(exp(cbind(OR = coef(reldecision2019), confint(reldecision2019))),4)

reldecision2019_crude <- glm(medmj_law ~ religious_decisions, data=drugs2019, family="binomial")
summary(reldecision2019_crude)
exp(cbind(OR = coef(reldecision2019_crude), confint.default(reldecision2019_crude))) 

#Global Model Test - Likelihood Ratio Test
reldecision2019_nulldev <- summary(reldecision2019)$null.deviance
reldecision2019_residdev <- summary(reldecision2019)$deviance
reldecision2019_chi <- reldecision2019_nulldev - reldecision2019_residdev
pchisq(reldecision2019_chi, df=8, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(reldecision2019), Sigma=vcov(reldecision2019), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2019_confounders, reldecision2019)
reldecision2019_anova_chi <- anova(religion2019_confounders, reldecision2019)$Deviance
pchisq(reldecision2019_anova_chi, df=1, lower.tail=FALSE)



#Important That Friends Share Religious Beliefs
relfriend2019 <- glm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, data=drugs2019, family="binomial")
summary(relfriend2019)
round(exp(cbind(OR = coef(relfriend2019), confint(relfriend2019))),4)

relfriend2019_crude <- glm(medmj_law ~ religious_friends, data=drugs2019, family="binomial")
summary(relfriend2019_crude)
exp(cbind(OR = coef(relfriend2019_crude), confint.default(relfriend2019_crude))) 

#Global Model Test - Likelihood Ratio Test
relfriend2019_nulldev <- summary(relfriend2019)$null.deviance
relfriend2019_residdev <- summary(relfriend2019)$deviance
relfriend2019_chi <- relfriend2019_nulldev - relfriend2019_residdev
pchisq(relfriend2019_chi, df=8, lower.tail=FALSE)

#Parameter Test - Wald Test
wald.test(b=coef(relfriend2019), Sigma=vcov(relfriend2019), Terms=2:4)

#Parameter Test - Partial Likelihood Ratio Test
anova(religion2019_confounders, relfriend2019)
relfriend2019_anova_chi <- anova(religion2019_confounders, relfriend2019)$Deviance
pchisq(relfriend2019_anova_chi, df=1, lower.tail=FALSE)
```



### Merged Dataset Logistic Regression
```{r}
###Confounders
religion_confounders <- glm(medmj_law ~ age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs, family="binomial")



#Saturated Model
religion <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs, family="binomial")

summary(religion)
round(exp(cbind(OR = coef(religion), confint(religion))),4)



#Religious Beliefs are Important
relimport <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs, family="binomial")

summary(relimport)
round(exp(cbind(OR = coef(relimport), confint(relimport))),4)



#Religion Influence Decisions 
reldecision <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs, family="binomial")

summary(reldecision)
round(exp(cbind(OR = coef(reldecision), confint(reldecision))),4) 



#Important That Friends Share Religious Beliefs 
relfriend <- glm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs, family="binomial")

summary(relfriend)
round(exp(cbind(OR = coef(relfriend), confint(relfriend))),4)
```



### Checking Interaction in Merged Dataset
```{r}
#Religious Beliefs are Important 
relimport_age <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:age, data=drugs, family="binomial")
summary(relimport_age)
exp(cbind(OR = coef(relimport_age), confint.default(relimport_age)))

relimport_sex <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:sex, data=drugs, family="binomial")
summary(relimport_sex)
exp(cbind(OR = coef(relimport_sex), confint.default(relimport_sex)))

relimport_race <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:race, data=drugs, family="binomial")
summary(relimport_race)
exp(cbind(OR = coef(relimport_race), confint(relimport_race))) 

relimport_education <- glm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:education, data=drugs, family="binomial")
summary(relimport_education)
exp(cbind(OR = coef(relimport_education), confint.default(relimport_education)))



#Religion Influence Decisions 
reldecision_age <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:age, data=drugs, family="binomial")
summary(reldecision_age)

reldecision_sex <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:sex, data=drugs, family="binomial")
summary(reldecision_sex)

reldecision_race <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:race, data=drugs, family="binomial")
summary(reldecision_race)
exp(cbind(OR = coef(reldecision_race), confint(reldecision_race))) 

reldecision_education <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:education, data=drugs, family="binomial")
summary(reldecision_education)
exp(cbind(OR = coef(reldecision_education), confint.default(reldecision_education)))



#Important That Friends Share Religious Beliefs 
relfriend_age <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:age, data=drugs, family="binomial")
summary(relfriend_age)
exp(cbind(OR = coef(relfriend_age), confint.default(relfriend_age)))

relfriend_sex <- glm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:sex, data=drugs, family="binomial")
summary(relfriend_sex)
exp(cbind(OR = coef(relfriend_sex), confint.default(relfriend_sex)))


relfriend_race <- glm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_friends:race, data=drugs, family="binomial")
summary(relfriend_race)
exp(cbind(OR = coef(relfriend_race), confint(relfriend_race))) 

relfriend_education <- glm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_friends:education, data=drugs, family="binomial")
summary(relfriend_education)
exp(cbind(OR = coef(relfriend_education), confint.default(relfriend_education)))
```



### Stratifying Merged Dataset
```{r}
#Race-Stratified Datasets
drugs_white <- drugs[drugs$race=="White",]
drugs_black <- drugs[drugs$race=="Black",]
drugs_hispanic <- drugs[drugs$race=="Hispanic",] 
drugs_other <- drugs[drugs$race=="Other",]



#Race:White Descriptive Statistics
white_html <- table1(~religion_importance + religious_decisions + religious_friends + age + sex +  marital_status + education + cigarettes + alcohol + mj_use + year | medmj_law, data=drugs_white, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="Race: White")

nonmedmjlaw_white_table <- lapply(drugs_white[drugs_white$medmj_law=="No",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
medmjlaw_white_table <- lapply(drugs_white[drugs_white$medmj_law=="Yes",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
overall_white_table <- lapply(drugs_white[,-c(7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))

chisq_white <- lapply(drugs_white[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_white$medmj_law)))
pvalues_white <- lapply(drugs_white[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_white$medmj_law))$p.value)



#Race:Black Descriptive Statistics
table_black <- table1(~religion_importance + religious_decisions + religious_friends + age + sex +  marital_status + education + cigarettes + alcohol + mj_use + year | medmj_law, data=drugs_black, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="Race: Black")

nonmedmjlaw_black_table <- lapply(drugs_black[drugs_black$medmj_law=="No",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
medmjlaw_black_table <- lapply(drugs_black[drugs_black$medmj_law=="Yes",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
overall_black_table <- lapply(drugs_black[,-c(7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))

chisq_black <- lapply(drugs_black[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_black$medmj_law)))
pvalues_black <- lapply(drugs_black[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_black$medmj_law))$p.value)



#Race:Hispanic Descriptive Statistics
table_hispanic <- table1(~religion_importance + religious_decisions + religious_friends + age + sex +  marital_status + education + cigarettes + alcohol + mj_use + year | medmj_law, data=drugs_hispanic, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="Race: Hispanic")

nonmedmjlaw_hispanic_table <- lapply(drugs_hispanic[drugs_hispanic$medmj_law=="No",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
medmjlaw_hispanic_table <- lapply(drugs_hispanic[drugs_hispanic$medmj_law=="Yes",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
overall_hispanic_table <- lapply(drugs_hispanic[,-c(7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))

chisq_hispanic <- lapply(drugs_hispanic[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_hispanic$medmj_law)))
pvalues_hispanic <- lapply(drugs_hispanic[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_hispanic$medmj_law))$p.value)



#Race:Other Descriptive Statistics
table_other <- table1(~religion_importance + religious_decisions + religious_friends + age + sex +  marital_status + education + cigarettes + alcohol + mj_use + year | medmj_law, data=drugs_other, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="Race: Other")

nonmedmjlaw_other_table <- lapply(drugs_other[drugs_other$medmj_law=="No",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
medmjlaw_other_table <- lapply(drugs_other[drugs_other$medmj_law=="Yes",-c(1,7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))
overall_other_table <- lapply(drugs_other[,-c(7,14:16)], 
       function(x) round(prop.table(table(x))*100, 2))

chisq_other <- lapply(drugs_other[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_other$medmj_law)))
pvalues_other <- lapply(drugs_other[,-c(1,7,14:16)], 
       function(x) chisq.test(table(x,drugs_other$medmj_law))$p.value)
```



### Race-Stratified Analysis
```{r}
#Saturated Models
religion_white <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_white, family="binomial")
summary(religion_white)
round(exp(cbind(OR = coef(religion_white), confint(religion_white))),4)

religion_black <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_black, family="binomial")
summary(religion_black)
round(exp(cbind(OR = coef(religion_black), confint(religion_black))),4)

religion_hispanic <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_hispanic, family="binomial")
summary(religion_hispanic)
round(exp(cbind(OR = coef(religion_hispanic), confint(religion_hispanic))),4)

religion_other <- glm(medmj_law ~ religion_importance + religious_decisions + religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_other, family="binomial")
summary(religion_other)
round(exp(cbind(OR = coef(religion_other), confint(religion_other))),4)



#Race:White Logistic Regressions
relimport_white <- glm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_white, family="binomial")
summary(relimport_white)
round(exp(cbind(OR = coef(relimport_white), confint(relimport_white))),4)

reldecision_white <- glm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_white, family="binomial")
summary(reldecision_white)
round(exp(cbind(OR = coef(reldecision_white), confint(reldecision_white))),4) 

relfriend_white <- glm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_white, family="binomial")
summary(relfriend_white)
round(exp(cbind(OR = coef(relfriend_white), confint(relfriend_white))),4)



#Race:Black Logistic Regressions
relimport_black <- glm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_black, family="binomial")
summary(relimport_black)
round(exp(cbind(OR = coef(relimport_black), confint(relimport_black))),4)

reldecision_black <- glm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_black, family="binomial")
summary(reldecision_black)
round(exp(cbind(OR = coef(reldecision_black), confint(reldecision_black))),4)

relfriend_black <- glm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_black, family="binomial")
summary(relfriend_black)
round(exp(cbind(OR = coef(relfriend_black), confint(relfriend_black))),4)



#Race:Hispanic Logistic Regressions
relimport_hispanic <- glm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_hispanic, family="binomial")
summary(relimport_hispanic)
round(exp(cbind(OR = coef(relimport_hispanic), confint(relimport_hispanic))),4)

reldecision_hispanic <- glm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_hispanic, family="binomial")
summary(reldecision_hispanic)
round(exp(cbind(OR = coef(reldecision_hispanic), confint(reldecision_hispanic))),4)

relfriend_hispanic <- glm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_hispanic, family="binomial")
summary(relfriend_hispanic)
round(exp(cbind(OR = coef(relfriend_hispanic), confint(relfriend_hispanic))),4)



#Race:Other Logistic Regressions
relimport_other <- glm(medmj_law ~ religion_importance + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_other, family="binomial")
summary(relimport_other)
round(exp(cbind(OR = coef(relimport_other), confint(relimport_other))),4)

reldecision_other <- glm(medmj_law ~ religious_decisions + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_other, family="binomial")
summary(reldecision_other)
round(exp(cbind(OR = coef(reldecision_other), confint(reldecision_other))),4)

relfriend_other <- glm(medmj_law ~ religious_friends + age + sex + marital_status + education + cigarettes + alcohol + mj_use + year, data=drugs_other, family="binomial")
summary(relfriend_other)
round(exp(cbind(OR = coef(relfriend_other), confint(relfriend_other))),4)
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



### Checking Interaction in Weighted Dataset
```{r}
#Religious Beliefs are Important 
relimport_age_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:age, design=drugs_weighted, family="binomial")
summary(relimport_age_weighted)
exp(cbind(OR = coef(relimport_age), confint.default(relimport_age_weighted)))

relimport_sex_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:sex, design=drugs_weighted, family="binomial")
summary(relimport_sex_weighted)
exp(cbind(OR = coef(relimport_sex_weighted), confint.default(relimport_sex_weighted)))

relimport_education_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religion_importance:education, design=drugs_weighted, family="binomial")
summary(relimport_education_weighted)
exp(cbind(OR = coef(relimport_education_weighted), confint.default(relimport_education_weighted)))



#Religion Influence Decisions 
reldecision_age_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:age, design=drugs_weighted, family="binomial")
summary(reldecision_age_weighted)
exp(cbind(OR = coef(reldecision_age_weighted), confint.default(reldecision_age_weighted)))

reldecision_sex_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:sex, design=drugs_weighted, family="binomial")
summary(reldecision_sex_weighted)
exp(cbind(OR = coef(reldecision_sex_weighted), confint.default(reldecision_sex_weighted)))

reldecision_education_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:education, design=drugs_weighted, family="binomial")
summary(reldecision_education_weighted)
exp(cbind(OR = coef(reldecision_education_weighted), confint.default(reldecision_education_weighted)))



#Important That Friends Share Religious Beliefs 
relfriend_age_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:age, design=drugs_weighted, family="binomial")
summary(relfriend_age_weighted)
exp(cbind(OR = coef(relfriend_age_weighted), confint.default(relfriend_age_weighted)))

relfriend_sex <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_decisions:sex, design=drugs_weighted, family="binomial")
summary(relfriend_sex)
exp(cbind(OR = coef(relfriend_sex), confint.default(relfriend_sex)))

relfriend_education <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + year + religious_friends:education, design=drugs_weighted, family="binomial")
summary(relfriend_education)
exp(cbind(OR = coef(relfriend_education), confint.default(relfriend_education)))
```



### 2020 Dataset
```{r}
load("C:/Irene Hsueh's Documents/MS Applied Biostatistics/Research Rotation/2020 NSDUH Release/NSDUH_2020.Rdata")

#Selecting Variables for Analysis
drugs2020 <- NSDUH_2020 %>% 
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
                strata = VESTRQ1Q4_C, 
                weights = ANALWTQ1Q4_C) %>%
#Selecting Participants 18 or Older
  filter(age>=2) %>%
#Converting Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(85, 94, 97, 98, 99)) %>% 
#Excluding Observations with NA
  na.omit() %>% 
#Adding Year Variable & Recoding Variables  
  mutate(year = 2020, 
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
```



### Weighted 2020 Dataset 
```{r}
drugs2020_weighted <- svydesign(id = ~cluster, 
                                 strata = ~strata, 
                                 weights = ~weights,
                                 data = drugs2020,
                                 nest = TRUE)
drugs_nomedmj2020_weighted <- subset(drugs2020_weighted, medmj_law=="No")
drugs_medmj2020_weighted <- subset(drugs2020_weighted, medmj_law=="Yes")


#Descriptive Statistics
overall_table2020_weighted <- lapply(names(drugs2020[,-c(13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs2020_weighted))

overall_prop2020_weighted <- lapply(names(drugs2020[,-c(13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs2020_weighted))*100, 2))


#nomedmjlaw_table 
nomedmj_table2020_weighted <- lapply(names(drugs2020[-c(1, 13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_nomedmj2020_weighted))

nomedmj_prop2020_weighted <- lapply(names(drugs2020[,-c(1, 13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_nomedmj2020_weighted))*100, 2))


#medmjlaw_table 
medmj_table2020_weighted <- lapply(names(drugs2020[-c(1, 13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=drugs_medmj2020_weighted))

medmj_prop2020_weighted <- lapply(names(drugs2020[,-c(1, 13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=drugs_medmj2020_weighted))*100, 2))

 
#Chi-Squared Tests
chisq2020_weighted <- lapply(names(drugs2020[-c(1, 13:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs2020_weighted))
pvalues2020_weighted <- lapply(names(drugs2020[-c(1, 13:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = drugs2020_weighted)$p.value)
```



### Weighted 2020 Dataset Logistic Regression
```{r}
#Religious Beliefs are Important
relimport2020_weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=drugs2020_weighted, family="binomial")

summary(relimport2020_weighted)
round(exp(cbind(OR = coef(relimport2020_weighted), confint(relimport2020_weighted))),4)



#Religion Influence Decisions 
reldecision2020_weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=drugs2020_weighted, family="binomial")

summary(reldecision2020_weighted)
round(exp(cbind(OR = coef(reldecision2020_weighted), confint(reldecision2020_weighted))),4) 



#Important That Friends Share Religious Beliefs 
relfriend2020_weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=drugs2020_weighted, family="binomial")

summary(relfriend2020_weighted)
round(exp(cbind(OR = coef(relfriend2020_weighted), confint(relfriend2020_weighted))),4)
```



### Checking Interaction in Weighted 2020 Dataset
```{r}
#Religious Beliefs are Important
relimport_age_2020weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religion_importance:age, design=drugs2020_weighted, family="binomial")
summary(relimport_age_2020weighted)

relimport_sex_2020weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religion_importance:sex, design=drugs2020_weighted, family="binomial")
summary(relimport_sex_2020weighted)

relimport_race_2020weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religion_importance:race, design=drugs2020_weighted, family="binomial")
summary(relimport_race_2020weighted)

relimport_education_2020weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religion_importance:education, design=drugs2020_weighted, family="binomial")
summary(relimport_education_2020weighted)

relimport_mjuse_2020weighted <- svyglm(medmj_law ~ religion_importance + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religion_importance:mj_use, design=drugs2020_weighted, family="binomial")
summary(relimport_mjuse_2020weighted)



#Stolen/Tried to reldecision Anything Worth >$50
reldecision_age_2020weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_decisions:age, design=drugs2020_weighted, family="binomial")
summary(reldecision_age_2020weighted)

reldecision_sex_2020weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_decisions:sex, design=drugs2020_weighted, family="binomial")
summary(reldecision_sex_2020weighted)

reldecision_race_2020weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_decisions:race, design=drugs2020_weighted, family="binomial")
summary(reldecision_race_2020weighted)

reldecision_education_2020weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_decisions:education, design=drugs2020_weighted, family="binomial")
summary(reldecision_education_2020weighted)

reldecision_mjuse_2020weighted <- svyglm(medmj_law ~ religious_decisions + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_decisions:mj_use, design=drugs2020_weighted, family="binomial")
summary(reldecision_mjuse_2020weighted)



#relfriended Someone With Intent to Seriously Hurt Them
relfriend_age_2020weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_friends:age, design=drugs2020_weighted, family="binomial")
summary(relfriend_age_2020weighted)

relfriend_sex_2020weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_friends:sex, design=drugs2020_weighted, family="binomial")
summary(relfriend_sex_2020weighted)

relfriend_race_2020weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_friends:race, design=drugs2020_weighted, family="binomial")
summary(relfriend_race_2020weighted)

relfriend_education_2020weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_friends:education, design=drugs2020_weighted, family="binomial")
summary(relfriend_education_2020weighted)

relfriend_mjuse_2020weighted <- svyglm(medmj_law ~ religious_friends + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + religious_friends:mj_use, design=drugs2020_weighted, family="binomial")
summary(relfriend_mjuse_2020weighted)
```

