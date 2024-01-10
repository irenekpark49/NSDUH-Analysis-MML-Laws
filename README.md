# NSDUH-Analysis-MML-Laws
For the research rotation project requirement for my MS in Applied Biostatistics at Boston University, I used the NSDUH datasets to analyze the association between the importance of someone's religious beliefs and whether their state of residence had medical marijuana laws (MML). Datasets can be found at 
[2018 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2018-nsduh-2018-ds0001), 
[2019 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2019-nsduh-2019-ds0001),
[2020 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2020-nsduh-2020-ds0001).

I combined the 2018 and 2019 waves into one dataset and analyzed the 2020 wave separately because of different data collection procedures in 2020 due to the COVID-19 pandemic. The main outcome variable was a binary yes/no variable of whether respondents were living in a state that had a law allowing marijuana use for medical reasons at the time of the interview. The three explanatory variables of interest were 1) “Your religious beliefs are a very important part of your life” 2) “Your religious beliefs influence how you make decisions in your life” and 3) “It is important that your friends share your religious beliefs”. Answers were _Strongly Disagree_, _Disagree_, _Agree_, or _Strongly Agree_. I also adjusted for covariates of age category, sex, race, marital status, education level, whether someone had ever smoked, drank, or used marijuana, and year of interview. 

First, I ran three weighted adjusted multivariable logistic regressions, all of which showed significant association between MML and those who answered _Strongly Disagree_ or _Disagree_ to the three main predictors (p-values < 0.001). Then, I looked for any interaction between the three predictor variables and covariates. Only interaction with race warranted stratification. You can find my full report of the results [here](https://github.com/irenekpark49/NSDUH-Analysis-MML-Laws/blob/main/Irene's%20Research%20Rotation%20Manuscript.pdf).

I'd like to thank my advisors, Professor Ching-Ti Liu and Professor Yen-Han Lee. 
Feel free to contact me at irenekpark49@gmail.com with any questions about my code!
