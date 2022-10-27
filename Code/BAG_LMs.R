# BAG and bio-psycho-social factors
# BAG was calculated from diffusion data using six diffusion approaches: BRIA, DKI, DTI, SMT, mcSMT, WMTI
# code by Max Korbmacher, 14.10.2022

### PREP ####
# packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ModelMetrics")
library(ModelMetrics)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
library(viridis)
#install.packages("hrbrthemes")
#library(hrbrthemes)
#install.packages("dplyr")
library(dplyr)
#install.packages("lme4")
library(lme4)
library(lmerTest)
install.packages("MuMIn")
library(MuMIn)
library(rstatix)
install.packages("sjPlot")
library(sjPlot)

# get data
demo = read.csv("/cluster/projects/p33/users/maxk/UKB/BP_BAG/data/demo_clean.csv")
demo2 = read.csv("/cluster/projects/p33/users/maxk/UKB/clean_data/clean_phenotypes.csv")
BAG = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_dwMRI_test90_no_SD.csv")
BAG_BRIA = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_Bayes_no_SD_test.csv")
BAG_DKI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_DKI_no_SD_test.csv")
BAG_MEAN = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_MEAN_no_SD_test.csv")
BAG_DTI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_DTI_no_SD_test.csv")
BAG_SMT = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_SMT_no_SD_test.csv")
BAG_SMT_mc = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_SMT_mc_no_SD_test.csv")
BAG_WMTI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_WMTI_no_SD_test.csv")

#process data
demo21 = demo2 %>% subset(select = c(-X, -sex, -age, -birth_weight_t2, -birth_weight_t3)) %>% select(!contains("t4"))
demo21$mean_inc_pair_matches = rowSums(demo21[ ,c("inc_pair_matches_t3r1","inc_pair_matches_t3r2","excl_sample.inc_pair_matches_t3r3")],na.rm = FALSE)/3
demo21 = demo21 %>% subset(select = c(-inc_pair_matches_t3r1, -inc_pair_matches_t3r2, -excl_sample.inc_pair_matches_t3r3))
df = merge(demo, BAG, by="eid")
df = df %>% subset(select=-c(X, age.2.0))
df = df %>% rename(brainage_gap = brainage_gap_dwMRI_test90_no_SD,
                   BAG_residual = BAG_residual_dwMRI_test90_no_SD,
                   BAG_corrected_age = BAG_corrected_dwMRI_test90_no_SD)
df = merge(df, demo21, by = "eid")
df = df %>% subset(select = c(-waist_circumference.2.0, -hip_circumference.2.0, -height.2.0, -Height_m.2.0, -weight.2.0, -Assessment_centre.2.0, matrix_puzzles_viewed_t3))
levels(df$sex) = c("Female","Male")

# Change ordinal variables into numeric / name levels correctly

df$job_satisfaction_t3 = (factor(df$job_satisfaction_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$job_satisfaction_t3 = unclass(df$job_satisfaction_t3)
df$excl_sample.finance_satisfaction_t3 = (factor(df$excl_sample.finance_satisfaction_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$excl_sample.finance_satisfaction_t3 =unclass(df$excl_sample.finance_satisfaction_t3)
df$excl_sample.health_satisfaction_t3 = (factor(df$excl_sample.health_satisfaction_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$excl_sample.health_satisfaction_t3 =unclass(df$excl_sample.health_satisfaction_t3)
df$excl_sample.family_rel_satisfaction_t3 = (factor(df$excl_sample.family_rel_satisfaction_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$excl_sample.family_rel_satisfaction_t3 =unclass(df$excl_sample.family_rel_satisfaction_t3)
df$excl_sample.friend_rel_satisfaction_t3 = (factor(df$excl_sample.friend_rel_satisfaction_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$excl_sample.friend_rel_satisfaction_t3 = unclass(df$excl_sample.friend_rel_satisfaction_t3)
df$happiness_t3 = (factor(df$happiness_t3, levels = c("Extremely unhappy","Very unhappy","Moderately unhappy","Moderately happy","Very happy","Extremely happy")))
df$happiness_t3 = unclass(df$happiness_t3)
df$overall_health_t3 = (factor(df$overall_health_t3, levels = c("Poor","Fair","Good","Excellent")))
df$overall_health_t3 = unclass(df$overall_health_t3)
df$t3_income = as.factor(df$t3_income)
levels(df$t3_income) = c("£18-30k","£30-52k","£52-100k","£ Don't know",">£100k","<£18k","£ no answer")
df$t3_income = factor(df$t3_income, levels = c("<£18k","£18-30k","£30-52k","£52-100k",">£100k","£ no answer","£ Don't know"))
df$t3_income = droplevels(df$t3_income, exclude = c("£ no answer", "£ Don't know"))
df$social_visits_t3 = factor(df$social_visits_t3, levels = c("don't know", "no friends/family outside household","Never or almost never", "Once every few months", "About once a month","About once a week", "2-4 times a week", "Almost daily"))
df$social_visits_t3 = droplevels(df$social_visits_t3, exclude = c("don't know", "no friends/family outside household"))
 
# rename wrongly labelled tower arranging var
df = df %>% rename("tower_arranging" = "matrix_puzzles_correct_t3")

# DESCRIPTIVES & BIVARIATE RELATIONSHIPS ####

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## 1) get mean and sd in a table for all independent variables ########
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# print column names for the following output
a = colnames(df)
# print mean values
b = c()
for (i in 1:ncol(df)){b[i] = (mean(df[,i], na.rm = TRUE))}
# print sd
c = c()
for (i in 1:ncol(df)){c[i] = print(sd(df[,i], na.rm = TRUE))}
# print table
print(data.frame(a,b,c))

## get percentages for the rest of the nominal independent variables
prop.table(table(df$site_t3))
prop.table(table(df$t3_income))
prop.table(table(df$smoking.2.0))

## participant numbers
nrow(df) # total
table(df$site_t3) # by site
table(df$sex) # sex
nrow(df) - sum(is.na(df$age)) # age
table(df$t3_income) # income
table(df$higher_education_t3) # edu

nrow(df) - sum(is.na(df$matrix_puzzles_solved_t3)) ##### cog scores
nrow(df) - sum(is.na(df$tower_arranging))
nrow(df) - sum(is.na(df$prospective_memory_t3))
nrow(df) - sum(is.na(df$fluid_intelligence_t3))
nrow(df) - sum(is.na(df$digits_remembered_t3))
nrow(df) - sum(is.na(df$mean_inc_pair_matches))

nrow(df) - sum(is.na(df$job_satisfaction_t3)) ###### satisfaction
nrow(df) - sum(is.na(df$excl_sample.finance_satisfaction_t3))
nrow(df) - sum(is.na(df$excl_sample.health_satisfaction_t3))
nrow(df) - sum(is.na(df$overall_health_t3))
nrow(df) - sum(is.na(df$excl_sample.family_rel_satisfaction_t3))
nrow(df) - sum(is.na(df$excl_sample.friend_rel_satisfaction_t3))
nrow(df) - sum(is.na(df$happiness_t3))

nrow(df) - sum(is.na(df$BMI.2.0)) ### health risk factors
nrow(df) - sum(is.na(df$pulse_pressure.2.0))
nrow(df) - sum(is.na(df$WHR.2.0))
table(df$smoking.2.0)
table(df$diabetic.2.0)
table(df$hypertension.2.0)
table(df$high_cholesterol.2.0)
table(df$diagnosed_vascular_problem)
nrow(df) - sum(is.na(df$birth_weight_t1))
nrow(df) - sum(is.na(df$daily_coffee_t3))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## 2) get BAG values for each group and relate them with bio-psycho-socialfactors ###### 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

#### starting with demographics

# scanner site
df %>% group_by(site_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% kruskal_test(brainage_gap ~ site_t3)
df %>% kruskal_effsize(brainage_gap ~ site_t3)
# sex
df %>% group_by(sex) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ sex)
df %>% wilcox_effsize(brainage_gap ~ sex)

# age
df %>% drop_na(age) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(age, brainage_gap)

#### socio-demographics
# ethnicity
df %>% group_by(Ethnicity)%>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% group_by(Ethnicity)%>% dplyr::summarize(n = n())%>%mutate(freq = n/ sum(n))
df %>% wilcox_test(brainage_gap ~ Ethnicity)
df %>% wilcox_effsize(brainage_gap ~ Ethnicity)

# income (without those who did not want to tell or didn't know)
df %>% group_by(t3_income) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% 
  kruskal_test(brainage_gap ~ t3_income)
df %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% 
  kruskal_effsize(brainage_gap ~ t3_income)
# experimentally including all income groups, also those who did not want to tell or didn't know
df %>% kruskal_test(brainage_gap ~ t3_income)
df %>% kruskal_effsize(brainage_gap ~ t3_income)

# higher ed
df %>% group_by(higher_education_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ higher_education_t3)
df %>% wilcox_effsize(brainage_gap ~ higher_education_t3)

#### cognitive scores

# matrix puzzles solved
df %>% drop_na(matrix_puzzles_solved_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, matrix_puzzles_solved_t3)

# Tower rearranging correct
df %>% drop_na(tower_arranging) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, tower_arranging)

# prospective memory
df %>% drop_na(prospective_memory_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, prospective_memory_t3)

# fluid intel
df %>% drop_na(fluid_intelligence_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, fluid_intelligence_t3)

# digits remembered
df %>% drop_na(digits_remembered_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, digits_remembered_t3)

# mean number of correct pair matches across trials
df %>% drop_na(mean_inc_pair_matches) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, mean_inc_pair_matches)


###### life satisfaction
# job sat
df %>% drop_na(job_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, job_satisfaction_t3)
#finance sat
df %>% drop_na(excl_sample.finance_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, excl_sample.finance_satisfaction_t3)
# health sat
df %>% drop_na(excl_sample.health_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, excl_sample.health_satisfaction_t3)
# overall health rating
df %>% drop_na(overall_health_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, overall_health_t3)
# family rel sat
df %>% drop_na(excl_sample.family_rel_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, excl_sample.family_rel_satisfaction_t3)
# friend rel sat
df %>% drop_na(excl_sample.friend_rel_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, excl_sample.friend_rel_satisfaction_t3)
# happiness
df %>% drop_na(happiness_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, happiness_t3)


#### health risk factors
# BMI
df %>% drop_na(BMI.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, BMI.2.0)

# pulse pressure
df %>% drop_na(pulse_pressure.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, pulse_pressure.2.0)

# waist to hip ratio
df %>% drop_na(WHR.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, WHR.2.0)

# smoking
df %>% group_by(smoking.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ smoking.2.0)
df %>% wilcox_effsize(brainage_gap ~ smoking.2.0)

# diabetes
df %>% group_by(diabetic.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ diabetic.2.0)
df %>% wilcox_effsize(brainage_gap ~ diabetic.2.0)

# hypertens
df %>% group_by(hypertension.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ hypertension.2.0)
df %>% wilcox_effsize(brainage_gap ~ hypertension.2.0)

# high chol
df %>% group_by(high_cholesterol.2.0) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ high_cholesterol.2.0)
df %>% wilcox_effsize(brainage_gap ~ high_cholesterol.2.0)

# diagnosed vasc problems
df %>% group_by(diagnosed_vascular_problem) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% wilcox_test(brainage_gap ~ diagnosed_vascular_problem)
df %>% wilcox_effsize(brainage_gap ~ diagnosed_vascular_problem)


# birth weight
df %>% drop_na(birth_weight_t1) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, birth_weight_t1)

# daily coffe intake
df %>% drop_na(daily_coffee_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df %>% cor_test(brainage_gap, daily_coffee_t3)



# MIXED MODELS ####

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## first, check how models work on multimodel predictions (full model BAG) #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# 0) Null models
MLM0_1 = lmer(brainage_gap ~ 1 + (1|site_t3), df) # random only
isSingular(MLM0_1, tol = 1e-4)
summary(MLM0_1)
MLM0_2 = lmer(brainage_gap ~ 1 + age + (1|site_t3), df) # add age
isSingular(MLM0_2, tol = 1e-4)

# 1) baseline model
MLM_baseline = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3), df)
isSingular(MLM_baseline, tol = 1e-4)
summary(MLM_baseline)
data.frame(r.squaredGLMM(MLM_baseline),AIC(MLM_baseline),(sigma(MLM_baseline))^2,summary(MLM_baseline)$logLik[1])

# create table comparing null models and baseline model
tab_model(MLM0_2, MLM_baseline, show.se = T, show.df = F, show.aic = T, show.loglik = T)

# 2) add sociodemographics to baseline model
MLM_sociodemo = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ higher_education_t3 + as.numeric(t3_income), df)
isSingular(MLM_sociodemo, tol = 1e-4)
summary(MLM_sociodemo)
tab_model(MLM_baseline, MLM_sociodemo, show.se = T, show.df = F, show.aic = T, show.loglik = T)


# 3) add health and lifestyle factors to baseline model
MLM_health = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI.2.0 + WHR.2.0 + pulse_pressure.2.0 + alcohol_drinker.2.0 + diabetic.2.0 +
                    high_cholesterol.2.0 + hypertension.2.0 + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3, df)
isSingular(MLM_health, tol = 1e-4)
summary(MLM_health)
r.squaredGLMM(MLM_health)
tab_model(MLM_baseline, MLM_health, show.se = T, show.df = F, show.aic = T, show.loglik = T)

# 4) add well-being to baseline model
MLM_wellbeing = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                       excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 + as.numeric(social_visits_t3), df)
summary(MLM_wellbeing)
r.squaredGLMM(MLM_wellbeing)
tab_model(MLM_health, show.se = T, show.df = F, show.aic = T, show.loglik = T)

# 5) add cognitive scores to baseline model
MLM_cog = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + matrix_puzzles_viewed_t3 + fluid_intelligence_t3 + 
                 excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, df)
isSingular(MLM_cog, tol = 1e-4)
summary(MLM_cog)
r.squaredGLMM(MLM_cog)
tab_model(MLM_cog, show.se = T, show.df = F, show.aic = T, show.loglik = T)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## second, loop over all BAGs and create a model performance metric table  #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# match BAGs with full data frame (to later make a list of them)
df_new = df %>% select(-brainage_gap, -age)
MEAN = merge(df_new, BAG_MEAN, by = "eid")
BRIA = merge(df_new, BAG_BRIA, by = "eid")
DKI = merge(df_new, BAG_DKI, by = "eid")
DTI = merge(df_new, BAG_DTI, by = "eid")
SMT = merge(df_new, BAG_SMT, by = "eid")
SMT_mc = merge(df_new, BAG_SMT_mc, by = "eid")
WMTI = merge(df_new, BAG_WMTI, by = "eid")

# name all brainage_gaps correctly (brainage_gap)
FULL = df
MEAN = MEAN %>% rename("brainage_gap" = "brainage_gap_MEAN_no_SD_test")
BRIA= BRIA %>% rename("brainage_gap" = "brainage_gap_Bayes_no_SD_test")
DKI= DKI %>% rename("brainage_gap" = "brainage_gap_DKI_no_SD_test")
DTI= DTI %>% rename("brainage_gap" = "brainage_gap_DTI_no_SD_test")
SMT= SMT %>% rename("brainage_gap" = "brainage_gap_SMT_no_SD_test")
SMT_mc= SMT_mc %>% rename("brainage_gap" = "brainage_gap_SMT_mc_no_SD_test")
WMTI= WMTI %>% rename("brainage_gap" = "brainage_gap_WMTI_no_SD_test")

# create a list with all the data frames
brainage_list = list(FULL, MEAN, BRIA, DKI, DTI, SMT, SMT_mc, WMTI)

# loop over list for predictions

## ## ## NULL MODEL 0.1 ## ## ## 
model0.1 <- list()
for (i in 1:8) {model0.1[[i]] = lmer(brainage_gap~ 1 + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.1_metrics[i,] = c(r.squaredGLMM(model0.1[[i]]),AIC(model0.1[[i]]),(sigma(model0.1[[i]]))^2,summary(model0.1[[i]])$logLik[1])} 

## ## ## NULL MODEL 0.2 ## ## ## 
model0.2 <- list()
for (i in 1:8) {model0.2[[i]] = lmer(brainage_gap~ 1 + age + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.2_metrics[i,] = c(r.squaredGLMM(model0.2[[i]]),AIC(model0.2[[i]]),(sigma(model0.2[[i]]))^2,summary(model0.2[[i]])$logLik[1])} 

## ## ## BASELINE MODEL 1 ## ## ## 
model1 <- list()
for (i in 1:8) {model1[[i]] = lmer(brainage_gap~ 1 + age + sex + age:sex + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model1_metrics[i,] = c(r.squaredGLMM(model1[[i]]),AIC(model1[[i]]),(sigma(model1[[i]]))^2,summary(model1[[i]])$logLik[1])} 

## ## ## SOCIODEMO MODEL 2 ## ## ## 
model2 <- list()
for (i in 1:8) {model2[[i]] = lmer(brainage_gap~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model2_metrics[i,] = c(r.squaredGLMM(model2[[i]]),AIC(model2[[i]]),(sigma(model2[[i]]))^2,summary(model2[[i]])$logLik[1])} 

## ## ## HEALTH MODEL 3 ## ## ## 
model3 <- list()
for (i in 1:8) {model3[[i]] = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI.2.0 + WHR.2.0 + pulse_pressure.2.0 + alcohol_drinker.2.0 + diabetic.2.0 +
                    high_cholesterol.2.0 + hypertension.2.0 + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3_metrics[i,] = c(r.squaredGLMM(model3[[i]]),AIC(model3[[i]]),(sigma(model3[[i]]))^2,summary(model3[[i]])$logLik[1])} 

## ## ## WELLBEING MODEL 4 ## ## ## 
model4 <- list()
for (i in 1:8) {model4[[i]] = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                       excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4_metrics[i,] = c(r.squaredGLMM(model4[[i]]),AIC(model4[[i]]),(sigma(model4[[i]]))^2,summary(model4[[i]])$logLik[1])} 


## ## ## COGNITION MODEL 5 ## ## ## 
model5 <- list()
for (i in 1:8) {model5[[i]] = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                 excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5_metrics[i,] = c(r.squaredGLMM(model5[[i]]),AIC(model5[[i]]),(sigma(model5[[i]]))^2,summary(model5[[i]])$logLik[1])} 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
## ## CREATE ONE SINGLE DF FROM ALL THE OUTPUT ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
output = rbind(model0.1_metrics,model0.2_metrics, model1_metrics, model2_metrics, model3_metrics, model4_metrics, model5_metrics)
output$Model = c(replicate(8,"random only"), replicate(8,"age"), replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
output$BAG = c(replicate(7, dMRI_model))
output = output %>% rename("R2M"="X1",
                  "R2C" = "X2",
                  "AIC" = "X3",
                  "sig2" = "X4",
                  "logLik" = "X5")
write.csv(output, "/cluster/projects/p33/users/maxk/UKB/export/FrontiersTable3.csv")

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## third, loop over all models and create beta tables and figures  #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

############## BASELINE MODEL
baseline = list()
for (i in 1:8){
  baseline[[i]] = cbind(summary(model1[[i]])$coefficients[2:4,1], confint(model1[[i]])[4:6,], c("Age","Sex","Age*Sex"), c(replicate(3, "Baseline")))
} # to get the intercept as well: # baseline[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "Baseline")))

baseline_tab = data.frame(do.call(rbind, baseline))
dMRI_model_list = c(replicate(3,"FULL"), replicate(3,"MEAN"), replicate(3,"BRIA"), replicate(3,"DKI"),
                    replicate(3,"DTI"), replicate(3,"SMT"), replicate(3,"SMT_mc"), replicate(3,"WMTI"))
baseline_tab$Model = dMRI_model_list
baseline_tab = baseline_tab %>% rename("Beta" = "V1",
                        "CI2.5" = "X2.5..",
                        "CI97.5" = "X97.5..",
                        "Variable" = "V4")
levels(baseline_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
baseline_tab$Beta = as.numeric(as.character(baseline_tab$Beta))
baseline_tab$CI2.5 = as.numeric(as.character(baseline_tab$CI2.5))
baseline_tab$CI97.5 = as.numeric(as.character(baseline_tab$CI97.5))
baseline_tab$Model = (as.factor(baseline_tab$Model))
baseline_tab$Variable = (as.factor(baseline_tab$Variable))

# Basic bar plot
baseline_plot = ggplot(baseline_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(summary(model2[[i]])$coefficients[2:8,1], confint(model2[[i]])[4:10,], c("Age","Sex","Ethnicity","Higher Education","Income","Social Visits","Age*Sex"), c(replicate(5, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "sociodemo")))

sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(7,"FULL"), replicate(7,"MEAN"), replicate(7,"BRIA"), replicate(7,"DKI"),
                    replicate(7,"DTI"), replicate(7,"SMT"), replicate(7,"SMT_mc"), replicate(7,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% rename("Beta" = "V1",
                                       "CI2.5" = "X2.5..",
                                       "CI97.5" = "X97.5..",
                                       "Variable" = "V4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$CI2.5 = as.numeric(as.character(sociodemo_tab$CI2.5))
sociodemo_tab$CI97.5 = as.numeric(as.character(sociodemo_tab$CI97.5))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Age","Sex","Age*Sex","Ethnicity","Income","Higher Education", "Social Visits"))

# Basic plot
sociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSociodemoPlot.pdf",sociodemo_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSociodemoPlot.pdf",sociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
health = list()
for (i in 1:8){
  health[[i]] = cbind(summary(model3[[i]])$coefficients[2:15,1], confint(model3[[i]])[4:17,], c("Age","Sex","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee","Age*Sex"), c(replicate(18, "health")))
} # to get the intercept as well: # health[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(14,"FULL"), replicate(14,"MEAN"), replicate(14,"BRIA"), replicate(14,"DKI"),
                    replicate(14,"DTI"), replicate(14,"SMT"), replicate(14,"SMT_mc"), replicate(14,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$CI2.5 = as.numeric(as.character(health_tab$CI2.5))
health_tab$CI97.5 = as.numeric(as.character(health_tab$CI97.5))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","Sex","Age*Sex","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol")))

# Basic bar plot
health_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersHealthPlot.pdf",health_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersHealthPlot.pdf",health_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(summary(model4[[i]])$coefficients[2:11,1], confint(model4[[i]])[4:13,], c("Age","Sex","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction","Age*Sex"), c(replicate(11, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(10,"FULL"), replicate(10,"MEAN"), replicate(10,"BRIA"), replicate(10,"DKI"),
                    replicate(10,"DTI"), replicate(10,"SMT"), replicate(10,"SMT_mc"), replicate(10,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% rename("Beta" = "V1",
                                   "CI2.5" = "X2.5..",
                                   "CI97.5" = "X97.5..",
                                   "Variable" = "V4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$CI2.5 = as.numeric(as.character(wellbeing_tab$CI2.5))
wellbeing_tab$CI97.5 = as.numeric(as.character(wellbeing_tab$CI97.5))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Sex","Age*Sex","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
wellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersWellbeingPlot.pdf",wellbeing_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersWellbeingPlot.pdf",wellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(summary(model5[[i]])$coefficients[2:11,1], confint(model5[[i]])[4:13,], c("Age","Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered","Age*Sex"), c(replicate(10, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(10,"FULL"), replicate(10,"MEAN"), replicate(10,"BRIA"), replicate(10,"DKI"),
                    replicate(10,"DTI"), replicate(10,"SMT"), replicate(10,"SMT_mc"), replicate(10,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$CI2.5 = as.numeric(as.character(cognition_tab$CI2.5))
cognition_tab$CI97.5 = as.numeric(as.character(cognition_tab$CI97.5))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Sex","Age*Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
cognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersCognitionPlot.pdf",cognition_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersCognitionPlot.pdf",cognition_plot, height = 6, width = 9)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
## fourth, compute P-vals for predictors  #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #

baseline_p = list()
for (i in 1:8){baseline_p[[i]] = cbind(summary(model1[[i]])$coefficients)}
names = c("Intercept", "age","sex","sex*age")
names1 = c(rep(names, 8))
baseline_p = data.frame(do.call(cbind, baseline_p))
baseline_p$names = names1
ggplot(baseline_p, aes(x = names, y = Pr...t..)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= names), size = 0.2)

social_p = list()
for (i in 1:8){social_p[[i]] = cbind(summary(model2[[i]])$coefficients)} 
social_p = data.frame(do.call(rbind, social_p))
names = rownames(summary(model2[[1]])$coefficients)
names1 = c(rep(names, 8))
social_p = data.frame(do.call(cbind, social_p))
social_p$names = names1
ggplot(social_p, aes(x = names, y = Pr...t..)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= names), size = 0.2)
social_p %>% filter(names == "EthnicitynonEuropean")
social_p$names

health_p = list()
for (i in 1:8){health_p[[i]] = cbind(summary(model3[[i]])$coefficients)} 
health_p = data.frame(do.call(rbind, health_p))
names = rownames(summary(model3[[1]])$coefficients)
names1 = c(rep(names, 8))
health_p = data.frame(do.call(cbind, health_p))
health_p$names = names1
ggplot(health_p, aes(x = names, y = Pr...t..)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= names), size = 0.2)
health_p %>% filter(names == "diagnosed_vascular_problemTRUE")
health_p$names

wellbeing_p = list()
for (i in 1:8){wellbeing_p[[i]] = cbind(summary(model4[[i]])$coefficients)} 
wellbeing_p = data.frame(do.call(rbind, wellbeing_p))
names = rownames(summary(model4[[1]])$coefficients)
names1 = c(rep(names, 8))
wellbeing_p = data.frame(do.call(cbind, wellbeing_p))
wellbeing_p$names = names1

ggplot(wellbeing_p, aes(x = names, y = Pr...t..)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= names), size = 0.2)
wellbeing_p %>% filter(names == "overall_health_t3")
wellbeing_p$names

cognitive_p = list()
for (i in 1:8){cognitive_p[[i]] = cbind(summary(model5[[i]])$coefficients)} 
cognitive_p = data.frame(do.call(rbind, cognitive_p))
names = rownames(summary(model5[[1]])$coefficients)
names1 = c(rep(names, 8))
cognitive_p = data.frame(do.call(cbind, cognitive_p))
cognitive_p$names = names1
dp = plot(density(df$brainage_gap))
ggsave("/tsd/p33/home/p33-maxk/export/BAG_density.pdf",dp, height = 6, width = 9)

ggplot(cognitive_p, aes(x = names, y = Pr...t..)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= names), size = 0.2)

cognitive_p%>% filter(names == "digits_remembered_t3")
cognitive_p$names

## ## ## ## ## ## ## ## ## ## #
# MIXED MODELS WITHOUT AGE ####
## ## ## ## ## ## ## ## ## ## #

## ## ## BASELINE MODEL 1 ## ## ## 
model1.2 <- list()
for (i in 1:8) {model1.2[[i]] = lmer(brainage_gap~ 1  + sex  + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model1.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model1.2_metrics[i,] = c(r.squaredGLMM(model1.2[[i]]),AIC(model1.2[[i]]),(sigma(model1.2[[i]]))^2,summary(model1.2[[i]])$logLik[1])} 

## ## ## SOCIODEMO MODEL 2 ## ## ## 
model2.2 <- list()
for (i in 1:8) {model2.2[[i]] = lmer(brainage_gap~ 1  + sex  + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income) + social_visits_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model2.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model2.2_metrics[i,] = c(r.squaredGLMM(model2.2[[i]]),AIC(model2.2[[i]]),(sigma(model2.2[[i]]))^2,summary(model2.2[[i]])$logLik[1])} 

## ## ## HEALTH MODEL 3 ## ## ## 
model3.2 <- list()
for (i in 1:8) {model3.2[[i]] = lmer(brainage_gap ~ 1  + sex  + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                     high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model3.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3.2_metrics[i,] = c(r.squaredGLMM(model3.2[[i]]),AIC(model3.2[[i]]),(sigma(model3.2[[i]]))^2,summary(model3.2[[i]])$logLik[1])} 

## ## ## WELLBEING MODEL 4 ## ## ## 
model4.2 <- list()
for (i in 1:8) {model4.2[[i]] = lmer(brainage_gap ~ 1  + sex  + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 + as.numeric(social_visits_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4.2_metrics[i,] = c(r.squaredGLMM(model4.2[[i]]),AIC(model4.2[[i]]),(sigma(model4.2[[i]]))^2,summary(model4.2[[i]])$logLik[1])} 


## ## ## COGNITION MODEL 5 ## ## ## 
model5.2 <- list()
for (i in 1:8) {model5.2[[i]] = lmer(brainage_gap ~ 1  + sex  + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5.2_metrics[i,] = c(r.squaredGLMM(model5.2[[i]]),AIC(model5.2[[i]]),(sigma(model5.2[[i]]))^2,summary(model5.2[[i]])$logLik[1])} 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
## ## CREATE ONE SINGLE DF FROM ALL THE OUTPUT ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
output = rbind(model1.2_metrics, model2.2_metrics, model3.2_metrics, model4.2_metrics, model5.2_metrics)
output$Model = c(replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
output$BAG = c(replicate(5, dMRI_model))
output = output %>% rename("R2M"="X1",
                           "R2C" = "X2",
                           "AIC" = "X3",
                           "sig2" = "X4",
                           "logLik" = "X5")
write.csv(output, "/cluster/projects/p33/users/maxk/UKB/export/FrontiersST2.csv")

## ## ## ## ## ## ## ## ##

## NO AGE IN MODELS: beta tables and figures  #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

############## BASELINE MODEL
baseline = list()
for (i in 1:8){
  baseline[[i]] = cbind(summary(model1.2[[i]])$coefficients[2,1], confint(model1.2[[i]])[4,], c("Sex"), c(replicate(3, "Baseline")))
} # to get the intercept as well: # baseline[[i]] = cbind(summary(model1.2[[i]])$coefficients[,1], confint(model1.2[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "Baseline")))

baseline_tab = data.frame(do.call(rbind, baseline))
dMRI_model_list = c(replicate(1,"FULL"), replicate(1,"MEAN"), replicate(1,"BRIA"), replicate(1,"DKI"),
                    replicate(1,"DTI"), replicate(1,"SMT"), replicate(1,"SMT_mc"), replicate(1,"WMTI"))
baseline_tab$Model = dMRI_model_list
baseline_tab = baseline_tab %>% rename("Beta" = "V1",
                                       "CI2.5" = "X2.5..",
                                       "CI97.5" = "X97.5..",
                                       "Variable" = "V4")
levels(baseline_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
baseline_tab$Beta = as.numeric(as.character(baseline_tab$Beta))
baseline_tab$CI2.5 = as.numeric(as.character(baseline_tab$CI2.5))
baseline_tab$CI97.5 = as.numeric(as.character(baseline_tab$CI97.5))
baseline_tab$Model = (as.factor(baseline_tab$Model))
baseline_tab$Variable = (as.factor(baseline_tab$Variable))

# Basic bar plot
baseline_plot = ggplot(baseline_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(summary(model2.2[[i]])$coefficients[2:6,1], confint(model2.2[[i]])[4:8,], c("Sex","Ethnicity","Higher Education","Income","Social Visits"))
} # to get the intercept as well: # sociodemo[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "sociodemo")))
summary(model2.2[[1]])$coefficients[2:6,1]
sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(5,"FULL"), replicate(5,"MEAN"), replicate(5,"BRIA"), replicate(5,"DKI"),
                    replicate(5,"DTI"), replicate(5,"SMT"), replicate(5,"SMT_mc"), replicate(5,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$CI2.5 = as.numeric(as.character(sociodemo_tab$CI2.5))
sociodemo_tab$CI97.5 = as.numeric(as.character(sociodemo_tab$CI97.5))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Sex","Ethnicity","Income","Higher Education", "Social Visits"))

# Basic plot
sociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSF6.pdf",sociodemo_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSF6.pdf",sociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
health = list()
for (i in 1:8){
  health[[i]] = cbind(summary(model3.2[[i]])$coefficients[2:13,1], confint(model3.2[[i]])[4:15,], c("Sex","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee"), c(replicate(18, "health")))
} # to get the intercept as well: # health[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(12,"FULL"), replicate(12,"MEAN"), replicate(12,"BRIA"), replicate(12,"DKI"),
                    replicate(12,"DTI"), replicate(12,"SMT"), replicate(12,"SMT_mc"), replicate(12,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% rename("Beta" = "V1",
                                   "CI2.5" = "X2.5..",
                                   "CI97.5" = "X97.5..",
                                   "Variable" = "V4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$CI2.5 = as.numeric(as.character(health_tab$CI2.5))
health_tab$CI97.5 = as.numeric(as.character(health_tab$CI97.5))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Sex","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol")))

# Basic bar plot
health_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSF7.pdf",health_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSF7.pdf",health_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(summary(model4.2[[i]])$coefficients[2:9,1], confint(model4.2[[i]])[4:11,], c("Sex","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction"))
} # to get the intercept as well: # wellbeing[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$CI2.5 = as.numeric(as.character(wellbeing_tab$CI2.5))
wellbeing_tab$CI97.5 = as.numeric(as.character(wellbeing_tab$CI97.5))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Sex","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
wellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSF8.pdf",wellbeing_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSF8.pdf",wellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(summary(model5.2[[i]])$coefficients[2:9,1], confint(model5.2[[i]])[4:11,], c("Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered"))
} # to get the intercept as well: # cognition[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$CI2.5 = as.numeric(as.character(cognition_tab$CI2.5))
cognition_tab$CI97.5 = as.numeric(as.character(cognition_tab$CI97.5))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
cognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSF9.pdf",cognition_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSF9.pdf",cognition_plot, height = 6, width = 9)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #

# MIXED MODELS BY SEX ####
## ## ## ## ## ## ## ## ## 
brainage_list = list(FULL, MEAN, BRIA, DKI, DTI, SMT, SMT_mc, WMTI) # just as a refresher

# split each data frame in list by sex
brainage_females = list()
brainage_males = list()
for (i in 1:8){
  brainage_females[[i]] = brainage_list[[i]]%>% filter(sex == 0)
  brainage_males[[i]] = brainage_list[[i]]%>% filter(sex == 1)
}

### run models ####

## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## RUN MIXED MODELS FOR FEMALES ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## BASELINE MODEL 1 ## ## ## 
Fmodel1 <- list()
for (i in 1:8) {Fmodel1[[i]] = lmer(brainage_gap~ 1 + age  + (1|site_t3), data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel1_metrics[i,] = c(r.squaredGLMM(Fmodel1[[i]]),AIC(Fmodel1[[i]]),(sigma(Fmodel1[[i]]))^2,summary(Fmodel1[[i]])$logLik[1])} 

## ## ## SOCIODEMO Fmodel 2 ## ## ## 
Fmodel2 <- list()
for (i in 1:8) {Fmodel2[[i]] = lmer(brainage_gap~ 1 + age  + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel2_metrics[i,] = c(r.squaredGLMM(Fmodel2[[i]]),AIC(Fmodel2[[i]]),(sigma(Fmodel2[[i]]))^2,summary(Fmodel2[[i]])$logLik[1])} 

## ## ## HEALTH Fmodel 3 ## ## ## 
Fmodel3 <- list()
for (i in 1:8) {Fmodel3[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3)+ BMI.2.0 + WHR.2.0 + pulse_pressure.2.0 + alcohol_drinker.2.0 + diabetic.2.0 +
                                     high_cholesterol.2.0 + hypertension.2.0 + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3, data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel3_metrics[i,] = c(r.squaredGLMM(Fmodel3[[i]]),AIC(Fmodel3[[i]]),(sigma(Fmodel3[[i]]))^2,summary(Fmodel3[[i]])$logLik[1])} 

## ## ## WELLBEING Fmodel 4 ## ## ## 
Fmodel4 <- list()
for (i in 1:8) {Fmodel4[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel4_metrics[i,] = c(r.squaredGLMM(Fmodel4[[i]]),AIC(Fmodel4[[i]]),(sigma(Fmodel4[[i]]))^2,summary(Fmodel4[[i]])$logLik[1])} 


## ## ## COGNITION Fmodel 5 ## ## ## 
Fmodel5 <- list()
for (i in 1:8) {Fmodel5[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel5_metrics[i,] = c(r.squaredGLMM(Fmodel5[[i]]),AIC(Fmodel5[[i]]),(sigma(Fmodel5[[i]]))^2,summary(Fmodel5[[i]])$logLik[1])} 





## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## RUN MIXED MODELS FOR MALES ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## BASELINE MODEL 1 ## ## ## 
Mmodel1 <- list()
for (i in 1:8) {Mmodel1[[i]] = lmer(brainage_gap~ 1 + age  + (1|site_t3), data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel1_metrics[i,] = c(r.squaredGLMM(Mmodel1[[i]]),AIC(Mmodel1[[i]]),(sigma(Mmodel1[[i]]))^2,summary(Mmodel1[[i]])$logLik[1])} 

## ## ## SOCIODEMO Mmodel 2 ## ## ## 
Mmodel2 <- list()
for (i in 1:8) {Mmodel2[[i]] = lmer(brainage_gap~ 1 + age  + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel2_metrics[i,] = c(r.squaredGLMM(Mmodel2[[i]]),AIC(Mmodel2[[i]]),(sigma(Mmodel2[[i]]))^2,summary(Mmodel2[[i]])$logLik[1])} 

## ## ## HEALTH Mmodel 3 ## ## ## 
Mmodel3 <- list()
for (i in 1:8) {Mmodel3[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3)+ BMI.2.0 + WHR.2.0 + pulse_pressure.2.0 + alcohol_drinker.2.0 + diabetic.2.0 +
                                      high_cholesterol.2.0 + hypertension.2.0 + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3, data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel3_metrics[i,] = c(r.squaredGLMM(Mmodel3[[i]]),AIC(Mmodel3[[i]]),(sigma(Mmodel3[[i]]))^2,summary(Mmodel3[[i]])$logLik[1])} 

## ## ## WELLBEING Mmodel 4 ## ## ## 
Mmodel4 <- list()
for (i in 1:8) {Mmodel4[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                      excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel4_metrics[i,] = c(r.squaredGLMM(Mmodel4[[i]]),AIC(Mmodel4[[i]]),(sigma(Mmodel4[[i]]))^2,summary(Mmodel4[[i]])$logLik[1])} 


## ## ## COGNITION Mmodel 5 ## ## ## 
Mmodel5 <- list()
for (i in 1:8) {Mmodel5[[i]] = lmer(brainage_gap ~ 1 + age  + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                      excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel5_metrics[i,] = c(r.squaredGLMM(Mmodel5[[i]]),AIC(Mmodel5[[i]]),(sigma(Mmodel5[[i]]))^2,summary(Mmodel5[[i]])$logLik[1])} 







## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
## ## CREATE METRICS DF FOR EACH FEMALES & MALES ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
# females
Foutput = rbind(Fmodel1_metrics, Fmodel2_metrics, Fmodel3_metrics, Fmodel4_metrics, Fmodel5_metrics)
Foutput$Model = c(replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
Foutput$BAG = c(replicate(5, dMRI_model))
Foutput = Foutput %>% rename("R2M"="X1",
                           "R2C" = "X2",
                           "AIC" = "X3",
                           "sig2" = "X4",
                           "logLik" = "X5")
write.csv(Foutput, "/cluster/projects/p33/users/maxk/UKB/export/FrontiersST3_females.csv")
# males
Moutput = rbind(Mmodel1_metrics, Mmodel2_metrics, Mmodel3_metrics, Mmodel4_metrics, Mmodel5_metrics)
Moutput$Model = c(replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
Moutput$BAG = c(replicate(5, dMRI_model))
Moutput = Moutput %>% rename("R2M"="X1",
                             "R2C" = "X2",
                             "AIC" = "X3",
                             "sig2" = "X4",
                             "logLik" = "X5")
write.csv(Moutput, "/cluster/projects/p33/users/maxk/UKB/export/FrontiersST3_males.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## loop over all models and create beta tables and figures  ###########
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# FEMALES

####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(summary(Fmodel2[[i]])$coefficients[2:6,1], confint(Fmodel2[[i]])[4:8,], c("Age","Ethnicity","Higher Education","Income","Social Visits"), c(replicate(5, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(summary(Fmodel1[[i]])$coefficients[,1], confint(Fmodel1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "sociodemo")))
sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(5,"FULL"), replicate(5,"MEAN"), replicate(5,"BRIA"), replicate(5,"DKI"),
                    replicate(5,"DTI"), replicate(5,"SMT"), replicate(5,"SMT_mc"), replicate(5,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$CI2.5 = as.numeric(as.character(sociodemo_tab$CI2.5))
sociodemo_tab$CI97.5 = as.numeric(as.character(sociodemo_tab$CI97.5))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Age","Ethnicity","Income","Higher Education", "Social Visits"))

# Basic plot
Fsociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: BAG ~ Sociodemographic Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  ylim(-1,1) + theme_bw()
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFSociodemoPlot.pdf",Fsociodemo_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFSociodemoPlot.pdf",Fsociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
health = list()
for (i in 1:8){
  health[[i]] = cbind(summary(Fmodel3[[i]])$coefficients[2:13,1], confint(Fmodel3[[i]])[4:15,], c("Age","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee"), c(replicate(18, "health")))
} # to get the intercept as well: # health[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(12,"FULL"), replicate(12,"MEAN"), replicate(12,"BRIA"), replicate(12,"DKI"),
                    replicate(12,"DTI"), replicate(12,"SMT"), replicate(12,"SMT_mc"), replicate(12,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% rename("Beta" = "V1",
                                   "CI2.5" = "X2.5..",
                                   "CI97.5" = "X97.5..",
                                   "Variable" = "V4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$CI2.5 = as.numeric(as.character(health_tab$CI2.5))
health_tab$CI97.5 = as.numeric(as.character(health_tab$CI97.5))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol")))

# Basic bar plot
Fhealth_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: BAG ~ Health Risk Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))+ ylim(-1,8)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFHealthPlot.pdf",Fhealth_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFHealthPlot.pdf",Fhealth_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(summary(Fmodel4[[i]])$coefficients[2:9,1], confint(Fmodel4[[i]])[4:11,], c("Age","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction"), c(replicate(11, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$CI2.5 = as.numeric(as.character(wellbeing_tab$CI2.5))
wellbeing_tab$CI97.5 = as.numeric(as.character(wellbeing_tab$CI97.5))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
Fwellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: BAG ~ Life Satisfaction Factors", x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) + ylim(-0.75, 0.5)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFWellbeingPlot.pdf",Fwellbeing_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFWellbeingPlot.pdf",Fwellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(summary(Fmodel5[[i]])$coefficients[2:9,1], confint(Fmodel5[[i]])[4:11,], c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered"), c(replicate(10, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$CI2.5 = as.numeric(as.character(cognition_tab$CI2.5))
cognition_tab$CI97.5 = as.numeric(as.character(cognition_tab$CI97.5))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
Fcognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: BAG ~ Cognitive Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) + ylim(-0.75, 0.6)
##ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFCognitionPlot.pdf",Fcognition_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFCognitionPlot.pdf",Fcognition_plot, height = 6, width = 9)







# MALES

####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(summary(Mmodel2[[i]])$coefficients[2:6,1], confint(Mmodel2[[i]])[4:8,], c("Age","Ethniciy","Higher Education","Income","Social Visits"), c(replicate(5, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(summary(Mmodel1[[i]])$coefficients[,1], confint(Mmodel1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "sociodemo")))

sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(5,"FULL"), replicate(5,"MEAN"), replicate(5,"BRIA"), replicate(5,"DKI"),
                    replicate(5,"DTI"), replicate(5,"SMT"), replicate(5,"SMT_mc"), replicate(5,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$CI2.5 = as.numeric(as.character(sociodemo_tab$CI2.5))
sociodemo_tab$CI97.5 = as.numeric(as.character(sociodemo_tab$CI97.5))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Age","Ethniciy","Income","Higher Education", "Social Visits"))

# Basic plot
Msociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: BAG ~ Sociodemographic Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  ylim(-1,1) + theme_bw()
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMSociodemoPlot.pdf",Msociodemo_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMSociodemoPlot.pdf",Msociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
health = list()
for (i in 1:8){
  health[[i]] = cbind(summary(Mmodel3[[i]])$coefficients[2:13,1], confint(Mmodel3[[i]])[4:15,], c("Age","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee"), c(replicate(18, "health")))
} # to get the intercept as well: # health[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(12,"FULL"), replicate(12,"MEAN"), replicate(12,"BRIA"), replicate(12,"DKI"),
                    replicate(12,"DTI"), replicate(12,"SMT"), replicate(12,"SMT_mc"), replicate(12,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% rename("Beta" = "V1",
                                   "CI2.5" = "X2.5..",
                                   "CI97.5" = "X97.5..",
                                   "Variable" = "V4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$CI2.5 = as.numeric(as.character(health_tab$CI2.5))
health_tab$CI97.5 = as.numeric(as.character(health_tab$CI97.5))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol")))

# Basic bar plot
Mhealth_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: BAG ~ Health Risk Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45)) + ylim(-1,8)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMHealthPlot.pdf",Mhealth_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMHealthPlot.pdf",Mhealth_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(summary(Mmodel4[[i]])$coefficients[2:9,1], confint(Mmodel4[[i]])[4:11,], c("Age","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction"), c(replicate(11, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$CI2.5 = as.numeric(as.character(wellbeing_tab$CI2.5))
wellbeing_tab$CI97.5 = as.numeric(as.character(wellbeing_tab$CI97.5))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
Mwellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: BAG ~ Life Satisfaction Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) + ylim(-0.75, 0.5)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMWellbeingPlot.pdf",MWellbeing_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMWellbeingPlot.pdf",MWellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(summary(Mmodel5[[i]])$coefficients[2:9,1], confint(Mmodel5[[i]])[4:11,], c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered"), c(replicate(10, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(summary(model1[[i]])$coefficients[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% rename("Beta" = "V1",
                                         "CI2.5" = "X2.5..",
                                         "CI97.5" = "X97.5..",
                                         "Variable" = "V4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$CI2.5 = as.numeric(as.character(cognition_tab$CI2.5))
cognition_tab$CI97.5 = as.numeric(as.character(cognition_tab$CI97.5))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
Mcognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=CI2.5, ymax=CI97.5, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: BAG ~ Cognitive Factors",x="Predictors of BAG", y = "Beta with 95% CI")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) + ylim(-0.75, 0.6)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMcognitionPlot.pdf",Mcognition_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMcognitionPlot.pdf",Mcognition_plot, height = 6, width = 9)


### ARRANGE PLOTS #
Sbeta = ggarrange(Fsociodemo_plot, Msociodemo_plot, ncol = 2, common.legend = T,legend = "bottom")
Hbeta = ggarrange(Fhealth_plot, Mhealth_plot, ncol = 2, common.legend = T,legend = "bottom")
Wbeta = ggarrange(Fwellbeing_plot, Mwellbeing_plot, ncol = 2, common.legend = T,legend = "bottom")
Cbeta = ggarrange(Fcognition_plot, Mcognition_plot, ncol = 2, common.legend = T,legend = "bottom")

# all in one
plotall = ggarrange(Sbeta, Hbeta, Wbeta, Cbeta, ncol = 1, nrow = 4, common.legend = T,legend = "bottom")
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSEXsociodemoPlot.pdf",plotall, height = 18, width = 12)
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSF3.pdf",plotall, height = 18, width = 12)