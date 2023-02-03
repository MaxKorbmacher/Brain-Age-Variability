# BAG and bio-psycho-social factors
# BAG was calculated from diffusion data using six diffusion approaches: BRIA, DKI, DTI, SMT, mcSMT, WMTI
# code by Max Korbmacher, 14.10.2022
# last update: 02.02.2023 (use of standardized betas in figures)

### PREPARATION ####
# packages
#install.packages("tidyverse")
library(sjPlot)
library(car)
library(ggplotify)
library(pheatmap)
library(rstatix)
library(cocor)
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
#install.packages("MuMIn")
library(MuMIn)
library(rstatix)
#install.packages("sjPlot")
library(sjPlot)
library(boot)

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

# calculate the corrected brain age gap as brainage_gap
BAG$brainage_gap = (BAG$pred_age_dwMRI_test90_no_SD - BAG$age)
BAG_BRIA$brainage_gap = (BAG_BRIA$pred_age_Bayes_no_SD_test - BAG_BRIA$age)
BAG_DKI$brainage_gap = (BAG_DKI$pred_age_DKI_no_SD_test - BAG_DKI$age)
BAG_DTI$brainage_gap =( BAG_DTI$pred_age_DTI_no_SD_test - BAG_DTI$age)
BAG_MEAN$brainage_gap = (BAG_MEAN$pred_age_MEAN_no_SD_test - BAG_MEAN$age)
BAG_SMT$brainage_gap = (BAG_SMT$pred_age_SMT_no_SD_test - BAG_SMT$age)
BAG_SMT_mc$brainage_gap = (BAG_SMT_mc$pred_age_SMT_mc_no_SD_test - BAG_SMT_mc$age)
BAG_WMTI$brainage_gap = (BAG_WMTI$pred_age_WMTI_no_SD_test - BAG_WMTI$age)


#process data
demo21 = demo2 %>% subset(select = c(-X, -sex, -age, -birth_weight_t2, -birth_weight_t3)) %>% select(!contains("t4"))
demo21$mean_inc_pair_matches = rowSums(demo21[ ,c("inc_pair_matches_t3r1","inc_pair_matches_t3r2","excl_sample.inc_pair_matches_t3r3")],na.rm = FALSE)/3
demo21 = demo21 %>% subset(select = c(-inc_pair_matches_t3r1, -inc_pair_matches_t3r2, -excl_sample.inc_pair_matches_t3r3))
df = merge(demo, BAG, by="eid")
df = df %>% subset(select=-c(X))
#df = df %>% dplyr::rename(brainage_gap = brainage_gap_dwMRI_test90_no_SD,
#                   BAG_residual = BAG_residual_dwMRI_test90_no_SD,
#                   BAG_corrected_age = BAG_corrected_dwMRI_test90_no_SD)
df = merge(df, demo21, by = "eid")
df = df %>% subset(select = c(-waist_circumference, -hip_circumference, -height, -weight, -Assessment_centre, matrix_puzzles_viewed_t3))
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

# dplyr::rename wrongly labelled tower arranging var
df = df %>% dplyr::rename("tower_arranging" = "matrix_puzzles_correct_t3")

# DESCRIPTIVES & BIVARIATE RELATIONSHIPS ####

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## 1) get mean and sd in a table for all independent variables ########
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# print column names for the following output
a = colnames(df)
# print mean values
b = c()
for (i in 1:ncol(df)){b[i] = (mean(as.numeric(df[,i]), na.rm = TRUE))}
# print sd
c = c()
for (i in 1:ncol(df)){c[i] = (sd(as.numeric(df[,i]), na.rm = TRUE))}
# print table
(data.frame(a,b,c))

## get percentages for the rest of the nominal independent variables
prop.table(table(df$site_t3))
prop.table(table(df$t3_income))
prop.table(table(df$smoking))

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

nrow(df) - sum(is.na(df$BMI)) ### Health and Lifestyle Factors
nrow(df) - sum(is.na(df$pulse_pressure))
nrow(df) - sum(is.na(df$WHR))
table(df$smoking)
table(df$diabetic)
table(df$hypertension)
table(df$high_cholesterol)
table(df$diagnosed_vascular_problem)
nrow(df) - sum(is.na(df$birth_weight_t1))
nrow(df) - sum(is.na(df$daily_coffee_t3))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## 2) get Brain age vals for each group and relate them with bio-psycho-socialfactors ###### 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# define a function to extract standardised beta coefficients from the mixed linear models (lmer)
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#### starting with demographics
mean(df$pred_age_dwMRI_test90_no_SD)
sd(df$pred_age_dwMRI_test90_no_SD)
df$brainage_gap = df$pred_age_dwMRI_test90_no_SD # label perdicted age as brainage_gap
# scanner site
df %>% group_by(Assessment_centre) %>% dplyr::summarize(Mean = mean(brainage_gap))
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df)
summary(baseline_model)
stdCoef.merMod(baseline_model)

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
df_red = df %>% select(Ethnicity,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
ethn_model = lmer(brainage_gap ~  Ethnicity+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(ethn_model) - r.squaredGLMM(baseline_model)
anova(baseline_model, ethn_model)
stdCoef.merMod(ethn_model)

# income (without those who did not want to tell or didn't know)
df %>% group_by(t3_income) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(t3_income,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  t3_income+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# higher ed
df %>% group_by(higher_education_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(higher_education_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  higher_education_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)



#### cognitive scores

# matrix puzzles solved
df %>% drop_na(matrix_puzzles_solved_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(matrix_puzzles_solved_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  matrix_puzzles_solved_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# Tower rearranging correct
df %>% drop_na(tower_arranging) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(tower_arranging,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  tower_arranging+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# prospective memory
df %>% drop_na(prospective_memory_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(prospective_memory_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  prospective_memory_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# fluid intel
df %>% drop_na(fluid_intelligence_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(fluid_intelligence_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  fluid_intelligence_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# digits remembered
df %>% drop_na(digits_remembered_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(digits_remembered_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  digits_remembered_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# mean number of correct pair matches across trials
df %>% drop_na(mean_inc_pair_matches) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(mean_inc_pair_matches,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  mean_inc_pair_matches+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


###### life satisfaction
# job sat
df %>% drop_na(job_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(job_satisfaction_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  job_satisfaction_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


#finance sat
df %>% drop_na(excl_sample.finance_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(excl_sample.finance_satisfaction_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  excl_sample.finance_satisfaction_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# health sat
df %>% drop_na(excl_sample.health_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(excl_sample.health_satisfaction_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  excl_sample.health_satisfaction_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# overall health rating
df %>% drop_na(overall_health_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(overall_health_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  overall_health_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# family rel sat
df %>% drop_na(excl_sample.family_rel_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(excl_sample.family_rel_satisfaction_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  excl_sample.family_rel_satisfaction_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# friend rel sat
df %>% drop_na(excl_sample.friend_rel_satisfaction_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(excl_sample.friend_rel_satisfaction_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  excl_sample.friend_rel_satisfaction_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# happiness
df %>% drop_na(happiness_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(happiness_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  happiness_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


#### Health and Lifestyle Factors
# BMI
df %>% drop_na(BMI) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(BMI,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  BMI+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# pulse pressure
df %>% drop_na(pulse_pressure) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(pulse_pressure,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  pulse_pressure+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# waist to hip ratio
df %>% drop_na(WHR) %>% dplyr::summarize(Mean = mean(WHR), SD = sd(WHR))
df_red = df %>% select(WHR,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  WHR+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# we can also separate by sex, as this is informative
df %>% drop_na(WHR) %>% group_by(sex)%>% dplyr::summarize(Mean = mean(WHR), SD = sd(WHR))


# smoking
df %>% group_by(smoking) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(smoking,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  smoking+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# diabetes
df %>% group_by(diabetic) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(diabetic,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  diabetic+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# hypertension
df %>% group_by(hypertension) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(hypertension,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  hypertension+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

# high chol
df %>% group_by(high_cholesterol) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(high_cholesterol,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  high_cholesterol+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# diagnosed vasc problems
df %>% group_by(diagnosed_vascular_problem) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(diagnosed_vascular_problem,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  diagnosed_vascular_problem+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# birth weight
df %>% drop_na(birth_weight_t1) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(birth_weight_t1,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  birth_weight_t1+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)


# daily coffe intake
df %>% drop_na(daily_coffee_t3) %>% dplyr::summarize(Mean = mean(brainage_gap))
df_red = df %>% select(daily_coffee_t3,brainage_gap, age,sex, Assessment_centre) %>% na.omit()
var_model = lmer(brainage_gap ~  daily_coffee_t3+age*sex + (1|Assessment_centre), data = df_red)
baseline_model = lmer(brainage_gap ~  age*sex + (1|Assessment_centre), data = df_red)
r.squaredGLMM(var_model) - r.squaredGLMM(baseline_model)
summary(var_model)
anova(baseline_model, var_model)
stdCoef.merMod(var_model)

#Group diff:
# sex = -0.09
# hypert = 0.06 & vascular diag = 0.06

# PP = 0.05
# WHR = 0.07
# overall health = 0.04

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
MLM_health = lmer(brainage_gap ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                    high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, df)
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

## 1.1) rename brainage
BAG_MEAN = BAG_MEAN %>% dplyr::rename("brainage" = "pred_age_MEAN_no_SD_test")
BAG_BRIA= BAG_BRIA %>% dplyr::rename("brainage" = "pred_age_Bayes_no_SD_test")
BAG_DKI= BAG_DKI %>% dplyr::rename("brainage" = "pred_age_DKI_no_SD_test")
BAG_DTI= BAG_DTI %>% dplyr::rename("brainage" = "pred_age_DTI_no_SD_test")
BAG_SMT= BAG_SMT %>% dplyr::rename("brainage" = "pred_age_SMT_no_SD_test")
BAG_SMT_mc= BAG_SMT_mc %>% dplyr::rename("brainage" = "pred_age_SMT_mc_no_SD_test")
BAG_WMTI= BAG_WMTI %>% dplyr::rename("brainage" = "pred_age_WMTI_no_SD_test")
BAG= BAG %>% dplyr::rename("brainage" = "pred_age_dwMRI_test90_no_SD")

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
FULL = merge(df_new, BAG, by = "eid")
#FULL$age = df$age
#MEAN = MEAN %>% dplyr::rename("brainage_gap" = "brainage_gap_MEAN_no_SD_test")
#BRIA= BRIA %>% dplyr::rename("brainage_gap" = "brainage_gap_Bayes_no_SD_test")
#DKI= DKI %>% dplyr::rename("brainage_gap" = "brainage_gap_DKI_no_SD_test")
#DTI= DTI %>% dplyr::rename("brainage_gap" = "brainage_gap_DTI_no_SD_test")
#SMT= SMT %>% dplyr::rename("brainage_gap" = "brainage_gap_SMT_no_SD_test")
#SMT_mc= SMT_mc %>% dplyr::rename("brainage_gap" = "brainage_gap_SMT_mc_no_SD_test")
#WMTI= WMTI %>% dplyr::rename("brainage_gap" = "brainage_gap_WMTI_no_SD_test")

# create a list with all the data frames
brainage_list = list(FULL, MEAN, BRIA, DKI, DTI, SMT, SMT_mc, WMTI)

# loop over list for predictions

## ## ## NULL MODEL 0.1 ## ## ## 
model0.1 <- list()
for (i in 1:8) {model0.1[[i]] = lmer(brainage~ 1 + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.1_metrics[i,] = c(r.squaredGLMM(model0.1[[i]]),AIC(model0.1[[i]]),(sigma(model0.1[[i]]))^2,summary(model0.1[[i]])$logLik[1])} 

## ## ## NULL MODEL 0.2 ## ## ## 
model0.2 <- list()
for (i in 1:8) {model0.2[[i]] = lmer(brainage~ 1 + sex + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.2_metrics[i,] = c(r.squaredGLMM(model0.2[[i]]),AIC(model0.2[[i]]),(sigma(model0.2[[i]]))^2,summary(model0.2[[i]])$logLik[1])} 

## ## ## BASELINE MODEL 1 ## ## ## 
model1 <- list()
for (i in 1:8) {model1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model1_metrics[i,] = c(r.squaredGLMM(model1[[i]]),AIC(model1[[i]]),(sigma(model1[[i]]))^2,summary(model1[[i]])$logLik[1])} 

## ## ## SOCIODEMO MODEL 2 ## ## ## 
model2 <- list()
for (i in 1:8) {model2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model2_metrics[i,] = c(r.squaredGLMM(model2[[i]]),AIC(model2[[i]]),(sigma(model2[[i]]))^2,summary(model2[[i]])$logLik[1])} 

## ## ## HEALTH MODEL 3 ## ## ## 
model3 <- list()
for (i in 1:8) {model3[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                     high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3_metrics[i,] = c(r.squaredGLMM(model3[[i]]),AIC(model3[[i]]),(sigma(model3[[i]]))^2,summary(model3[[i]])$logLik[1])} 

## ## ## WELLBEING MODEL 4 ## ## ## 
model4 <- list()
for (i in 1:8) {model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4_metrics[i,] = c(r.squaredGLMM(model4[[i]]),AIC(model4[[i]]),(sigma(model4[[i]]))^2,summary(model4[[i]])$logLik[1])} 


## ## ## COGNITION MODEL 5 ## ## ## 
model5 <- list()
for (i in 1:8) {model5[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5_metrics[i,] = c(r.squaredGLMM(model5[[i]]),AIC(model5[[i]]),(sigma(model5[[i]]))^2,summary(model5[[i]])$logLik[1])} 
r.squaredGLMM(model5[[1]])
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
## ## CREATE ONE SINGLE DF FROM ALL THE OUTPUT ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ###
output = rbind(model0.1_metrics,model0.2_metrics, model1_metrics, model2_metrics, model3_metrics, model4_metrics, model5_metrics)
output$Model = c(replicate(8,"random only"), replicate(8,"sex"), replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
output$BAG = c(replicate(7, dMRI_model))
output = output %>% dplyr::rename("R2M"="X1",
                                  "R2C" = "X2",
                                  "AIC" = "X3",
                                  "sig2" = "X4",
                                  "logLik" = "X5")
write.csv(output, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST10.csv")

output %>% filter(Model == "health") %>% dplyr::summarize(MeanM = mean(R2M), SDM = sd(R2M),MeanC = mean(R2C), SDC = sd(R2C))
output %>% filter(Model == "wellbeing") %>% dplyr::summarize(MeanM = mean(R2M), SDM = sd(R2M),MeanC = mean(R2C), SDC = sd(R2C))
output %>% filter(Model == "cognition") %>% dplyr::summarize(MeanM = mean(R2M), SDM = sd(R2M),MeanC = mean(R2C), SDC = sd(R2C))
output %>% filter(Model == "sociodemo") %>% dplyr::summarize(MeanM = mean(R2M), SDM = sd(R2M),MeanC = mean(R2C), SDC = sd(R2C))
output %>% filter(Model == "baseline") %>% dplyr::summarize(MeanM = mean(R2M), SDM = sd(R2M),MeanC = mean(R2C), SDC = sd(R2C))

##########################################
### check differences in models############
###########################################
# this is not so straight forward due to missingness in the data
# we therefore have to re-fit the baseline model to the data corresponding each desired comparison
# however, a few model comparisons work:
baseline_anova = list()
for (i in 1:8){baseline_anova[[i]] = anova(model0.1[[i]], model0.2[[i]], model1[[i]])}
# make table
baseline_table = baseline_anova %>% bind_rows() %>% group_split()
baseline_table = do.call(rbind.data.frame,baseline_table)
baseline_table = baseline_table%>% dplyr::select(-npar, -Df) %>% mutate(Model = c(replicate(3,"FULL"),
                                                                                  replicate(3,"MEAN"), 
                                                                                  replicate(3,"BRIA"),
                                                                                  replicate(3,"DKI"),
                                                                                  replicate(3,"DTI"),
                                                                                  replicate(3,"SMT"),
                                                                                  replicate(3,"SMT_mc"),
                                                                                  replicate(3,"WMTI")))
# this table indicates differences between (1) a model containing nothin but RF scanner site, (2) sex & RF, (3) sex*age & RF
baseline_table

# re-fit baseline models for comparison 
# Comparison of baseline with model 2
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model2[[i]]))])
model1[[i]] = lmer(brainage~ 1 + sex*age + (1|site_t3), data=data)
model2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=data)
} # re-fit based on model2 data

model2_anova = list()
for (i in 1:8){model2_anova[[i]] = anova(model1[[i]], model2[[i]])} # calculate differences
# make table
## starting with the names
mod_nam = c(replicate(2,"FULL"),replicate(2,"MEAN"), replicate(2,"BRIA"),replicate(2,"DKI"),replicate(2,"DTI"),replicate(2,"SMT"),replicate(2,"SMT_mc"),replicate(2,"WMTI"))
##
model2_table = model2_anova %>% bind_rows() %>% group_split()
model2_table = do.call(rbind.data.frame,model2_table)
model2_table = model2_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)

# Comparison with model 3
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model3[[i]]))])
model1[[i]] = lmer(brainage~ 1 + sex*age + (1|site_t3), data=data)
model3[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=data)
} # re-fit based on model3 data

model3_anova = list()
for (i in 1:8){model3_anova[[i]] = anova(model1[[i]], model3[[i]])} # calculate differences
# make table
model3_table = model3_anova %>% bind_rows() %>% group_split()
model3_table = do.call(rbind.data.frame,model3_table)
model3_table = model3_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)

# Comparison with model 4
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model4[[i]]))])
model1[[i]] = lmer(brainage~ 1 + sex*age + (1|site_t3), data=data)
model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=data)}
model4_anova = list()
for (i in 1:8){model4_anova[[i]] = anova(model1[[i]], model4[[i]])} # calculate differences
# make table
model4_table = model4_anova %>% bind_rows() %>% group_split()
model4_table = do.call(rbind.data.frame,model4_table)
model4_table = model4_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)

# Comparison with model 5
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model5[[i]]))])
model1[[i]] = lmer(brainage~ 1 + sex*age + (1|site_t3), data=data)
model5[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=data)}
model5_anova = list()
for (i in 1:8){model5_anova[[i]] = anova(model1[[i]], model5[[i]])} # calculate differences
# make table
model5_table = model5_anova %>% bind_rows() %>% group_split()
model5_table = do.call(rbind.data.frame,model5_table)
model5_table = model5_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)

## save tables
## This first table is not used in the supplement or manuscript. However, it can address questions on the differences between null and baseline models
write.csv(baseline_table, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST2_0.csv")
write.csv(model2_table, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST2_1.csv")
write.csv(model3_table,"/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST2_2.csv")
write.csv(model4_table,"/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST2_3.csv")
write.csv(model5_table, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export/ST2_4.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## third, loop over all models and create beta tables and figures  #####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

############## BASELINE MODEL
# fit model1 again to all data
model1 <- list()
for (i in 1:8) {model1[[i]] = lmer(brainage_gap~ 1 + age + sex + age:sex + (1|site_t3), data=brainage_list[[i]])}
baseline = list()
for (i in 1:8){
  baseline[[i]] = cbind(stdCoef.merMod(model1[[i]])[2:4,1], stdCoef.merMod(model1[[i]])[2:4,2], c("Age","Sex","Age*Sex"), c(replicate(3, "Baseline")))
} 
baseline_tab = data.frame(do.call(rbind, baseline))
dMRI_model_list = c(replicate(3,"FULL"), replicate(3,"MEAN"), replicate(3,"BRIA"), replicate(3,"DKI"),
                    replicate(3,"DTI"), replicate(3,"SMT"), replicate(3,"SMT_mc"), replicate(3,"WMTI"))
baseline_tab$Model = dMRI_model_list
baseline_tab = baseline_tab %>% dplyr::rename("Beta" = "X1",
                                              "SD" = "X2",
                                              "Variable" = "X3",
                                              "Predictors" = "X4")
levels(baseline_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
baseline_tab$Beta = as.numeric(as.character(baseline_tab$Beta))
baseline_tab$SD = as.numeric(as.character(baseline_tab$SD))
baseline_tab$Model = (as.factor(baseline_tab$Model))
baseline_tab$Variable = (as.factor(baseline_tab$Variable))

# Basic bar plot
baseline_plot = ggplot(baseline_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()

####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(stdCoef.merMod(model2[[i]])[2:8,1], stdCoef.merMod(model2[[i]])[2:8,2], c("Age","Sex","Ethnicity","Higher Education","Income","Social Visits","Age*Sex"), c(replicate(7, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "sociodemo")))
sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(7,"FULL"), replicate(7,"MEAN"), replicate(7,"BRIA"), replicate(7,"DKI"),
                    replicate(7,"DTI"), replicate(7,"SMT"), replicate(7,"SMT_mc"), replicate(7,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% dplyr::rename("Beta" = "X1",
                                              "SD" = "X2",
                                              "Variable" = "X3",
                                              "Predictors" = "X4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$SD = as.numeric(as.character(sociodemo_tab$SD))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = (as.factor(sociodemo_tab$Variable))

# Basic plot
sociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersSociodemoPlot.pdf",sociodemo_plot, height = 6, width = 9)
ggsave("/tsd/p33/home/p33-maxk/export/FrontiersSociodemoPlot.pdf",sociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
health = list()
for (i in 1:8){
  health[[i]] = cbind(stdCoef.merMod(model3[[i]])[2:16,1], stdCoef.merMod(model3[[i]])[2:16,2], c("Age","Sex","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee","Smoking","Age*Sex"), c(replicate(15, "health")))
} # to get the intercept as well: # health[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(15,"FULL"), replicate(15,"MEAN"), replicate(15,"BRIA"), replicate(15,"DKI"),
                    replicate(15,"DTI"), replicate(15,"SMT"), replicate(15,"SMT_mc"), replicate(15,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% dplyr::rename("Beta" = "X1",
                                          "SD" = "X2",
                                          "Variable" = "X3",
                                          "Predictors" = "X4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$SD = as.numeric(as.character(health_tab$SD))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (as.factor(health_tab$Variable))

# Basic bar plot
health_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersHealthPlot.pdf",health_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersHealthPlot.pdf",health_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(stdCoef.merMod(model4[[i]])[2:11,1], stdCoef.merMod(model4[[i]])[2:11,2], c("Age","Sex","Job Satisfaction", "Finance Satisfaction", "Health Satisfaction",  "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction","Self-Rated Health","Age*Sex"), c(replicate(10, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(10,"FULL"), replicate(10,"MEAN"), replicate(10,"BRIA"), replicate(10,"DKI"),
                    replicate(10,"DTI"), replicate(10,"SMT"), replicate(10,"SMT_mc"), replicate(10,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$SD = as.numeric(as.character(wellbeing_tab$SD))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Sex","Age*Sex","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
wellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/Figure4.pdf",wellbeing_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersWellbeingPlot.pdf",wellbeing_plot, height = 6, width = 9)


################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(stdCoef.merMod(model5[[i]])[2:11,1], stdCoef.merMod(model5[[i]])[2:11,2], c("Age","Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered","Age*Sex"), c(replicate(10, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(10,"FULL"), replicate(10,"MEAN"), replicate(10,"BRIA"), replicate(10,"DKI"),
                    replicate(10,"DTI"), replicate(10,"SMT"), replicate(10,"SMT_mc"), replicate(10,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$SD = as.numeric(as.character(cognition_tab$SD))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Sex","Age*Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
cognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
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
baseline_p = data.frame(do.call(rbind, baseline_p))
baseline_p$names = names1
baseline_p$stat_model = c(replicate(4,"Full"), replicate(4,"Mean"), replicate(4,"BRIA"),replicate(4,"DKI"), replicate(4,"DTI"), replicate(4,"SMT"), replicate(4,"SMTmc"), replicate(4,"WMTI"))
baseline_p$corrected_p = baseline_p$Pr...t.. * 8
baseline_p %>% filter(corrected_p < 0.05)

social_p = list()
for (i in 1:8){social_p[[i]] = cbind(summary(model2[[i]])$coefficients)} 
social_p = data.frame(do.call(rbind, social_p))
names = rownames(summary(model2[[1]])$coefficients)
names1 = c(rep(names, 8))
social_p = data.frame(do.call(cbind, social_p))
social_p$names = names1
social_p$stat_model = c(replicate((length(names1)/8),"Full"), replicate((length(names1)/8),"Mean"), replicate((length(names1)/8),"BRIA"),replicate((length(names1)/8),"DKI"), replicate((length(names1)/8),"DTI"), replicate((length(names1)/8),"SMT"), replicate((length(names1)/8),"SMTmc"), replicate((length(names1)/8),"WMTI"))
social_p$corrected_p = social_p$Pr...t.. * 8
social_p %>% filter(corrected_p < 0.05)%>% arrange(names)

health_p = list()
for (i in 1:8){health_p[[i]] = cbind(summary(model3[[i]])$coefficients)} 
health_p = data.frame(do.call(rbind, health_p))
names = rownames(summary(model3[[1]])$coefficients)
names1 = c(rep(names, 8))
health_p = data.frame(do.call(cbind, health_p))
health_p$names = names1
health_p$stat_model = c(replicate((length(names1)/8),"Full"), replicate((length(names1)/8),"Mean"), replicate((length(names1)/8),"BRIA"),replicate((length(names1)/8),"DKI"), replicate((length(names1)/8),"DTI"), replicate((length(names1)/8),"SMT"), replicate((length(names1)/8),"SMTmc"), replicate((length(names1)/8),"WMTI"))
health_p$corrected_p = health_p$Pr...t.. * 8
health_p %>% filter(corrected_p < 0.05) %>% arrange(names)


wellbeing_p = list()
for (i in 1:8){wellbeing_p[[i]] = cbind(summary(model4[[i]])$coefficients)} 
wellbeing_p = data.frame(do.call(rbind, wellbeing_p))
names = rownames(summary(model4[[1]])$coefficients)
names1 = c(rep(names, 8))
wellbeing_p = data.frame(do.call(cbind, wellbeing_p))
wellbeing_p$names = names1
wellbeing_p$stat_model = c(replicate((length(names1)/8),"Full"), replicate((length(names1)/8),"Mean"), replicate((length(names1)/8),"BRIA"),replicate((length(names1)/8),"DKI"), replicate((length(names1)/8),"DTI"), replicate((length(names1)/8),"SMT"), replicate((length(names1)/8),"SMTmc"), replicate((length(names1)/8),"WMTI"))
wellbeing_p$corrected_p = wellbeing_p$Pr...t.. * 8
wellbeing_p %>% filter(corrected_p < 0.05) %>% arrange(names)


cognitive_p = list()
for (i in 1:8){cognitive_p[[i]] = cbind(summary(model5[[i]])$coefficients)} 
cognitive_p = data.frame(do.call(rbind, cognitive_p))
names = rownames(summary(model5[[1]])$coefficients)
names1 = c(rep(names, 8))
cognitive_p = data.frame(do.call(cbind, cognitive_p))
cognitive_p$names = names1
cognitive_p$stat_model = c(replicate((length(names1)/8),"Full"), replicate((length(names1)/8),"Mean"), replicate((length(names1)/8),"BRIA"),replicate((length(names1)/8),"DKI"), replicate((length(names1)/8),"DTI"), replicate((length(names1)/8),"SMT"), replicate((length(names1)/8),"SMTmc"), replicate((length(names1)/8),"WMTI"))
cognitive_p$corrected_p = cognitive_p$Pr...t.. * 8
cognitive_p %>% filter(corrected_p < 0.05) %>% arrange(names)


#########################
# MIXED MODELS BY SEX ####
## ## ## ## ## ## ## ## ## 
df_new = df %>% select(-brainage_gap, -age)
MEAN = merge(df_new, BAG_MEAN, by = "eid")
BRIA = merge(df_new, BAG_BRIA, by = "eid")
DKI = merge(df_new, BAG_DKI, by = "eid")
DTI = merge(df_new, BAG_DTI, by = "eid")
SMT = merge(df_new, BAG_SMT, by = "eid")
SMT_mc = merge(df_new, BAG_SMT_mc, by = "eid")
WMTI = merge(df_new, BAG_WMTI, by = "eid")
FULL = df_new
FULL$age = df$age

# MEAN = MEAN %>% dplyr::rename("brainage" = "pred_age_MEAN_no_SD_test")
# BRIA= BRIA %>% dplyr::rename("brainage" = "pred_age_Bayes_no_SD_test")
# DKI= DKI %>% dplyr::rename("brainage" = "pred_age_DKI_no_SD_test")
# DTI= DTI %>% dplyr::rename("brainage" = "pred_age_DTI_no_SD_test")
# SMT= SMT %>% dplyr::rename("brainage" = "pred_age_SMT_no_SD_test")
# SMT_mc= SMT_mc %>% dplyr::rename("brainage" = "pred_age_SMT_mc_no_SD_test")
# WMTI = WMTI %>% dplyr::rename("brainage" = "pred_age_WMTI_no_SD_test")
FULL = FULL %>% dplyr::rename("brainage" = "pred_age_dwMRI_test90_no_SD")
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
for (i in 1:8) {Fmodel1[[i]] = lmer(brainage~ 1 + age  + (1|site_t3), data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel1_metrics[i,] = c(r.squaredGLMM(Fmodel1[[i]]),AIC(Fmodel1[[i]]),(sigma(Fmodel1[[i]]))^2,summary(Fmodel1[[i]])$logLik[1])} 

## ## ## SOCIODEMO Fmodel 2 ## ## ## 
Fmodel2 <- list()
for (i in 1:8) {Fmodel2[[i]] = lmer(brainage~ 1 + age  + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel2_metrics[i,] = c(r.squaredGLMM(Fmodel2[[i]]),AIC(Fmodel2[[i]]),(sigma(Fmodel2[[i]]))^2,summary(Fmodel2[[i]])$logLik[1])} 

## ## ## HEALTH Fmodel 3 ## ## ## 
Fmodel3 <- list()
for (i in 1:8) {Fmodel3[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                      high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel3_metrics[i,] = c(r.squaredGLMM(Fmodel3[[i]]),AIC(Fmodel3[[i]]),(sigma(Fmodel3[[i]]))^2,summary(Fmodel3[[i]])$logLik[1])} 

## ## ## WELLBEING Fmodel 4 ## ## ## 
Fmodel4 <- list()
for (i in 1:8) {Fmodel4[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                      excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel4_metrics[i,] = c(r.squaredGLMM(Fmodel4[[i]]),AIC(Fmodel4[[i]]),(sigma(Fmodel4[[i]]))^2,summary(Fmodel4[[i]])$logLik[1])} 


## ## ## COGNITION Fmodel 5 ## ## ## 
Fmodel5 <- list()
for (i in 1:8) {Fmodel5[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                      excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_females[[i]])}
# loop over output data to create a table with Fmodel metrics
Fmodel5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Fmodel5_metrics[i,] = c(r.squaredGLMM(Fmodel5[[i]]),AIC(Fmodel5[[i]]),(sigma(Fmodel5[[i]]))^2,summary(Fmodel5[[i]])$logLik[1])} 





## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## RUN MIXED MODELS FOR MALES ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## BASELINE MODEL 1 ## ## ## 
Mmodel1 <- list()
for (i in 1:8) {Mmodel1[[i]] = lmer(brainage~ 1 + age  + (1|site_t3), data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel1_metrics[i,] = c(r.squaredGLMM(Mmodel1[[i]]),AIC(Mmodel1[[i]]),(sigma(Mmodel1[[i]]))^2,summary(Mmodel1[[i]])$logLik[1])} 

## ## ## SOCIODEMO Mmodel 2 ## ## ## 
Mmodel2 <- list()
for (i in 1:8) {Mmodel2[[i]] = lmer(brainage~ 1 + age  + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel2_metrics[i,] = c(r.squaredGLMM(Mmodel2[[i]]),AIC(Mmodel2[[i]]),(sigma(Mmodel2[[i]]))^2,summary(Mmodel2[[i]])$logLik[1])} 

## ## ## HEALTH Mmodel 3 ## ## ## 
Mmodel3 <- list()
for (i in 1:8) {Mmodel3[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                      high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel3_metrics[i,] = c(r.squaredGLMM(Mmodel3[[i]]),AIC(Mmodel3[[i]]),(sigma(Mmodel3[[i]]))^2,summary(Mmodel3[[i]])$logLik[1])} 

## ## ## WELLBEING Mmodel 4 ## ## ## 
Mmodel4 <- list()
for (i in 1:8) {Mmodel4[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                      excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_males[[i]])}
# loop over output data to create a table with Mmodel metrics
Mmodel4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {Mmodel4_metrics[i,] = c(r.squaredGLMM(Mmodel4[[i]]),AIC(Mmodel4[[i]]),(sigma(Mmodel4[[i]]))^2,summary(Mmodel4[[i]])$logLik[1])} 


## ## ## COGNITION Mmodel 5 ## ## ## 
Mmodel5 <- list()
for (i in 1:8) {Mmodel5[[i]] = lmer(brainage ~ 1 + age  + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
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
Foutput = Foutput %>% dplyr::rename("R2M"="X1",
                                    "R2C" = "X2",
                                    "AIC" = "X3",
                                    "sig2" = "X4",
                                    "logLik" = "X5")
write.csv(Foutput, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export//FrontiersST3_females.csv")
# males
Moutput = rbind(Mmodel1_metrics, Mmodel2_metrics, Mmodel3_metrics, Mmodel4_metrics, Mmodel5_metrics)
Moutput$Model = c(replicate(8,"baseline"), replicate(8,"sociodemo"), replicate(8,"health"), replicate(8,"wellbeing"), replicate(8,"cognition"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
Moutput$BAG = c(replicate(5, dMRI_model))
Moutput = Moutput %>% dplyr::rename("R2M"="X1",
                                    "R2C" = "X2",
                                    "AIC" = "X3",
                                    "sig2" = "X4",
                                    "logLik" = "X5")
write.csv(Moutput, "/cluster/projects/p33/users/maxk/UKB/Frontiers_export/FrontiersST3_males.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## loop over all models and create beta tables and figures  ###########
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# FEMALES

####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(stdCoef.merMod(Fmodel2[[i]])[2:6,1], stdCoef.merMod(Fmodel2[[i]])[2:6,2], c("Age","Ethnicity","Higher Education","Income","Social Visits"), c(replicate(5, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(stdCoef.merMod(Fmodel1[[i]])[,1], confint(Fmodel1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "sociodemo")))
sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(5,"FULL"), replicate(5,"MEAN"), replicate(5,"BRIA"), replicate(5,"DKI"),
                    replicate(5,"DTI"), replicate(5,"SMT"), replicate(5,"SMT_mc"), replicate(5,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$SD = as.numeric(as.character(sociodemo_tab$SD))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Age","Ethnicity","Income","Higher Education", "Social Visits"))

# Basic plot
Fsociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: Brain Age ~ Sociodemographic Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  theme_bw()
#ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/FrontiersFSociodemoPlot.pdf",Fsociodemo_plot, height = 6, width = 9)
#ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFSociodemoPlot.pdf",Fsociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
pred_names = c("Age","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee", "Smoking")

health = list()
for (i in 1:8){
  health[[i]] = cbind(stdCoef.merMod(Fmodel3[[i]])[2:14,1], stdCoef.merMod(Fmodel3[[i]])[2:14,2],pred_names, c(replicate(13, "health")))
} # to get the intercept as well: # health[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(length(pred_names),"FULL"), replicate(length(pred_names),"MEAN"), replicate(length(pred_names),"BRIA"), replicate(length(pred_names),"DKI"),
                    replicate(length(pred_names),"DTI"), replicate(length(pred_names),"SMT"), replicate(length(pred_names),"SMT_mc"), replicate(length(pred_names),"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% dplyr::rename("Beta" = "V1",
                                          "SD" = "V2",
                                          "Predictors" = "V4",
                                          "Variable" = "pred_names")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$SD = as.numeric(as.character(health_tab$SD))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol", "Smoking")))

# Basic bar plot
Fhealth_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: Brain Age ~ Health and Lifestyle Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFHealthPlot.pdf",Fhealth_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFHealthPlot.pdf",Fhealth_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(stdCoef.merMod(Fmodel4[[i]])[2:9,1], stdCoef.merMod(Fmodel4[[i]])[2:9,2], c("Age","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction"), c(replicate(8, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$SD = as.numeric(as.character(wellbeing_tab$SD))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
Fwellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: Brain Age ~ Life Satisfaction Factors", x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFWellbeingPlot.pdf",Fwellbeing_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFWellbeingPlot.pdf",Fwellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(stdCoef.merMod(Fmodel5[[i]])[2:9,1], stdCoef.merMod(Fmodel5[[i]])[2:9,2], c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered"), c(replicate(8, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$SD = as.numeric(as.character(cognition_tab$SD))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
Fcognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Females: Brain Age ~ Cognitive Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
##ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersFCognitionPlot.pdf",Fcognition_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersFCognitionPlot.pdf",Fcognition_plot, height = 6, width = 9)






# MALES

####################### SOCIODEMO MODEL
sociodemo = list()
for (i in 1:8){
  sociodemo[[i]] = cbind(stdCoef.merMod(Mmodel2[[i]])[2:6,1], stdCoef.merMod(Mmodel2[[i]])[2:6,2], c("Age","Ethniciy","Higher Education","Income","Social Visits"), c(replicate(5, "sociodemo")))
} # to get the intercept as well: # sociodemo[[i]] = cbind(stdCoef.merMod(Mmodel1[[i]])[,1], confint(Mmodel1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "sociodemo")))

sociodemo_tab = data.frame(do.call(rbind, sociodemo))
dMRI_model_list = c(replicate(5,"FULL"), replicate(5,"MEAN"), replicate(5,"BRIA"), replicate(5,"DKI"),
                    replicate(5,"DTI"), replicate(5,"SMT"), replicate(5,"SMT_mc"), replicate(5,"WMTI"))
sociodemo_tab$Model = dMRI_model_list
sociodemo_tab = sociodemo_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(sociodemo_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
sociodemo_tab$Beta = as.numeric(as.character(sociodemo_tab$Beta))
sociodemo_tab$SD = as.numeric(as.character(sociodemo_tab$SD))
sociodemo_tab$Model = (as.factor(sociodemo_tab$Model))
sociodemo_tab$Variable = factor(sociodemo_tab$Variable, levels =c("Age","Ethniciy","Income","Higher Education", "Social Visits"))

# Basic plot
Msociodemo_plot = ggplot(sociodemo_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: Brain Age ~ Sociodemographic Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMSociodemoPlot.pdf",Msociodemo_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMSociodemoPlot.pdf",Msociodemo_plot, height = 6, width = 9)

################ HEALTH MODEL
pred_names = c("Age","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee", "Smoking")
health = list()
for (i in 1:8){
  health[[i]] = cbind(stdCoef.merMod(Mmodel3[[i]])[2:14,1], stdCoef.merMod(Mmodel3[[i]])[2:14,2], pred_names, c(replicate(13, "health")))
} # to get the intercept as well: # health[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(length(pred_names),"FULL"), replicate(length(pred_names),"MEAN"), replicate(length(pred_names),"BRIA"), replicate(length(pred_names),"DKI"),
                    replicate(length(pred_names),"DTI"), replicate(length(pred_names),"SMT"), replicate(length(pred_names),"SMT_mc"), replicate(length(pred_names),"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% dplyr::rename("Beta" = "V1",
                                          "SD" = "V2",
                                          "Predictors" = "V4",
                                          "Variable" = "pred_names")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$SD = as.numeric(as.character(health_tab$SD))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","BMI","WHR","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol", "Smoking")))

# Basic bar plot
Mhealth_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: Brain Age ~ Health and Lifestyle Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45)) # + ylim(-1,8)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMHealthPlot.pdf",Mhealth_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMHealthPlot.pdf",Mhealth_plot, height = 6, width = 9)

################ WELLBEING MODEL
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(stdCoef.merMod(Mmodel4[[i]])[2:9,1], stdCoef.merMod(Mmodel4[[i]])[2:9,2], c("Age","Job Satisfaction", "Finance Satisfaction", "Self-rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction"), c(replicate(8, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$SD = as.numeric(as.character(wellbeing_tab$SD))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
wellbeing_tab = na.omit(wellbeing_tab)
# Basic bar plot
Mwellbeing_plot =ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: Brain Age ~ Life Satisfaction Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) #+ ylim(-0.75, 0.5)
#ggsave("/cluster/projects/p33/users/maxk/UKB/export/FrontiersMWellbeingPlot.pdf",MWellbeing_plot, height = 6, width = 9)
##ggsave("/tsd/p33/home/p33-maxk/export/FrontiersMWellbeingPlot.pdf",MWellbeing_plot, height = 6, width = 9)

################ COGNIION MODEL
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(stdCoef.merMod(Mmodel5[[i]])[2:9,1], stdCoef.merMod(Mmodel5[[i]])[2:9,2], c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered"), c(replicate(8, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Age"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(8,"FULL"), replicate(8,"MEAN"), replicate(8,"BRIA"), replicate(8,"DKI"),
                    replicate(8,"DTI"), replicate(8,"SMT"), replicate(8,"SMT_mc"), replicate(8,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$SD = as.numeric(as.character(cognition_tab$SD))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Matrix Puzzles Solved","Digits Remembered")))

# Basic bar plot
Mcognition_plot =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.2)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(title = "Males: Brain Age ~ Cognitive Factors",x="Predictors of BAG", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45)) #+ ylim(-0.75, 0.6)
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
ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/FrontiersSF3.pdf",plotall, height = 18, width = 12)

#####
#####
#####
#####
### POST HOC QUESTIONS ####
### MULTICOLLINEARITY CHECKS ####

############ CHECK CORRELATIONS

cog_mat = df %>% dplyr::select(tower_arranging, prospective_memory_t3, fluid_intelligence_t3, excl_sample.digit_sub_correct_t3, mean_inc_pair_matches, matrix_puzzles_solved_t3, digits_remembered_t3)
colnames(cog_mat) = c("Tower Arranging", "Prospective Memory", "Fluid Intelligence", "Digit Substitution", "Pair Matches", "Matrix Puzzles", "Digits Remembered")
res = cor(na.omit(cog_mat))
cors = as.ggplot(pheatmap(res,cluster_rows = F,cluster_cols = F, display_numbers = TRUE))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/Cognitive_Score_Correlations.pdf", cors,height = 6, width = 6)

soc_mat = df %>% dplyr::select(job_satisfaction,finance_satisfaction,health_satisfaction, overall_health_t3, excl_sample.family_rel_satisfaction_t3 , excl_sample.friend_rel_satisfaction_t3, happiness_t3)
colnames(soc_mat) = c("Job Satisfaction", "Financial Satisfaction", "Health Satisfaction", "Overall Health", "Family Relationship Satisfaction","Friend Relationship Satisfaction","Happiness")
res_soc = cor(na.omit(soc_mat))
cors_soc = as.ggplot(pheatmap(res_soc,cluster_rows = F,cluster_cols = F, display_numbers = TRUE))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/Satisfaction_Score_Correlations.pdf", cors_soc,height = 6, width = 6)
cors_soc
health_mat = df %>%  dplyr::select(BMI, WHR, pulse_pressure, birth_weight, sleep_hours, daily_coffee)
colnames(health_mat) = c("BMI", "WHR", "Pulse Pressure", "Birth Weight", "Sleep Hours", "Daily Coffee")
res_health = cor(na.omit(health_mat))
cors_health = as.ggplot(pheatmap(res_health,cluster_rows = F,cluster_cols = F, display_numbers = TRUE))
ggsave("/cluster/projects/p33/users/maxk/UKB/export/Health_Score_Correlations.pdf", cors_health,height = 6, width = 6)


################ CHECK THE VARIANCE INFLATION FACTOR

# make function that applies to mixed models (the regular vif() potentially underestimates VIF in mixed models)

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(MLM_cog)
vif.mer(MLM_health)
vif.mer(MLM_wellbeing)

# compare to regular vif() calc
vif(MLM_cog)

### MULTICOLLINEARITY SANITY CHECKS ####
#####
### RE-FIT NON-REDUCED MODELS #### 

## 1.1) rename brainage
BAG_MEAN = BAG_MEAN %>% dplyr::rename("brainage" = "pred_age_MEAN_no_SD_test")
BAG_BRIA= BAG_BRIA %>% dplyr::rename("brainage" = "pred_age_Bayes_no_SD_test")
BAG_DKI= BAG_DKI %>% dplyr::rename("brainage" = "pred_age_DKI_no_SD_test")
BAG_DTI= BAG_DTI %>% dplyr::rename("brainage" = "pred_age_DTI_no_SD_test")
BAG_SMT= BAG_SMT %>% dplyr::rename("brainage" = "pred_age_SMT_no_SD_test")
BAG_SMT_mc= BAG_SMT_mc %>% dplyr::rename("brainage" = "pred_age_SMT_mc_no_SD_test")
BAG_WMTI= BAG_WMTI %>% dplyr::rename("brainage" = "pred_age_WMTI_no_SD_test")
BAG= BAG %>% dplyr::rename("brainage" = "pred_age_dwMRI_test90_no_SD")

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
FULL = merge(df_new, BAG, by = "eid")
#FULL$age = df$age
#MEAN = MEAN %>% dplyr::rename("brainage_gap" = "brainage_gap_MEAN_no_SD_test")
#BRIA= BRIA %>% dplyr::rename("brainage_gap" = "brainage_gap_Bayes_no_SD_test")
#DKI= DKI %>% dplyr::rename("brainage_gap" = "brainage_gap_DKI_no_SD_test")
#DTI= DTI %>% dplyr::rename("brainage_gap" = "brainage_gap_DTI_no_SD_test")
#SMT= SMT %>% dplyr::rename("brainage_gap" = "brainage_gap_SMT_no_SD_test")
#SMT_mc= SMT_mc %>% dplyr::rename("brainage_gap" = "brainage_gap_SMT_mc_no_SD_test")
#WMTI= WMTI %>% dplyr::rename("brainage_gap" = "brainage_gap_WMTI_no_SD_test")

# create a list with all the data frames
brainage_list = list(FULL, MEAN, BRIA, DKI, DTI, SMT, SMT_mc, WMTI)

# loop over list for predictions

## ## ## NULL MODEL 0.1 ## ## ## 
model0.1 <- list()
for (i in 1:8) {model0.1[[i]] = lmer(brainage~ 1 + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.1_metrics[i,] = c(r.squaredGLMM(model0.1[[i]]),AIC(model0.1[[i]]),(sigma(model0.1[[i]]))^2,summary(model0.1[[i]])$logLik[1])} 

## ## ## NULL MODEL 0.2 ## ## ## 
model0.2 <- list()
for (i in 1:8) {model0.2[[i]] = lmer(brainage~ 1 + sex + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model0.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model0.2_metrics[i,] = c(r.squaredGLMM(model0.2[[i]]),AIC(model0.2[[i]]),(sigma(model0.2[[i]]))^2,summary(model0.2[[i]])$logLik[1])} 

## ## ## BASELINE MODEL 1 ## ## ## 
model1 <- list()
for (i in 1:8) {model1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model1_metrics[i,] = c(r.squaredGLMM(model1[[i]]),AIC(model1[[i]]),(sigma(model1[[i]]))^2,summary(model1[[i]])$logLik[1])} 

## ## ## SOCIODEMO MODEL 2 ## ## ## 
model2 <- list()
for (i in 1:8) {model2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3), data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model2_metrics[i,] = c(r.squaredGLMM(model2[[i]]),AIC(model2[[i]]),(sigma(model2[[i]]))^2,summary(model2[[i]])$logLik[1])} 

## ## ## HEALTH MODEL 3 ## ## ## 
model3 <- list()
for (i in 1:8) {model3[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                     high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model3_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3_metrics[i,] = c(r.squaredGLMM(model3[[i]]),AIC(model3[[i]]),(sigma(model3[[i]]))^2,summary(model3[[i]])$logLik[1])} 

## ## ## WELLBEING MODEL 4 ## ## ## 
model4 <- list()
for (i in 1:8) {model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4_metrics[i,] = c(r.squaredGLMM(model4[[i]]),AIC(model4[[i]]),(sigma(model4[[i]]))^2,summary(model4[[i]])$logLik[1])} 


## ## ## COGNITION MODEL 5 ## ## ## 
model5 <- list()
for (i in 1:8) {model5[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5_metrics[i,] = c(r.squaredGLMM(model5[[i]]),AIC(model5[[i]]),(sigma(model5[[i]]))^2,summary(model5[[i]])$logLik[1])} 


### RUN REDUCED MODELS ####
######### No WHR ####
# remove WHR to see which effect this has on BMI
model3.1 <- list()
for (i in 1:8) {model3.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + pulse_pressure + alcohol_drinker + diabetic +
                                       high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_list[[i]])}



# loop over output data to create a table with model metrics
model3.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3.1_metrics[i,] = c(r.squaredGLMM(model3.1[[i]]),AIC(model3.1[[i]]),(sigma(model3.1[[i]]))^2,summary(model3.1[[i]])$logLik[1])} 

# these metrics are very close to what has been there with WHR
model3_diff1 = model3.1_metrics - model3_metrics

for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model3[[i]]))])
model3.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=data)
model3[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=data)
} # re-fit based on model3 data

# get p-vals
health_p2 = list()
for (i in 1:8){health_p2[[i]] = cbind(summary(model3.1[[i]])$coefficients)} 
health_p2 = data.frame(do.call(rbind, health_p2))
names = rownames(summary(model3.1[[1]])$coefficients)
names1 = c(rep(names, 8))
health_p2 = data.frame(do.call(cbind, health_p2))
health_p2$names = names1
health_p2$stat_model = c(replicate((length(names1)/8),"Full"), replicate((length(names1)/8),"Mean"), replicate((length(names1)/8),"BRIA"),replicate((length(names1)/8),"DKI"), replicate((length(names1)/8),"DTI"), replicate((length(names1)/8),"SMT"), replicate((length(names1)/8),"SMTmc"), replicate((length(names1)/8),"WMTI"))
health_p2$corrected_p = health_p2$Pr...t.. * 8
health_p2 %>% filter(corrected_p < 0.05)%>% arrange(names)



# get model differences
model3_diffs1 = model3.1_metrics - model3_metrics


model3.1_anova = list()
for (i in 1:8){model3.1_anova[[i]] = anova(model3[[i]], model3.1[[i]])} # calculate differences

# make table
model3.1_table = model3.1_anova %>% bind_rows() %>% group_split()
model3.1_table = do.call(rbind.data.frame,model3.1_table)
model3.1_table = model3.1_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)


# PLOT
health = list()
for (i in 1:8){
  health[[i]] = cbind(stdCoef.merMod(model3.1[[i]])[2:15,1], stdCoef.merMod(model3.1[[i]])[2:15,2], c("Age","Sex","BMI","PP","Alcohol","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Birth Weight","Sleep","Coffee","Smoking","Age*Sex"), c(replicate(14, "health")))
} # to get the intercept as well: # health[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(14,"FULL"), replicate(14,"MEAN"), replicate(14,"BRIA"), replicate(14,"DKI"),
                    replicate(14,"DTI"), replicate(14,"SMT"), replicate(14,"SMT_mc"), replicate(14,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% dplyr::rename("Beta" = "X1",
                                          "SD" = "X2",
                                          "Variable" = "X3",
                                          "Predictors" = "X4")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$SD = as.numeric(as.character(health_tab$SD))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","Sex","Age*Sex","BMI","Birth Weight","PP","Diabetic","High Chol","Hypertension","Vascular Diagnosis","Sleep","Coffee","Alcohol", "Smoking")))

# Basic bar plot
no_WHR_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/SF5.pdf",no_WHR_plot, height = 6, width = 9)




#### NO Hypertension #####
# exclude hypertension, as it is just a derivate of pulse pressure
model3.2 <- list()
for (i in 1:8) {model3.2[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +
                                       high_cholesterol + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model3.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model3.2_metrics[i,] = c(r.squaredGLMM(model3.2[[i]]),AIC(model3.2[[i]]),(sigma(model3.2[[i]]))^2,summary(model3.2[[i]])$logLik[1])} 


################### MODEL DIFFERENCES (reduced vs full)


model3_diffs2 = model3.2_metrics - model3_metrics



#################### STATISTICALLY TESTING MODEL DIFFERENCES

for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model3[[i]]))])
model3.2[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=data)
model3[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking, data=data)
} # re-fit based on model3 data

model3.2_anova = list()
for (i in 1:8){model3.2_anova[[i]] = anova(model3[[i]], model3.2[[i]])} # calculate differences

# make table
model3.2_table = model3.2_anova %>% bind_rows() %>% group_split()
model3.2_table = do.call(rbind.data.frame,model3.2_table)
model3.2_table = model3.2_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)


# PLOT
health = list()
for (i in 1:8){
  health[[i]] = cbind(stdCoef.merMod(model3.2[[i]])[2:15,1], stdCoef.merMod(model3.2[[i]])[2:15,], c("Age","Sex","BMI","WHR","PP","Alcohol","Diabetic","High Chol","Vascular Diagnosis","Birth Weight","Sleep","Coffee","Smoking","Age*Sex"), c(replicate(14, "health")))
} # to get the intercept as well: # health[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "health")))

health_tab = data.frame(do.call(rbind, health))
dMRI_model_list = c(replicate(14,"FULL"), replicate(14,"MEAN"), replicate(14,"BRIA"), replicate(14,"DKI"),
                    replicate(14,"DTI"), replicate(14,"SMT"), replicate(14,"SMT_mc"), replicate(14,"WMTI"))
health_tab$Model = dMRI_model_list
health_tab = health_tab %>% dplyr::rename("Beta" = "stdcoef",
                                          "SD" = "stdse",
                                          "Variable" = "c..Age....Sex....BMI....WHR....PP....Alcohol....Diabetic....High.Chol...",
                                          "Predictors" = "c.replicate.14...health...")
levels(health_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
health_tab$Beta = as.numeric(as.character(health_tab$Beta))
health_tab$SD = as.numeric(as.character(health_tab$SD))
health_tab$Model = (as.factor(health_tab$Model))
health_tab$Variable = (factor(health_tab$Variable, levels= c("Age","Sex","Age*Sex","BMI","Birth Weight","WHR","PP","Diabetic","High Chol","Vascular Diagnosis","Sleep","Coffee","Alcohol", "Smoking")))

# Basic bar plot
no_hypertension_plot = ggplot(health_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/SF6.pdf",no_hypertension_plot, height = 6, width = 9)




######### Fluid Intel & Matrix Puzzles ####




################### CALCULATE MODEL
# full model
model5 <- list()
for (i in 1:8) {model5[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                     excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5_metrics[i,] = c(r.squaredGLMM(model5[[i]]),AIC(model5[[i]]),(sigma(model5[[i]]))^2,summary(model5[[i]])$logLik[1])} 

# reduced model
model5.1 <- list()
for (i in 1:8) {model5.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + 
                                       excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model5.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model5.1_metrics[i,] = c(r.squaredGLMM(model5.1[[i]]),AIC(model5.1[[i]]),(sigma(model5.1[[i]]))^2,summary(model5.1[[i]])$logLik[1])} 

################### MODEL DIFFERENCES (reduced vs full)

# these metrics are very close to what has been there with WHR
model5_diff1 = model5.1_metrics - model5_metrics

#################### STATISTICAL TESTING MODEL DIFFERENCES

for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model5[[i]]))])
model5.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + digits_remembered_t3, data=data)
model5[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3) + matrix_puzzles_solved_t3 + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + digits_remembered_t3, data=data)
} # re-fit based on model5 data

model5.1_anova = list()
for (i in 1:8){model5.1_anova[[i]] = anova(model5[[i]], model5.1[[i]])} # calculate differences

# make table
model5.1_table = model5.1_anova %>% bind_rows() %>% group_split()
model5.1_table = do.call(rbind.data.frame,model5.1_table)
model5.1_table = model5.1_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)



################ REDUCED COGNIION MODEL PLOT
cognition = list()
for (i in 1:8){
  cognition[[i]] = cbind(stdCoef.merMod(model5.1[[i]])[2:10,1], stdCoef.merMod(model5.1[[i]])[2:10,2], c("Age","Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Digits Remembered","Age*Sex"), c(replicate(9, "cognition")))
} # to get the intercept as well: # cognition[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "cognition")))
cognition_tab = data.frame(do.call(rbind, cognition))
dMRI_model_list = c(replicate(9,"FULL"), replicate(9,"MEAN"), replicate(9,"BRIA"), replicate(9,"DKI"),
                    replicate(9,"DTI"), replicate(9,"SMT"), replicate(9,"SMT_mc"), replicate(9,"WMTI"))
cognition_tab$Model = dMRI_model_list
cognition_tab = cognition_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(cognition_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
cognition_tab$Beta = as.numeric(as.character(cognition_tab$Beta))
cognition_tab$SD = as.numeric(as.character(cognition_tab$SD))
cognition_tab$Model = (as.factor(cognition_tab$Model))
cognition_tab$Variable = (factor(cognition_tab$Variable, levels = c("Age","Sex","Age*Sex","Tower Arranging", "Prospective Memory", "Fluid Intelligence","Symbol Digit Substitution","Mean Correct Pair Matches","Digits Remembered")))



# Basic bar plot
cognition_plot2 =ggplot(cognition_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/SF9.pdf",cognition_plot2, height = 6, width = 9)




######### Exclude Overall Health ####
# re-fit full model
model4 <- list()
for (i in 1:8) {model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4_metrics[i,] = c(r.squaredGLMM(model4[[i]]),AIC(model4[[i]]),(sigma(model4[[i]]))^2,summary(model4[[i]])$logLik[1])} 

# fit reduced model
model4.1 <- list()
for (i in 1:8) {model4.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4.1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4.1_metrics[i,] = c(r.squaredGLMM(model4.1[[i]]),AIC(model4.1[[i]]),(sigma(model4.1[[i]]))^2,summary(model4.1[[i]])$logLik[1])} 



########## CALC MODEL DIFFERENCES

##### MODEL DIFFERENCES (reduced vs full)

# these metrics are very close to what has been there with WHR
model4_diff1 = model4.1_metrics - model4_metrics

#####STATISTICAL TESTING MODEL DIFFERENCES
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model4[[i]]))])
model4.1[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=data)
model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 +overall_health_t3, data=data)
}
model4.1_anova = list()
for (i in 1:8){model4.1_anova[[i]] = anova(model4[[i]], model4.1[[i]])} # calculate differences

# make table
model4.1_table = model4.1_anova %>% bind_rows() %>% group_split()
model4.1_table = do.call(rbind.data.frame,model4.1_table)
model4.1_table = model4.1_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)


################ PLOT
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(stdCoef.merMod(model4.1[[i]])[2:10,1], stdCoef.merMod(model4.1[[i]])[2:10,2], c("Age","Sex","Job Satisfaction", "Finance Satisfaction", "Health Satisfaction", "Family Relationships Satisfaction", "Happiness", "Friend Relationship Satisfaction","Age*Sex"), c(replicate(9, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(9,"FULL"), replicate(9,"MEAN"), replicate(9,"BRIA"), replicate(9,"DKI"),
                    replicate(9,"DTI"), replicate(9,"SMT"), replicate(9,"SMT_mc"), replicate(9,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
wellbeing_tab = wellbeing_tab %>% dplyr::rename("Beta" = "X1",
                                                "SD" = "X2",
                                                "Variable" = "X3",
                                                "Predictors" = "X4")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$SD = as.numeric(as.character(wellbeing_tab$SD))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Sex","Age*Sex","Job Satisfaction", "Finance Satisfaction", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Happiness")))
#wellbeing_tab = na.omit(wellbeing_tab)
#  plot
no_health_sat = ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/Frontiers_export/SF7.pdf",no_health_sat, height = 6, width = 9)



######### Exclude Happiness ####
# Happiness is highly correlated with Family Relation Satisfaction and Job Satisfaction

#full model
## ## ## WELLBEING MODEL 4 ## ## ## 
model4 <- list()
for (i in 1:8) {model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                     excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}

# reduced model
model4.2 <- list()
for (i in 1:8) {model4.2[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +
                                       excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + excl_sample.friend_rel_satisfaction_t3 , data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model4.2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model4.2_metrics[i,] = c(r.squaredGLMM(model4.2[[i]]),AIC(model4.2[[i]]),(sigma(model4.2[[i]]))^2,summary(model4.2[[i]])$logLik[1])} 




########## CALC MODEL DIFFERENCES

##### MODEL DIFFERENCES (reduced vs full)

# these metrics are very close to what has been there with WHR
model4_diff2 = model4.2_metrics - model4_metrics

#####STATISTICALLY TESTING MODEL DIFFERENCES

for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model4[[i]]))])
model4.2[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3+ overall_health_t3+excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 +  + excl_sample.friend_rel_satisfaction_t3, data=data)
model4[[i]] = lmer(brainage ~ 1 + age + sex + age:sex + (1|site_t3)+ job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3, data=data)}

model4.2_anova = list()
for (i in 1:8){model4.2_anova[[i]] = anova(model4[[i]], model4.2[[i]])} # calculate differences

# make table
model4.2_table = model4.2_anova %>% bind_rows() %>% group_split()
model4.2_table = do.call(rbind.data.frame,model4.2_table)
model4.2_table = model4.2_table%>% select(-npar, -Df) %>% mutate(Model = mod_nam)


################ PLOT
wellbeing = list()
for (i in 1:8){
  wellbeing[[i]] = cbind(stdCoef.merMod(model4.2[[i]])[2:10,1], stdCoef.merMod(model4.2[[i]])[2:10,], c("Age","Sex","Job Satisfaction", "Finance Satisfaction", "Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction","Age*Sex"), c(replicate(9, "wellbeing")))
} # to get the intercept as well: # wellbeing[[i]] = cbind(stdCoef.merMod(model1[[i]])[,1], confint(model1[[i]])[3:6,], c("Intercept","Sex","Age","Age*Sex"), c(replicate(4, "wellbeing")))
wellbeing_tab = data.frame(do.call(rbind, wellbeing))
dMRI_model_list = c(replicate(9,"FULL"), replicate(9,"MEAN"), replicate(9,"BRIA"), replicate(9,"DKI"),
                    replicate(9,"DTI"), replicate(9,"SMT"), replicate(9,"SMT_mc"), replicate(9,"WMTI"))
wellbeing_tab$Model = dMRI_model_list
names(wellbeing_tab) = c("0", "Beta", "SD", "Variable", "Predictors", "Model")
levels(wellbeing_tab$Model)= c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
wellbeing_tab$Beta = as.numeric(as.character(wellbeing_tab$Beta))
wellbeing_tab$SD = as.numeric(as.character(wellbeing_tab$SD))
wellbeing_tab$Model = (as.factor(wellbeing_tab$Model))
wellbeing_tab$Variable = (factor(wellbeing_tab$Variable, levels = c("Age","Sex","Age*Sex","Job Satisfaction", "Finance Satisfaction","Self-Rated Health", "Health Satisfaction", "Family Relationships Satisfaction", "Friend Relationship Satisfaction")))
wellbeing_tab = na.omit(wellbeing_tab)

# Basic plot
no_happiness = ggplot(wellbeing_tab, aes(x = Variable, y = Beta, group = Model)) + 
  geom_point(position = position_dodge(width = 0.4), aes(color= Model), size = 0.8)+
  geom_errorbar(aes(ymin=Beta-SD, ymax=Beta+SD, y=Beta, x = (Variable), color = Model), position = position_dodge(width = 0.4), width = 0.1)+
  labs(x="Predictors of Brain Age", y = "Standardized Beta ± SD")+
  #guides(color = guide_legend(reverse = FALSE))+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45))
ggsave("/cluster/projects/p33/users/maxk/UKB/export2/SF8.pdf",no_happiness, height = 6, width = 9)






######### MAKE A LARGE MODEL COMPARISON TABLE *AND* A TABLE SHOWING DIFFERENCES IN MODEL METRICS ########
# model comparison table
ST8 = rbind(model3.1_table,model3.2_table,
            model4.1_table, model4.2_table,
            model5.1_table)
ST8$Reduction = c(replicate(8, c("Full Model", "WHR Reduced")), replicate(8, c("Full Model","Hypertension Reduced")),replicate(8, c("Full Model","Self-Rated Health Reduced")),replicate(8,c("Full Model", "Happiness Reduced")), replicate(8, c("Full Model", "Matrix Puzzles Reduced")))
ST8 = ST8 %>% dplyr::rename("Diffusion_Model" = "Model",
                            "Statistical_Model" = "Reduction")
write_csv(ST8, "/cluster/projects/p33/users/maxk/UKB/export2/ST8.csv")

# differences in metrics
output = rbind(model3_diffs1,model3_diffs2,
               model4_diff1, model4_diff2,
               model5_diff1)
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
output$Diffusion_Model = c(replicate(5, dMRI_model))
output = output %>% dplyr::rename("R2M"="X1",
                                  "R2C" = "X2",
                                  "AIC" = "X3",
                                  "sig2" = "X4",
                                  "logLik" = "X5")
output$Statistical_Model = c(replicate(8, "WHR Reduced"), replicate(8, "Hypertension Reduced"),replicate(8, "Self-Rated Health Reduced"),replicate(8, "Happiness Reduced"), replicate(8, "Matrix Puzzles Reduced"))

write.csv(output, "/cluster/projects/p33/users/maxk/UKB/export2/ST7.csv")

cor(df$pred_age_dwMRI_test90_no_SD, df$age, use = "complete.obs")

#####
#####
### RUNNING A PCA AS PROOF OF CONCEPT ####

# make data frames 
cog_vars = df %>% dplyr::select(eid,sex,Assessment_centre, tower_arranging, prospective_memory_t3, fluid_intelligence_t3, excl_sample.digit_sub_correct_t3, mean_inc_pair_matches, matrix_puzzles_solved_t3, digits_remembered_t3) %>% na.omit()
sat_vars = df %>% dplyr::select(eid,sex,Assessment_centre, job_satisfaction_t3,excl_sample.finance_satisfaction_t3,excl_sample.health_satisfaction_t3, overall_health_t3, excl_sample.family_rel_satisfaction_t3 , excl_sample.friend_rel_satisfaction_t3, happiness_t3) %>% na.omit()
bio_vars = df %>%  dplyr::select(eid,sex,Assessment_centre, BMI, WHR, pulse_pressure, birth_weight_t1, sleep_hours_t3, daily_coffee_t3)%>% na.omit()
cog_pca = cog_vars %>% dplyr::select(-eid,-sex,-Assessment_centre)
sat_pca = sat_vars %>% dplyr::select(-eid,-sex,-Assessment_centre)
bio_pca = bio_vars %>% dplyr::select(-eid,-sex,-Assessment_centre)

# run PCA and check outcomes, i.e. do the components explain sufficient variance?
Cog_PCA = princomp(cog_pca, cor = TRUE, scores = TRUE)
summary(Cog_PCA)
Sat_PCA = princomp(sat_pca, cor = TRUE, scores = TRUE)
summary(Sat_PCA)
Bio_PCA = princomp(bio_pca, cor = TRUE, scores = TRUE)
summary(Bio_PCA)

# throw components and brain age together

## 1.) write component scores into reduced data frames (due to missingness)
cog_vars$CogPC = Cog_PCA$scores[,1]
sat_vars$SatPC = Sat_PCA$scores[,1]
bio_vars$BioPC = Bio_PCA$scores[,1]
cog_vars = cog_vars %>% dplyr::select(eid, sex, Assessment_centre, CogPC)
sat_vars = sat_vars %>% dplyr::select(eid, sex, Assessment_centre, SatPC)
bio_vars = bio_vars %>% dplyr::select(eid, sex, Assessment_centre, BioPC)

## 1.1) rename brainage
BAG = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_dwMRI_test90_no_SD.csv")
BAG_BRIA = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_Bayes_no_SD_test.csv")
BAG_DKI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_DKI_no_SD_test.csv")
BAG_MEAN = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_MEAN_no_SD_test.csv")
BAG_DTI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_DTI_no_SD_test.csv")
BAG_SMT = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_SMT_no_SD_test.csv")
BAG_SMT_mc = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_SMT_mc_no_SD_test.csv")
BAG_WMTI = read.csv("/cluster/projects/p33/users/maxk/UKB/brainage_MK/datafiles/no_SD/Brainage_WMTI_no_SD_test.csv")

BAG_MEAN = BAG_MEAN %>% dplyr::rename("brainage" = "pred_age_MEAN_no_SD_test")
BAG_BRIA= BAG_BRIA %>% dplyr::rename("brainage" = "pred_age_Bayes_no_SD_test")
BAG_DKI= BAG_DKI %>% dplyr::rename("brainage" = "pred_age_DKI_no_SD_test")
BAG_DTI= BAG_DTI %>% dplyr::rename("brainage" = "pred_age_DTI_no_SD_test")
BAG_SMT= BAG_SMT %>% dplyr::rename("brainage" = "pred_age_SMT_no_SD_test")
BAG_SMT_mc= BAG_SMT_mc %>% dplyr::rename("brainage" = "pred_age_SMT_mc_no_SD_test")
BAG_WMTI= BAG_WMTI %>% dplyr::rename("brainage" = "pred_age_WMTI_no_SD_test")
BAG= BAG %>% dplyr::rename("brainage" = "pred_age_dwMRI_test90_no_SD")

## 2.) merge the data frames with each brain age frame
### 2.1) with cog_vars
MEANc = merge(cog_vars, BAG_MEAN, by = "eid")
BRIAc = merge(cog_vars, BAG_BRIA, by = "eid")
DKIc = merge(cog_vars, BAG_DKI, by = "eid")
DTIc = merge(cog_vars, BAG_DTI, by = "eid")
SMTc = merge(cog_vars, BAG_SMT, by = "eid")
SMT_mcc = merge(cog_vars, BAG_SMT_mc, by = "eid")
WMTIc = merge(cog_vars, BAG_WMTI, by = "eid")
FULLc = merge(cog_vars, BAG, by = "eid")
### 2.2) with bio_vars
MEANb = merge(bio_vars, BAG_MEAN, by = "eid")
BRIAb = merge(bio_vars, BAG_BRIA, by = "eid")
DKIb = merge(bio_vars, BAG_DKI, by = "eid")
DTIb = merge(bio_vars, BAG_DTI, by = "eid")
SMTb = merge(bio_vars, BAG_SMT, by = "eid")
SMT_mcb = merge(bio_vars, BAG_SMT_mc, by = "eid")
WMTIb = merge(bio_vars, BAG_WMTI, by = "eid")
FULLb = merge(bio_vars, BAG, by = "eid")
### 2.3) with sat_vars
MEANs = merge(sat_vars, BAG_MEAN, by = "eid")
BRIAs = merge(sat_vars, BAG_BRIA, by = "eid")
DKIs = merge(sat_vars, BAG_DKI, by = "eid")
DTIs = merge(sat_vars, BAG_DTI, by = "eid")
SMTs = merge(sat_vars, BAG_SMT, by = "eid")
SMT_mcs = merge(sat_vars, BAG_SMT_mc, by = "eid")
WMTIs = merge(sat_vars, BAG_WMTI, by = "eid")
FULLs = merge(sat_vars, BAG, by = "eid")

# 3.) create a list with all the data frames
bio_list = list(FULLb, MEANb, BRIAb, DKIb, DTIb, SMTb, SMT_mcb, WMTIb)
cog_list = list(FULLc, MEANc, BRIAc, DKIc, DTIc, SMTc, SMT_mcc, WMTIc)
sat_list = list(FULLs, MEANs, BRIAs, DKIs, DTIs, SMTs, SMT_mcs, WMTIs)

# 4.) Run Models on the lists
## 4.1) bio_list
biomodel1 <- list()
for (i in 1:8) {biomodel1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|Assessment_centre), data=bio_list[[i]])}
biomodel2 <- list()
for (i in 1:8) {biomodel2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + BioPC + (1|Assessment_centre), data=bio_list[[i]])}
## 4.2) cog_list
cogmodel1 <- list()
for (i in 1:8) {cogmodel1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|Assessment_centre), data=cog_list[[i]])}
cogmodel2 <- list()
for (i in 1:8) {cogmodel2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + CogPC + (1|Assessment_centre), data=cog_list[[i]])}
## 4.3) sat_list
satmodel1 <- list()
for (i in 1:8) {satmodel1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|Assessment_centre), data=sat_list[[i]])}
satmodel2 <- list()
for (i in 1:8) {satmodel2[[i]] = lmer(brainage~ 1 + age + sex + age:sex + SatPC + (1|Assessment_centre), data=sat_list[[i]])}

# 5) Get model metrics
## 5.1) bio_list
biomodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {biomodel1_metrics[i,] = c(r.squaredGLMM(biomodel1[[i]]),AIC(biomodel1[[i]]),(sigma(biomodel1[[i]]))^2,summary(biomodel1[[i]])$logLik[1])} 
biomodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {biomodel2_metrics[i,] = c(r.squaredGLMM(biomodel2[[i]]),AIC(biomodel2[[i]]),(sigma(biomodel2[[i]]))^2,summary(biomodel2[[i]])$logLik[1])} 
## 4.2) cog_list
cogmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {cogmodel1_metrics[i,] = c(r.squaredGLMM(cogmodel1[[i]]),AIC(cogmodel1[[i]]),(sigma(cogmodel1[[i]]))^2,summary(cogmodel1[[i]])$logLik[1])} 
cogmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {cogmodel2_metrics[i,] = c(r.squaredGLMM(cogmodel2[[i]]),AIC(cogmodel2[[i]]),(sigma(cogmodel2[[i]]))^2,summary(cogmodel2[[i]])$logLik[1])} 
## 5.3) sat_list
satmodel1_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {satmodel1_metrics[i,] = c(r.squaredGLMM(satmodel1[[i]]),AIC(satmodel1[[i]]),(sigma(satmodel1[[i]]))^2,summary(satmodel1[[i]])$logLik[1])} 
satmodel2_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {satmodel2_metrics[i,] = c(r.squaredGLMM(satmodel2[[i]]),AIC(satmodel2[[i]]),(sigma(satmodel2[[i]]))^2,summary(satmodel2[[i]])$logLik[1])} 

# 5) Write model metrics
output = rbind(biomodel1_metrics, biomodel2_metrics, cogmodel1_metrics,cogmodel2_metrics,satmodel1_metrics,satmodel2_metrics)
output$Regression_Model = c(replicate(8,"Baseline Health Model"), replicate(8,"Principal Health Component"),replicate(8,"Baseline Cognitive Model"), replicate(8,"Principal Cognitive Component"),replicate(8,"Baseline Satisfaction Model"), replicate(8,"Principal Satisfaction Component"))
dMRI_model = c("FULL", "MEAN", "BRIA", "DKI", "DTI", "SMT", "SMT_mc", "WMTI")
output$Diffusion_Model = c(replicate(6, dMRI_model))
output = output %>% dplyr::rename("R2M"="X1",
                                  "R2C" = "X2",
                                  "AIC" = "X3",
                                  "sig2" = "X4",
                                  "logLik" = "X5")

write.csv(output, "/cluster/projects/p33/users/maxk/UKB/export/PCA_model_metrics.csv")

# 6) Compare models
biomodel_comp = list()
satmodel_comp = list()
cogmodel_comp = list()
for (i in 1:8){
  biomodel_comp[[i]] = anova(biomodel1[[i]], biomodel2[[i]])
  satmodel_comp[[i]] = anova(satmodel1[[i]], satmodel2[[i]])
  cogmodel_comp[[i]] = anova(cogmodel1[[i]], cogmodel2[[i]])
}
# 6.1) make table
## starting with the names
mod_nam = c(replicate(2,"FULL"),replicate(2,"MEAN"), replicate(2,"BRIA"),replicate(2,"DKI"),replicate(2,"DTI"),replicate(2,"SMT"),replicate(2,"SMT_mc"),replicate(2,"WMTI"))
# 6.1.1) biomodel
biotable = biomodel_comp %>% bind_rows() %>% group_split()
biotable = do.call(rbind.data.frame,biotable)
biotable = biotable%>% select(-npar, -Df) %>% mutate(Regression_Model = replicate(16,"Health and Lifestyle Factors"),Diffusion_Model = mod_nam)
# 6.1.1) cogmodel
cogtable = cogmodel_comp %>% bind_rows() %>% group_split()
cogtable = do.call(rbind.data.frame,cogtable)
cogtable = cogtable%>% select(-npar, -Df) %>% mutate(Regression_Model = replicate(16,"Cognitive Factors"),Diffusion_Model = mod_nam)
# 6.1.1) satmodel
sattable = satmodel_comp %>% bind_rows() %>% group_split()
sattable = do.call(rbind.data.frame,sattable)
sattable = sattable%>% select(-npar, -Df) %>% mutate(Regression_Model = replicate(16,"Life Satisfaction Factors"),Diffusion_Model = mod_nam)

PCA_model_comp = rbind(biotable, cogtable, sattable)
write.csv(PCA_model_comp, "/cluster/projects/p33/users/maxk/UKB/export2/PCA_model_comp.csv")

#####
#####
### CHECK DESCRIPTIVES OF ETHNICITY ####
## 2) get Brain age vals for each group and relate them with bio-psycho-social factors ###### 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
df$brainage = df$pred_age_dwMRI_test90_no_SD
levels(df$Ethnicity) = c("European", "nonEuropean")
#### starting with ethnicity
df %>% group_by(Ethnicity) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))

# brain age
df %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(brainage), SD = sd(brainage), N = length(brainage))
df %>% wilcox_test(brainage ~ Ethnicity)
df %>% wilcox_effsize(brainage ~ Ethnicity)

# scanner site
df %>% group_by(Assessment_centre, Ethnicity) %>% dplyr::summarize(N = length(age))
chisq.test(df$Ethnicity, df$Assessment_centre)

# sex
df %>% group_by(sex, Ethnicity) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$sex)

# age
df %>% drop_na(age) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(age), SD = sd(age), N = length(age))
df %>% wilcox_test(age ~ Ethnicity)
df %>% wilcox_effsize(age ~ Ethnicity)

# income
t1 = df %>% group_by(Ethnicity, t3_income) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$t3_income)

# higher ed
df %>% group_by(Ethnicity, higher_education_t3) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$higher_education_t3)

# matrix puzzles
df %>% drop_na(matrix_puzzles_solved_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(matrix_puzzles_solved_t3), SD = sd(matrix_puzzles_solved_t3), N = length(matrix_puzzles_solved_t3))
df %>% wilcox_test(matrix_puzzles_solved_t3 ~ Ethnicity)
df %>% wilcox_effsize(matrix_puzzles_solved_t3 ~ Ethnicity)

# tower arranging
df %>% drop_na(tower_arranging) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(tower_arranging), SD = sd(tower_arranging), N = length(tower_arranging))
df %>% wilcox_test(tower_arranging ~ Ethnicity)
df %>% wilcox_effsize(tower_arranging ~ Ethnicity)

# prospective memory
df %>% drop_na(prospective_memory_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(prospective_memory_t3), SD = sd(prospective_memory_t3), N = length(prospective_memory_t3))
df %>% wilcox_test(prospective_memory_t3 ~ Ethnicity)
df %>% wilcox_effsize(prospective_memory_t3 ~ Ethnicity)

# fluid intel
df %>% drop_na(fluid_intelligence_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(fluid_intelligence_t3), SD = sd(fluid_intelligence_t3), N = length(fluid_intelligence_t3))
df %>% wilcox_test(fluid_intelligence_t3 ~ Ethnicity)
df %>% wilcox_effsize(fluid_intelligence_t3 ~ Ethnicity)

# digits remembered
df %>% drop_na(digits_remembered_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(digits_remembered_t3), SD = sd(digits_remembered_t3), N = length(digits_remembered_t3))
df %>% wilcox_test(digits_remembered_t3 ~ Ethnicity)
df %>% wilcox_effsize(digits_remembered_t3 ~ Ethnicity)

# mean pair matches
df %>% drop_na(mean_inc_pair_matches) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(mean_inc_pair_matches), SD = sd(mean_inc_pair_matches), N = length(mean_inc_pair_matches))
df %>% wilcox_test(mean_inc_pair_matches ~ Ethnicity)
df %>% wilcox_effsize(mean_inc_pair_matches ~ Ethnicity)

# job sat
df %>% drop_na(job_satisfaction_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(job_satisfaction_t3), SD = sd(job_satisfaction_t3), N = length(job_satisfaction_t3))
df %>% wilcox_test(job_satisfaction_t3 ~ Ethnicity)
df %>% wilcox_effsize(job_satisfaction_t3 ~ Ethnicity)

# financial sat
df %>% drop_na(excl_sample.finance_satisfaction_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(excl_sample.finance_satisfaction_t3), SD = sd(excl_sample.finance_satisfaction_t3), N = length(excl_sample.finance_satisfaction_t3))
df %>% wilcox_test(excl_sample.finance_satisfaction_t3 ~ Ethnicity)
df %>% wilcox_effsize(excl_sample.finance_satisfaction_t3 ~ Ethnicity)

# health sat
df %>% drop_na(excl_sample.health_satisfaction_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(excl_sample.health_satisfaction_t3), SD = sd(excl_sample.health_satisfaction_t3), N = length(excl_sample.health_satisfaction_t3))
df %>% wilcox_test(excl_sample.health_satisfaction_t3 ~ Ethnicity)
df %>% wilcox_effsize(excl_sample.health_satisfaction_t3 ~ Ethnicity)

# overall health
df %>% drop_na(overall_health_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(overall_health_t3), SD = sd(overall_health_t3), N = length(overall_health_t3))
df %>% wilcox_test(overall_health_t3 ~ Ethnicity)
df %>% wilcox_effsize(overall_health_t3 ~ Ethnicity)

# family relationships
df %>% drop_na(excl_sample.family_rel_satisfaction_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(excl_sample.family_rel_satisfaction_t3), SD = sd(excl_sample.family_rel_satisfaction_t3), N = length(excl_sample.family_rel_satisfaction_t3))
df %>% wilcox_test(excl_sample.family_rel_satisfaction_t3 ~ Ethnicity)
df %>% wilcox_effsize(excl_sample.family_rel_satisfaction_t3 ~ Ethnicity)

# friend relationships
df %>% drop_na(excl_sample.friend_rel_satisfaction_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(excl_sample.friend_rel_satisfaction_t3), SD = sd(excl_sample.friend_rel_satisfaction_t3), N = length(excl_sample.friend_rel_satisfaction_t3))
df %>% wilcox_test(excl_sample.friend_rel_satisfaction_t3 ~ Ethnicity)
df %>% wilcox_effsize(excl_sample.friend_rel_satisfaction_t3 ~ Ethnicity)

# happiness
df %>% drop_na(happiness_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(happiness_t3), SD = sd(happiness_t3), N = length(happiness_t3))
df %>% wilcox_test(happiness_t3 ~ Ethnicity)
df %>% wilcox_effsize(happiness_t3 ~ Ethnicity)

# BMI
df %>% drop_na(BMI) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(BMI), SD = sd(BMI), N = length(BMI))
df %>% wilcox_test(BMI ~ Ethnicity)
df %>% wilcox_effsize(BMI ~ Ethnicity)

# pulse pressure
df %>% drop_na(pulse_pressure) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(pulse_pressure), SD = sd(pulse_pressure), N = length(pulse_pressure))
df %>% wilcox_test(pulse_pressure ~ Ethnicity)
df %>% wilcox_effsize(pulse_pressure ~ Ethnicity)

# WHR
df %>% drop_na(WHR) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(WHR), SD = sd(WHR), N = length(WHR))
df %>% wilcox_test(WHR ~ Ethnicity)
df %>% wilcox_effsize(WHR ~ Ethnicity)

# smoking
df %>% group_by(Ethnicity, smoking) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$smoking)

# diabetes
df %>% group_by(Ethnicity, diabetic) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$diabetic)

# hypertension
df %>% group_by(Ethnicity, hypertension) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$hypertension)

# high cholesterol
df %>% group_by(Ethnicity, high_cholesterol) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$high_cholesterol)

# vascular diagnosis
df %>% group_by(Ethnicity, diagnosed_vascular_problem) %>% dplyr::summarize(N = length(age), Perc = length(age)/nrow(df))
chisq.test(df$Ethnicity, df$diagnosed_vascular_problem)

# birth weight
df %>% drop_na(birth_weight_t1) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(birth_weight_t1), SD = sd(birth_weight_t1), N = length(birth_weight_t1))
df %>% wilcox_test(birth_weight_t1 ~ Ethnicity)
df %>% wilcox_effsize(birth_weight_t1 ~ Ethnicity)

# daily coffee
df %>% drop_na(daily_coffee_t3) %>% group_by(Ethnicity) %>% dplyr::summarize(Mean = mean(daily_coffee_t3), SD = sd(daily_coffee_t3), N = length(daily_coffee_t3))
df %>% wilcox_test(daily_coffee_t3 ~ Ethnicity)
df %>% wilcox_effsize(daily_coffee_t3 ~ Ethnicity)


#####
#####
### RUNNING ALL VARIABLES IN ONE MODEL ####
model_sat <- list()
for (i in 1:8) {model_sat[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3) + BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking + job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=brainage_list[[i]])}
# loop over output data to create a table with model metrics
model_sat_metrics = data.frame(matrix(nrow = 8, ncol = 5))
for (i in 1:8) {model_sat_metrics[i,] = c(r.squaredGLMM(model_sat[[i]]),AIC(model_sat[[i]]),(sigma(model_sat[[i]]))^2,summary(model_sat[[i]])$logLik[1])} 

# looking at the difference to the baseline model
diff_sat = rbind(model_sat_metrics) - rbind(model1_metrics)
mean(diff_sat$X1)

diff_sat = diff_sat %>% dplyr::rename("R2M"="X1",
                                      "R2C" = "X2",
                                      "AIC" = "X3",
                                      "sig2" = "X4",
                                      "logLik" = "X5")
write.csv(diff_sat, "/cluster/projects/p33/users/maxk/UKB/export2/ST11.csv")

# which is statistically significant
## refit
for (i in 1:8) {data=na.omit(brainage_list[[i]][ , all.vars(formula(model_sat[[i]]))])
model1[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3), data=data)
model_sat[[i]] = lmer(brainage~ 1 + age + sex + age:sex + (1|site_t3)+ Ethnicity + higher_education_t3 + as.numeric(t3_income)+ as.numeric(social_visits_t3) + BMI + WHR + pulse_pressure + alcohol_drinker + diabetic +high_cholesterol + hypertension + diagnosed_vascular_problem + birth_weight_t1 + sleep_hours_t3 + daily_coffee_t3 + smoking + job_satisfaction_t3 + excl_sample.finance_satisfaction_t3 + overall_health_t3 +excl_sample.health_satisfaction_t3 + excl_sample.family_rel_satisfaction_t3 + happiness_t3 + excl_sample.friend_rel_satisfaction_t3 + tower_arranging + prospective_memory_t3 + fluid_intelligence_t3 + excl_sample.digit_sub_correct_t3 + mean_inc_pair_matches + matrix_puzzles_solved_t3 + digits_remembered_t3, data=data)}

## run model comp
model_sat_anova = list()
for (i in 1:8){model_sat_anova[[i]] = anova(model_sat[[i]], model1[[i]])} # calculate differences

mod_nam = c(replicate(2,"FULL"),replicate(2,"MEAN"), replicate(2,"BRIA"),replicate(2,"DKI"),replicate(2,"DTI"),replicate(2,"SMT"),replicate(2,"SMT_mc"),replicate(2,"WMTI"))
# make table
sattable = model_sat_anova %>% bind_rows() %>% group_split()
sattable = do.call(rbind.data.frame,sattable)
sattable = sattable%>% select(-npar, -Df) %>% mutate(Regression_Model = replicate(16,"Health and Lifestyle Factors"),Diffusion_Model = mod_nam)

write.csv(sattable, "/cluster/projects/p33/users/maxk/UKB/export2/ST12.csv")
