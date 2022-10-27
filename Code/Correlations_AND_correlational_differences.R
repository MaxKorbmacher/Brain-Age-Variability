# BAG and bio-psycho-social factor correlations and correlational differences
## ALSO: a visualisation of the effect sizes for group differences
# BAG was calculated from diffusion data using six diffusion approaches: BRIA, DKI, DTI, SMT, mcSMT, WMTI
# code by Max Korbmacher, 07.10.2022

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
#install.packages("MuMIn")
library(MuMIn)
library(rstatix)
#install.packages("sjPlot")
library(sjPlot)
library(ggplotify)
library(pheatmap)
library(rstatix)
library(cocor)

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

# remove data which will not be correlated
for (i in 1:length(brainage_list)){
  brainage_list[[i]] = brainage_list[[i]] %>% select(brainage_gap, age, t3_income, matrix_puzzles_solved_t3, tower_arranging, 
                prospective_memory_t3,fluid_intelligence_t3,digits_remembered_t3, mean_inc_pair_matches,
                job_satisfaction_t3,excl_sample.finance_satisfaction_t3,excl_sample.health_satisfaction_t3,
                overall_health_t3, excl_sample.family_rel_satisfaction_t3, excl_sample.friend_rel_satisfaction_t3,
                happiness_t3,BMI, pulse_pressure,WHR,birth_weight_t1, daily_coffee_t3)
  brainage_list[[i]]$t3_income = as.numeric(brainage_list[[i]]$t3_income)
  brainage_list[[i]]$job_satisfaction_t3 = as.numeric(brainage_list[[i]]$job_satisfaction_t3)
  brainage_list[[i]]$excl_sample.finance_satisfaction_t3 = as.numeric(brainage_list[[i]]$excl_sample.finance_satisfaction_t3)
  brainage_list[[i]]$excl_sample.health_satisfaction_t3 = as.numeric(brainage_list[[i]]$excl_sample.health_satisfaction_t3)
  brainage_list[[i]]$overall_health_t3 = as.numeric(brainage_list[[i]]$overall_health_t3)
  brainage_list[[i]]$excl_sample.family_rel_satisfaction_t3 = as.numeric(brainage_list[[i]]$excl_sample.family_rel_satisfaction_t3)
  brainage_list[[i]]$excl_sample.friend_rel_satisfaction_t3 = as.numeric(brainage_list[[i]]$excl_sample.friend_rel_satisfaction_t3)
  brainage_list[[i]]$happiness_t3 = as.numeric(brainage_list[[i]]$happiness_t3)
}
library(RColorBrewer)
### ### ### ### ### ### ### ###
#### CORRELATIONS IN A PLOT #####
### ### ### ### ### ### ### ###
bagmat = list()
for (i in 1:length(brainage_list)){
  bagmat[[i]] = cor(brainage_list[[i]][1], brainage_list[[i]][2:ncol(brainage_list[[i]])], use = "complete.obs")
}
bagtab = data.frame(do.call(rbind, bagmat))
rownames(bagtab) = c("Full Multimodal", "Mean Multimodal", "BRIA", "DKI", "DTI", "SMT", "SMT mc", "WMTI")
colnames(bagtab) = c("Age", "Income", "Matrix Puzzles", "Tower Arranging", "Prospective Memory", "Fluid Intelligence", 
  "Digits Remembered", "Pair Matches", "Job Satisfaction", "Finance Satisfaction", "Health Satisfaction", "Self-Rated Health",
  "Family Relationship Satisfaction", "Friend Relationship Satisfaction", "Happiness", "BMI", "Pulse Pressure","WHR", "Birth Weight", "Daily Coffee")
# fix the order of the plot by the mean correlations
cororder = apply(bagtab, 2, mean) %>% sort() %>% names()
bagtab = subset(bagtab, select = cororder)
breaksList = seq(-.7,.3,by = 0.05)
bagplot = as.ggplot(pheatmap(bagtab,cluster_rows = F,cluster_cols = F, display_numbers = TRUE, 
                             color = colorRampPalette(rev(brewer.pal(n = 7, name = "YlGnBu")))(length(breaksList)), breaks = breaksList))

# save the correlation plot
#ggsave("/tsd/p33/home/p33-maxk/export/BAG_Correlations.pdf",bagplot, height = 6, width = 9)
ggsave("/cluster/projects/p33/users/maxk/UKB/export/Frontiers_Fig7.pdf",bagplot, height = 6, width = 9)

# check for significance
bag_p = data.frame(matrix(nrow = 21, ncol = 8))
for (i in 1:length(brainage_list)){
  bag_p[,i] = c(cor_test(brainage_list[[i]], use = "complete.obs") %>% filter(var1 == "brainage_gap") %>% select("p"))
}
bag_p %>% filter_all(all_vars(. > .05)) # only one correlation is non sig (birth weight)

### ### ### ### ### ### ### ###
#### CORRELATIONS COMPARISONS #####
### ### ### ### ### ### ### ###
BRIA$BRIA_BAG = BRIA$brainage_gap
DKI$DKI_BAG = DKI$brainage_gap
DTI$DTI_BAG = DTI$brainage_gap
SMT$SMT_BAG = SMT$brainage_gap
SMT_mc$SMT_mc_BAG = SMT_mc$brainage_gap
WMTI$WMTI_BAG = WMTI$brainage_gap
MEAN$MEAN_BAG = MEAN$brainage_gap
FULL$FULL_BAG = FULL$brainage_gap

final = merge(BRIA, FULL, by = "eid")
mean_reduced = merge(BRIA, MEAN, by = "eid")
final$BRIA_BAG = BRIA$BRIA_BAG
final$DKI_BAG = DKI$DKI_BAG
final$DTI_BAG = DTI$DTI_BAG
final$SMT_BAG = SMT$SMT_BAG
final$SMT_mc_BAG = SMT_mc$SMT_mc_BAG
final$WMTI_BAG = WMTI$WMTI_BAG
final$MEAN_BAG = mean_reduced$MEAN_BAG

final = final %>% select(FULL_BAG,MEAN_BAG,BRIA_BAG, DKI_BAG,DTI_BAG,SMT_BAG,SMT_mc_BAG,WMTI_BAG, age.x, t3_income.x, matrix_puzzles_solved_t3.x, tower_arranging.x, 
                                                   prospective_memory_t3.x,fluid_intelligence_t3.x,digits_remembered_t3.x, mean_inc_pair_matches.x,
                                                   job_satisfaction_t3.x,excl_sample.finance_satisfaction_t3.x,excl_sample.health_satisfaction_t3.x,
                                                   overall_health_t3.x, excl_sample.family_rel_satisfaction_t3.x, excl_sample.friend_rel_satisfaction_t3.x,
                                                   happiness_t3.x,BMI.2.0.x, pulse_pressure.2.0.x,WHR.2.0.x,birth_weight_t1.x, daily_coffee_t3.x)
final = final %>% rename_with(~str_remove(., '.x'))
final$t3_income = as.numeric(final$t3_income)
final$job_satisfaction_t3 = as.numeric(final$job_satisfaction_t3)
final$excl_sample.finance_satisfaction_t3 = as.numeric(final$cl_sample.finance_satisfaction_t3)
final$excl_sample.health_satisfaction_t3 = as.numeric(final$cl_sample.health_satisfaction_t3)
final$overall_health_t3 = as.numeric(final$overall_health_t3)
final$excl_sample.family_rel_satisfaction_t3 = as.numeric(final$cl_sample.family_rel_satisfaction_t3)
final$excl_sample.friend_rel_satisfaction_t3 = as.numeric(final$cl_sample.friend_rel_satisfaction_t3)
final$happiness_t3 = as.numeric(final$happiness_t3)

# Looking at the smallest correlation differences (Figure 4)
# for age
cocor(~BRIA_BAG+age|DKI_BAG+age, data = final)
# income
cocor(~MEAN_BAG+t3_income|BRIA_BAG+t3_income, data = final)#H0 not rejected
cocor(~MEAN_BAG+t3_income|DKI_BAG+t3_income, data = final)#H0 not rejected
cocor(~BRIA_BAG+t3_income|DKI_BAG+t3_income, data = final)#H0 not rejected
cocor(~DTI_BAG+t3_income|WMTI_BAG+t3_income, data = final)#H0 barely rejected
# matrix puzzles
cocor(~DKI_BAG+matr_puzzles_solved_t3.x|BRIA_BAG+matr_puzzles_solved_t3.x, data = final)
cocor(~DTI_BAG+matr_puzzles_solved_t3.x|WMTI_BAG+matr_puzzles_solved_t3.x, data = final)
cocor(~SMT_BAG+matr_puzzles_solved_t3.x|SMT_mc_BAG+matr_puzzles_solved_t3.x, data = final)
# tower arranging
cocor(~BRIA_BAG+tower_arranging|DKI_BAG+tower_arranging, data = final)
cocor(~BRIA_BAG+tower_arranging|DTI_BAG+tower_arranging, data = final)#
cocor(~BRIA_BAG+tower_arranging|SMT_mc_BAG+tower_arranging, data = final)
cocor(~SMT_mc_BAG+tower_arranging|DKI_BAG+tower_arranging, data = final)
cocor(~SMT_BAG+tower_arranging|WMTI_BAG+tower_arranging, data = final)
#prosp memory
cocor(~MEAN_BAG+prospective_memory_t3|DTI_BAG+prospective_memory_t3, data = final)#
cocor(~MEAN_BAG+prospective_memory_t3|WMTI_BAG+prospective_memory_t3, data = final)#
cocor(~DTI_BAG+prospective_memory_t3|WMTI_BAG+prospective_memory_t3, data = final)#

cocor(~FULL_BAG+prospective_memory_t3|DKI_BAG+prospective_memory_t3, data = final)#

cocor(~BRIA_BAG+prospective_memory_t3|SMT_BAG+prospective_memory_t3, data = final)#
cocor(~BRIA_BAG+prospective_memory_t3|SMT_mc_BAG+prospective_memory_t3, data = final)#
cocor(~SMT_BAG+prospective_memory_t3|SMT_mc_BAG+prospective_memory_t3, data = final)#

# Fluid intelligence
cocor(~BRIA_BAG+fluid_intelligence_t3|DTI_BAG+fluid_intelligence_t3, data = final)
cocor(~BRIA_BAG+fluid_intelligence_t3|SMT_BAG+fluid_intelligence_t3, data = final)
cocor(~BRIA_BAG+fluid_intelligence_t3|SMT_mc_BAG+fluid_intelligence_t3, data = final)#
cocor(~BRIA_BAG+fluid_intelligence_t3|WMTI_BAG+fluid_intelligence_t3, data = final)
cocor(~DTI_BAG+fluid_intelligence_t3|WMTI_BAG+fluid_intelligence_t3, data = final)
cocor(~DTI_BAG+fluid_intelligence_t3|SMT_BAG+fluid_intelligence_t3, data = final)
cocor(~DTI_BAG+fluid_intelligence_t3|SMT_mc_BAG+fluid_intelligence_t3, data = final)
cocor(~SMT_mc_BAG+fluid_intelligence_t3|WMTI_BAG+fluid_intelligence_t3, data = final)
cocor(~SMT_mc_BAG+fluid_intelligence_t3|SMT_BAG+fluid_intelligence_t3, data = final)
cocor(~WMTI_BAG+fluid_intelligence_t3|SMT_BAG+fluid_intelligence_t3, data = final)

# Digits remembered
cocor(~BRIA_BAG+digits_remembered_t3|DTI_BAG+digits_remembered_t3, data = final)
cocor(~BRIA_BAG+digits_remembered_t3|SMT_BAG+digits_remembered_t3, data = final)
cocor(~BRIA_BAG+digits_remembered_t3|SMT_mc_BAG+digits_remembered_t3, data = final)
cocor(~BRIA_BAG+digits_remembered_t3|WMTI_BAG+digits_remembered_t3, data = final)
cocor(~DTI_BAG+digits_remembered_t3|WMTI_BAG+digits_remembered_t3, data = final)
cocor(~DTI_BAG+digits_remembered_t3|SMT_BAG+digits_remembered_t3, data = final)

cocor(~SMT_mc_BAG+digits_remembered_t3|WMTI_BAG+digits_remembered_t3, data = final)
cocor(~SMT_mc_BAG+digits_remembered_t3|DTI_BAG+digits_remembered_t3, data = final)


# pair matches
cocor(~BRIA_BAG+mean_inc_pair_matches|SMT_mc_BAG+mean_inc_pair_matches, data = final) #
cocor(~DKI_BAG+mean_inc_pair_matches|DTI_BAG+mean_inc_pair_matches, data = final) #
cocor(~WMTI_BAG+mean_inc_pair_matches|SMT_BAG+mean_inc_pair_matches, data = final) #

# job sat
cocor(~FULL_BAG+job_satisfaction_t3|BRIA_BAG+job_satisfaction_t3, data = final) 
cocor(~FULL_BAG+job_satisfaction_t3|DTI_BAG+job_satisfaction_t3, data = final) 
cocor(~FULL_BAG+job_satisfaction_t3|WMTI_BAG+job_satisfaction_t3, data = final) 

cocor(~BRIA_BAG+job_satisfaction_t3|DTI_BAG+job_satisfaction_t3, data = final) #
cocor(~BRIA_BAG+job_satisfaction_t3|WMTI_BAG+job_satisfaction_t3, data = final) #
cocor(~DTI_BAG+job_satisfaction_t3|WMTI_BAG+job_satisfaction_t3, data = final) #

cocor(~DKI_BAG+job_satisfaction_t3|SMT_BAG+job_satisfaction_t3, data = final) #
cocor(~DKI_BAG+job_satisfaction_t3|SMT_mc_BAG+job_satisfaction_t3, data = final) #
cocor(~SMT_mc_BAG+job_satisfaction_t3|SMT_BAG+job_satisfaction_t3, data = final) #

# finance sat 
cocor(~FULL_BAG+cl_sample.finance_satisfaction_t3.x|BRIA_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.finance_satisfaction_t3.x|DKI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.finance_satisfaction_t3.x|DTI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.finance_satisfaction_t3.x|SMT_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.finance_satisfaction_t3.x|SMT_mc_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 

cocor(~BRIA_BAG+cl_sample.finance_satisfaction_t3.x|DKI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) #
cocor(~BRIA_BAG+cl_sample.finance_satisfaction_t3.x|DTI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) #
cocor(~BRIA_BAG+cl_sample.finance_satisfaction_t3.x|SMT_mc_BAG+cl_sample.finance_satisfaction_t3.x, data = final) # 

cocor(~BRIA_BAG+cl_sample.finance_satisfaction_t3.x|SMT_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~SMT_BAG+cl_sample.finance_satisfaction_t3.x|DKI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) 
cocor(~SMT_BAG+cl_sample.finance_satisfaction_t3.x|DTI_BAG+cl_sample.finance_satisfaction_t3.x, data = final) #
cocor(~SMT_BAG+cl_sample.finance_satisfaction_t3.x|SMT_mc_BAG+cl_sample.finance_satisfaction_t3.x, data = final)

cocor(~SMT_mc_BAG+cl_sample.finance_satisfaction_t3.x|DTI_BAG+cl_sample.finance_satisfaction_t3.x, data = final)
cocor(~SMT_mc_BAG+cl_sample.finance_satisfaction_t3.x|DKI_BAG+cl_sample.finance_satisfaction_t3.x, data = final)
cocor(~DTI_BAG+cl_sample.finance_satisfaction_t3.x|DKI_BAG+cl_sample.finance_satisfaction_t3.x, data = final)

# health sat
cocor(~FULL_BAG+cl_sample.health_satisfaction_t3.x|MEAN_BAG+cl_sample.health_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.health_satisfaction_t3.x|DKI_BAG+cl_sample.health_satisfaction_t3.x, data = final) 
cocor(~MEAN_BAG+cl_sample.health_satisfaction_t3.x|DKI_BAG+cl_sample.health_satisfaction_t3.x, data = final) #

cocor(~BRIA_BAG+cl_sample.health_satisfaction_t3.x|SMT_BAG+cl_sample.health_satisfaction_t3.x, data = final) 
cocor(~BRIA_BAG+cl_sample.health_satisfaction_t3.x|SMT_mc_BAG+cl_sample.health_satisfaction_t3.x, data = final) 
cocor(~SMT_BAG+cl_sample.health_satisfaction_t3.x|SMT_mc_BAG+cl_sample.health_satisfaction_t3.x, data = final) #
cocor(~DTI_BAG+cl_sample.health_satisfaction_t3.x|DKI_BAG+cl_sample.health_satisfaction_t3.x, data = final) 

# self-reated health
cocor(~FULL_BAG+overall_health_t3|MEAN_BAG+overall_health_t3, data = final) 
cocor(~FULL_BAG+overall_health_t3|DKI_BAG+overall_health_t3, data = final) 
cocor(~MEAN_BAG+overall_health_t3|DKI_BAG+overall_health_t3, data = final) 

cocor(~BRIA_BAG+overall_health_t3|DTI_BAG+overall_health_t3, data = final) #
cocor(~BRIA_BAG+overall_health_t3|WMTI_BAG+overall_health_t3, data = final) 
cocor(~WMTI_BAG+overall_health_t3|DTI_BAG+overall_health_t3, data = final) #

# family rel
cocor(~FULL_BAG+cl_sample.family_rel_satisfaction_t3.x|BRIA_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.family_rel_satisfaction_t3.x|DKI_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~FULL_BAG+cl_sample.family_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) #

cocor(~BRIA_BAG+cl_sample.family_rel_satisfaction_t3.x|DKI_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~BRIA_BAG+cl_sample.family_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~DKI_BAG+cl_sample.family_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 

cocor(~DTI_BAG+cl_sample.family_rel_satisfaction_t3.x|SMT_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~DTI_BAG+cl_sample.family_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 
cocor(~SMT_BAG+cl_sample.family_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.family_rel_satisfaction_t3.x, data = final) 

# friend relations
cocor(~BRIA_BAG+cl_sample.friend_rel_satisfaction_t3.x|DKI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~BRIA_BAG+cl_sample.friend_rel_satisfaction_t3.x|DTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~BRIA_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~BRIA_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) # 
cocor(~BRIA_BAG+cl_sample.friend_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #

cocor(~DKI_BAG+cl_sample.friend_rel_satisfaction_t3.x|DTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~DKI_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~DKI_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~DKI_BAG+cl_sample.friend_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #

cocor(~DTI_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~DTI_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #
cocor(~DTI_BAG+cl_sample.friend_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) 

cocor(~SMT_BAG+cl_sample.friend_rel_satisfaction_t3.x|SMT_mc_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final)
cocor(~SMT_BAG+cl_sample.friend_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) 
cocor(~SMT_mc_BAG+cl_sample.friend_rel_satisfaction_t3.x|WMTI_BAG+cl_sample.friend_rel_satisfaction_t3.x, data = final) #

# happy
cocor(~BRIA_BAG+happiness_t3|DTI_BAG+happiness_t3, data = final) #
cocor(~BRIA_BAG+happiness_t3|DKI_BAG+happiness_t3, data = final) #
cocor(~BRIA_BAG+happiness_t3|SMT_BAG+happiness_t3, data = final) #
cocor(~BRIA_BAG+happiness_t3|SMT_mc_BAG+happiness_t3, data = final) #
cocor(~BRIA_BAG+happiness_t3|WMTI_BAG+happiness_t3, data = final) #

cocor(~DTI_BAG+happiness_t3|DKI_BAG+happiness_t3, data = final) #
cocor(~DTI_BAG+happiness_t3|SMT_BAG+happiness_t3, data = final) #
cocor(~DTI_BAG+happiness_t3|SMT_mc_BAG+happiness_t3, data = final) #
cocor(~DTI_BAG+happiness_t3|WMTI_BAG+happiness_t3, data = final) #

cocor(~DKI_BAG+happiness_t3|SMT_BAG+happiness_t3, data = final) 
cocor(~DKI_BAG+happiness_t3|SMT_mc_BAG+happiness_t3, data = final) #
cocor(~DKI_BAG+happiness_t3|WMTI_BAG+happiness_t3, data = final) #
cocor(~SMT_BAG+happiness_t3|SMT_mc_BAG+happiness_t3, data = final) #
cocor(~SMT_BAG+happiness_t3|WMTI_BAG+happiness_t3, data = final) #
cocor(~SMT_mc_BAG+happiness_t3|WMTI_BAG+happiness_t3, data = final) #

# bmi
cocor(~BRIA_BAG+BMI.2.0|SMT_BAG+BMI.2.0, data = final) #
cocor(~DKI_BAG+BMI.2.0|SMT_mc_BAG+BMI.2.0, data = final) #
cocor(~FULL_BAG+BMI.2.0|SMT_mc_BAG+BMI.2.0, data = final) #
cocor(~DKI_BAG+BMI.2.0|FULL_BAG+BMI.2.0, data = final) #
cocor(~DKI_BAG+BMI.2.0|WMTI_BAG+BMI.2.0, data = final) #
cocor(~WMTI_BAG+BMI.2.0|FULL_BAG+BMI.2.0, data = final) #

# PP
cocor(~BRIA_BAG+pulse_pressure.2.0|WMTI_BAG+pulse_pressure.2.0, data = final) #
cocor(~DKI_BAG+pulse_pressure.2.0|SMT_mc_BAG+pulse_pressure.2.0, data = final) #

#WHR
cocor(~FULL_BAG+WHR.2.0|BRIA_BAG+WHR.2.0, data = final) #
cocor(~FULL_BAG+WHR.2.0|WMTI_BAG+WHR.2.0, data = final) #
cocor(~WMTI_BAG+WHR.2.0|BRIA_BAG+WHR.2.0, data = final) #

cocor(~DKI_BAG+WHR.2.0|DTI_BAG+WHR.2.0, data = final) #
cocor(~SMT_BAG+WHR.2.0|SMT_mc_BAG+WHR.2.0, data = final) #

# (birth weight not considered)

# coffee
cocor(~FULL_BAG+daily_coffee_t3|DKI_BAG+daily_coffee_t3, data = final) #
cocor(~FULL_BAG+daily_coffee_t3|DTI_BAG+daily_coffee_t3, data = final) #
cocor(~FULL_BAG+daily_coffee_t3|MEAN_BAG+daily_coffee_t3, data = final) #
cocor(~FULL_BAG+daily_coffee_t3|SMT_BAG+daily_coffee_t3, data = final) #
cocor(~FULL_BAG+daily_coffee_t3|SMT_mc_BAG+daily_coffee_t3, data = final) #
cocor(~FULL_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #

cocor(~DKI_BAG+daily_coffee_t3|DTI_BAG+daily_coffee_t3, data = final) #
cocor(~DKI_BAG+daily_coffee_t3|MEAN_BAG+daily_coffee_t3, data = final) #
cocor(~DKI_BAG+daily_coffee_t3|SMT_BAG+daily_coffee_t3, data = final) #
cocor(~DKI_BAG+daily_coffee_t3|SMT_mc_BAG+daily_coffee_t3, data = final) #
cocor(~DKI_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #

cocor(~DTI_BAG+daily_coffee_t3|MEAN_BAG+daily_coffee_t3, data = final) #
cocor(~DTI_BAG+daily_coffee_t3|SMT_BAG+daily_coffee_t3, data = final) #
cocor(~DTI_BAG+daily_coffee_t3|SMT_mc_BAG+daily_coffee_t3, data = final) #
cocor(~DTI_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #

cocor(~MEAN_BAG+daily_coffee_t3|SMT_BAG+daily_coffee_t3, data = final) #
cocor(~MEAN_BAG+daily_coffee_t3|SMT_mc_BAG+daily_coffee_t3, data = final) #
cocor(~MEAN_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #

cocor(~SMT_BAG+daily_coffee_t3|SMT_mc_BAG+daily_coffee_t3, data = final) #
cocor(~SMT_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #
cocor(~SMT_mc_BAG+daily_coffee_t3|WMTI_BAG+daily_coffee_t3, data = final) #

### DIFFERENCES BETWEEN DIFFERENCES ####
brainage_list = list(FULL, MEAN, BRIA, DKI, DTI, SMT, SMT_mc, WMTI)

######### Differences between several groups
kruskal_e = data.frame(matrix(nrow = 2, ncol = 8))
colnames(kruskal_e)= c("Full Multimodal", "Mean Multimodal", "BRIA", "DKI", "DTI", "SMT", "SMT mc", "WMTI")
for (i in 1:8){
  kruskal_e[1,i] = brainage_list[[i]] %>% kruskal_effsize(brainage_gap ~ site_t3) %>% pull(effsize)
  kruskal_e[2, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(brainage_gap ~ t3_income) %>% pull(effsize)
  #df %>% kruskal_effsize(brainage_gap ~ t3_income) # this would include 'don't know' and 'prefer not to say'
}
rownames(kruskal_e) = c("Scanner Site", "Income")

# plot
kruskal_e_heat = as.ggplot(pheatmap(kruskal_e,cluster_rows = F,cluster_cols = F, display_numbers = TRUE, number_format = "%.4f"))
for (i in 1:8){
  kruskal_e[1,i] = brainage_list[[i]] %>% kruskal_test(brainage_gap ~ site_t3) %>% pull(p)
  kruskal_e[2, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_test(brainage_gap ~ t3_income) %>% pull(p)
}
#show p vals
kruskal_e

######### Differences between two groups
wilcox_d = data.frame(matrix(nrow = 7, ncol = 8))
colnames(wilcox_d)= c("Full Multimodal", "Mean Multimodal", "BRIA", "DKI", "DTI", "SMT", "SMT mc", "WMTI")
for (i in 1:8){
  wilcox_d[1,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ sex) %>% pull(effsize)
  wilcox_d[2,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ Ethnicity) %>% pull(effsize)
  wilcox_d[3,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ higher_education_t3) %>% pull(effsize)
  wilcox_d[4,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ smoking.2.0) %>% pull(effsize)
  wilcox_d[5,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ diabetic.2.0) %>% pull(effsize)
  wilcox_d[6,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ hypertension.2.0) %>% pull(effsize)
  wilcox_d[7,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ high_cholesterol.2.0) %>% pull(effsize)
  wilcox_d[8,i] = brainage_list[[i]] %>% wilcox_effsize(brainage_gap ~ diagnosed_vascular_problem) %>% pull(effsize)
}
rownames(wilcox_d) = c("Sex","Ethnicity", "Higher Education", "Smoking", "Diabetic", "Hypertension", "High Cholesterol", "Vascular Diagnosis")
wilcox_d_heat = as.ggplot(pheatmap(wilcox_d,cluster_rows = F,cluster_cols = F, display_numbers = TRUE, number_format = "%.2f"))

# check p-vals
for (i in 1:8){
  wilcox_d[1,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ sex) %>% pull(p)
  wilcox_d[2,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ Ethnicity) %>% pull(p)
  wilcox_d[3,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ higher_education_t3) %>% pull(p)
  wilcox_d[4,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ smoking.2.0) %>% pull(p)
  wilcox_d[5,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ diabetic.2.0) %>% pull(p)
  wilcox_d[6,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ hypertension.2.0) %>% pull(p)
  wilcox_d[7,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ high_cholesterol.2.0) %>% pull(p)
  wilcox_d[8,i] = brainage_list[[i]] %>% wilcox_test(brainage_gap ~ diagnosed_vascular_problem) %>% pull(p)
}
#show p vals
wilcox_d %>% filter_all(all_vars(. >.001))

# arrange plots
DiffDiff = ggarrange(kruskal_e_heat, wilcox_d_heat, ncol = 1, nrow = 2, labels = c("a","b"))

# save plot
#ggsave("/tsd/p33/home/p33-maxk/export/DiffDiff.pdf",DiffDiff, height = 6, width = 10)
ggsave("/cluster/projects/p33/users/maxk/UKB/export/BAG_DiffDiff.pdf",DiffDiff, height = 6, width = 10)

#### COMPARE BAGs ####
library(reshape2)
BAG_comp = melt(final[,1:8])
BAG_comp %>% kruskal_test(value ~ variable)

#¤¤¤ by income ¤¤¤
kruskal_explore = data.frame(matrix(nrow = 18, ncol = 8))
colnames(kruskal_explore)= c("Full Multimodal", "Mean Multimodal", "BRIA", "DKI", "DTI", "SMT", "SMT mc", "WMTI")
rownames(kruskal_explore) = c("Matrix Puzzles", "Tower Arranging", "Prospective Memory", "Fluid Intelligence", "Digits Remembered", "Pair Matches", "Job Satisfaction","Finance Satisfaction", "Health Satisfaction", "Self-Rated Health", "Family Relationship Satisfaction", "Friend Relationship Satisfaction", "Happiness", "BMI", "Pulse Pressure", "WHR", "Birth Weight", "Daily Coffee")
#brainage_list[[1]]$check
for (i in 1:8){
  kruskal_explore[1, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(matrix_puzzles_solved_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[2, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(tower_arranging ~ t3_income) %>% pull(effsize)
  kruskal_explore[3, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(prospective_memory_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[4, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(fluid_intelligence_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[5, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(digits_remembered_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[6, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(mean_inc_pair_matches ~ t3_income) %>% pull(effsize)
  kruskal_explore[7, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(job_satisfaction_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[8, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(excl_sample.finance_satisfaction_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[9, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(excl_sample.health_satisfaction_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[10, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(overall_health_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[11, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(excl_sample.family_rel_satisfaction_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[12, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(excl_sample.friend_rel_satisfaction_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[13, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(happiness_t3 ~ t3_income) %>% pull(effsize)
  kruskal_explore[14, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(BMI.2.0 ~ t3_income) %>% pull(effsize)
  kruskal_explore[15, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(pulse_pressure.2.0 ~ t3_income) %>% pull(effsize)
  kruskal_explore[16, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(WHR.2.0 ~ t3_income) %>% pull(effsize)
  kruskal_explore[17, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(birth_weight_t1 ~ t3_income) %>% pull(effsize)
  kruskal_explore[18, i] = brainage_list[[i]] %>% filter(t3_income != c("Don't know", "Prefer not to answer"))%>% kruskal_effsize(daily_coffee_t3 ~ t3_income) %>% pull(effsize)
}
kruskal_explore

# explore some of the variables by income (only for full multimodal model)
table(brainage_list[[1]]$diabetic.2.0, brainage_list[[1]]$t3_income)
table(brainage_list[[1]]$hypertension.2.0, brainage_list[[1]]$t3_income)
table(brainage_list[[1]]$high_cholesterol.2.0, brainage_list[[1]]$t3_income)
table(brainage_list[[1]]$diagnosed_vascular_problem, brainage_list[[1]]$t3_income)