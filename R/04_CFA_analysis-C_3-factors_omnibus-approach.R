# "Omnibus" approach based on Vandenberg and Lance 2000

rm(list=ls())

library(lavaan)
library(tidyverse)
library(googledrive)

# Restarting point
load("2021-07-13_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 

######################################################################################################
# QUESTION FOR LISA: CONFIRM THAT FIRST STEP IS TO IDENTIFY AN EXPLORATORY MODEL USING EFA
# STEP 1: Since EFA was done on just the PRE data, do a CFA on just the POST data to make sure the EFA was not overfitting the data

# Define model:
# Only allow one item to load onto each latent variable (assumption with CFA is that each items loads equally)
# See EFA Results for FOUR factor loadings: https://docs.google.com/spreadsheets/d/1kZZ86hJYzD80pWJzTSI2UVlvumgmdh0exPSwoTb8lb0/edit#gid=1196504491
model_3 <- '
process =~ skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_relatetolife + understanding_sciprocess
identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic + attitudes_workwithothers + integration_applyingknowledge
confidence =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
 '

# Fit indices (results) are the same for parameterization = "delta" (same setting as measurement invariance) or "theta"
fit_3 <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
# fit_3_theta <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = FALSE, parameterization = "theta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
summary(fit_3)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post.results <- matrix(NA, nrow = 1, ncol = 6)
colnames(post.results) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(post.results) <- c("model3_delta-parameter")
post.results[1,] <- round(data.matrix(fitmeasures(fit_3, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

post.results

overfit_test_name <- "meas-invar_CFA-on-post-to-check-for-overfitting.txt"
sink(overfit_test_name)
print(post.results)
sink()

#drive_upload(overfit_test_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("18Zdh3-sD81h7CUU_Ip8lbVR4WGH6z1Zc"), media = overfit_test_name)  
file.remove(overfit_test_name)
# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is > 0.05 (not a good fit)

######################################################################################################
# STEP 2: OMNIBUS APPROACH TO MEASUREMENT INVARIANCE ANALYSIS based on Vandenberg and Lance 2000
# Estimate the null model. The null model does not include any differences across groups

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

no_groups <-cfa(model_3, tidy_dat_all, estimator = "MLR", meanstructure = T)
summary(no_groups, standardized = T, fit.measures = T)

residuals <- residuals(no_groups, type = "standardized")
# write.csv(residuals,"finalmod_residuals.csv") # save?


# QUESTION FOR LISA: Minimum value can just be 0 right? (only 119 rows for my model)
# Calculate modification indices
mod_ind <- modificationindices(no_groups, sort.=TRUE, minimum.value=3)
mod_ind <- modificationindices(no_groups, sort.=TRUE) # default minimum.value = 0
#write.csv(mod_ind,"finalmod_ModInd.csv")

######################################################################################################
# STEP 3 - Multiple Group comparison using test groups (pre vs post)

tidy_dat_all$test <- as.factor(tidy_dat_all$test)

test_mod <- cfa(model_3, tidy_dat_all, estimator = "MLR", group = "test")
# QUESTION FOR LISA:
# Warning message:
#   In lav_object_post_check(object) :
#   lavaan WARNING: some estimated ov variances are negative
summary(test_mod, fit.measures = T)

######################################################################################################
# STEP 4 - Compare the null and pre/post models

anova(no_groups, test_mod)
# CONFIRM WITH LISA: 
# p > 0.05 means no difference between the two models fits, i.e., no differences between groups and measurement invariance confirmed

######################################################################################################
# STEP 5 - Calculate means in SALG responses for the three latent factors pre vs post and do T-test for significance

# QUESTION FOR LISA: How to calculate means for the three latent factors and compare - is it just a T-test for statistical significance?

# FIX IT - OUTPUT FINAL SAMPLE NUMBERS:
tidy_dat_all %>%
  na.omit() %>%
  group_by(test) %>%
  count()

# CALCULATE GROUP MEANS ACROSS STUDENT RESPONSES
group_means <- tidy_dat_all %>%
  na.omit() %>%
  group_by(test) %>%
  summarise(attitudes_career = mean(attitudes_career), 
            attitudes_confidentresearch = mean(attitudes_confidentresearch),
            attitudes_confidentunderstanding = mean(attitudes_confidentunderstanding),
            attitudes_discussing = mean(attitudes_discussing),
            attitudes_enthusiastic = mean(attitudes_enthusiastic),
            attitudes_workwithothers = mean(attitudes_workwithothers),
            integration_applyingknowledge = mean(integration_applyingknowledge),
            integration_connectingknowledge = mean(integration_connectingknowledge),
            skills_developH0 = mean(skills_developH0),
            skills_evalH0 = mean(skills_evalH0),
            skills_testH0 = mean(skills_testH0),
            skills_withothers = mean(skills_withothers),
            understanding_ecology = mean(understanding_ecology),
            understanding_fertilization = mean(understanding_fertilization),
            understanding_relatetolife = mean(understanding_relatetolife),
            understanding_sciprocess = mean(understanding_sciprocess))

# REMINDER: MODEL
# model_3 <- '
# process =~ skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_relatetolife + understanding_sciprocess
# identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic + attitudes_workwithothers + integration_applyingknowledge
# confidence =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
#  '

# CALCULATE MEANS FOR LATENT FACTORS:
group_means %>%
  rowwise() %>%
  mutate(process = mean(skills_developH0, skills_evalH0, skills_testH0, skills_withothers, understanding_relatetolife, understanding_sciprocess),
         identity = mean(attitudes_career, attitudes_discussing, attitudes_enthusiastic, attitudes_workwithothers, integration_applyingknowledge),
         confidence = mean(attitudes_confidentunderstanding, understanding_ecology, understanding_fertilization)) %>%
  select(test, process, identity, confidence)

# T-Test
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")))





# OLD CODE:
# To constrain all the latent means (the omnibus test), you can just add "means" to the vector of parameters to constrain in the group.equal argument.  
compare <- measEq.syntax(configural.model = model_3,
                         data = tidy_dat_all,
                         ordered = FALSE, # ie all variables are ordinal
                         parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                         ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables [this is also the DEFAULT setting]
                         ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                         group = "test", # column name defining groups
                         group.equal = c("loadings", "intercepts", "means"))

model.compare <- as.character(compare)
fit.compare <- cfa(model.compare, data = tidy_dat_all, group = "test", ordered = FALSE, meanstructure = TRUE)
summary(fit.compare) 



