# "Omnibus" approach based on Vandenberg and Lance 2000

rm(list=ls())

library(lavaan)
library(tidyverse)
library(googledrive)

# Restarting point
load("2022-09-05_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 

######################################################################################################
# QUESTION FOR LISA: CONFIRM THAT FIRST STEP IS TO IDENTIFY AN EXPLORATORY MODEL USING EFA and confirm on the POST data
# STEP 1: Since EFA was done on just the PRE data, do a CFA on just the POST data to make sure the EFA was not overfitting the data

# Define model:
# Only allow one item to load onto each latent variable (assumption with CFA is that each items loads equally)
# Additional guidance: try testing an "expected" model based on quesitonnaire (e.g., "understanding," "skills," etc) and use anova to compare with EFA-suggested model
# Use EFA (data) and expected model (theory) along with model fit measures and anova to arrive at final model

# REMINDER: three factor model has really bad RMSEA (> 0.1); borderline acceptable cutoff would be < 0.08
# EFA Results for THREE factors: https://docs.google.com/spreadsheets/d/1kZZ86hJYzD80pWJzTSI2UVlvumgmdh0exPSwoTb8lb0/edit#gid=1196504491
# model <- '
# process =~ skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_relatetolife + understanding_sciprocess
# identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic + attitudes_workwithothers + integration_applyingknowledge
# confidence =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
#  '


# EFA Results for FOUR factors: https://docs.google.com/spreadsheets/d/1y5w1OhuB2VbnYgcQ5W9d5iI8qeC2xQKIQVf-LetNMmc/edit#gid=1486559735
model <- '
process =~ skills_developH0 + skills_evalH0 + skills_testH0
identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
integration =~ integration_applyingknowledge + integration_connectingknowledge
content =~ understanding_ecology + understanding_fertilization
 '

# Fit indices (results) are the same for parameterization = "delta" (same setting as measurement invariance) or "theta"
# Use MLR estimator for continuous-ish variables (e.g., often used when there are 5 or more response options)
fit <- cfa(model, data = tidy_dat_post_final, std.lv = TRUE, estimator = "MLR") 


# fit_3 <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
# fit_3_theta <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = FALSE, parameterization = "theta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
summary(fit, fit.measures = TRUE)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(post_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(post_fit) <- c("model_delta-parameter")
post_fit[1,] <- round(data.matrix(fitmeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

post_fit

overfit_test_name <- "meas-invar_CFA-on-post-to-check-for-overfitting.txt"
sink(overfit_test_name)
print(post_fit)
sink()

#drive_upload(overfit_test_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("18Zdh3-sD81h7CUU_Ip8lbVR4WGH6z1Zc"), media = overfit_test_name)  
file.remove(overfit_test_name)
# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is < 0.08 is marginal
# From Leandre R ,et al. "Evaluating the use of exploratory factor analysis in psychological research." Psychological methods 4, no. 3 (1999): 272:
# "It has been suggested that RMSEA values less than 0.05 are good, values between 0.05 and 0.08 are acceptable, 
# values between 0.08 and 0.1 are marginal, and values greater than 0.1 are poor [8]. 

######################################################################################################
# STEP 2: OMNIBUS APPROACH TO MEASUREMENT INVARIANCE ANALYSIS based on Vandenberg and Lance 2000
# Estimate the null model. The null model does not include any differences across groups

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

no_groups <-cfa(model, tidy_dat_all, estimator = "MLR", meanstructure = T)
summary(no_groups, standardized = T, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
no_groups_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(no_groups_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(no_groups_fit) <- c("model_delta-parameter")
no_groups_fit[1,] <- round(data.matrix(fitmeasures(no_groups, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

no_groups_fit

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

test_mod <- cfa(model, tidy_dat_all, estimator = "MLR", group = "test")
summary(test_mod, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
test_mod_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(test_mod_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(test_mod_fit) <- c("model_delta-parameter")
test_mod_fit[1,] <- round(data.matrix(fitmeasures(test_mod, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

test_mod_fit


######################################################################################################
# STEP 4 - Compare the null and pre/post models

# CONFIRM WITH LISA: emailed on 9/5/22
test_model_fit
no_groups_fit

anova(no_groups, test_mod)
######################################################################################################
# STEP 5 - Calculate means in SALG responses for the three latent factors pre vs post and do T-test for significance

# QUESTION FOR LISA: How to calculate means for the three latent factors and compare - is it just a T-test for statistical significance?

# FIX IT - OUTPUT FINAL SAMPLE NUMBERS:
tidy_dat_all %>%
  #na.omit() %>%
  group_by(test) %>%
  count()

# CALCULATE GROUP MEANS ACROSS STUDENT RESPONSES
group_means <- tidy_dat_all %>%
  na.omit() %>%
  group_by(test) %>%
  summarise(attitudes_career = mean(attitudes_career, na.rm = TRUE), 
            attitudes_confidentresearch = mean(attitudes_confidentresearch, na.rm = TRUE),
            attitudes_confidentunderstanding = mean(attitudes_confidentunderstanding, na.rm = TRUE),
            attitudes_discussing = mean(attitudes_discussing, na.rm = TRUE),
            attitudes_enthusiastic = mean(attitudes_enthusiastic, na.rm = TRUE),
            attitudes_workwithothers = mean(attitudes_workwithothers, na.rm = TRUE),
            integration_applyingknowledge = mean(integration_applyingknowledge, na.rm = TRUE),
            integration_connectingknowledge = mean(integration_connectingknowledge, na.rm = TRUE),
            skills_developH0 = mean(skills_developH0, na.rm = TRUE),
            skills_evalH0 = mean(skills_evalH0, na.rm = TRUE),
            skills_testH0 = mean(skills_testH0, na.rm = TRUE),
            skills_withothers = mean(skills_withothers, na.rm = TRUE),
            understanding_ecology = mean(understanding_ecology, na.rm = TRUE),
            understanding_fertilization = mean(understanding_fertilization, na.rm = TRUE),
            understanding_relatetolife = mean(understanding_relatetolife, na.rm = TRUE),
            understanding_sciprocess = mean(understanding_sciprocess, na.rm = TRUE))

# FIX IT - why does this method of calculating means NOT MATCH the t-test calculation of means down below
# CALCULATE MEANS FOR LATENT FACTORS:
group_means %>%
  rowwise() %>%
  mutate(process = mean(skills_developH0, skills_evalH0, skills_testH0, skills_withothers, understanding_relatetolife, understanding_sciprocess),
         identity = mean(attitudes_career, attitudes_discussing, attitudes_enthusiastic, attitudes_workwithothers, integration_applyingknowledge),
         confidence = mean(attitudes_confidentunderstanding, understanding_ecology, understanding_fertilization)) %>%
  select(test, process, identity, confidence)

# T-Test for PROCESS
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0, skills_withothers, understanding_relatetolife, understanding_sciprocess),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0, skills_withothers, understanding_relatetolife, understanding_sciprocess))


# T-test for IDENTITY
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic, attitudes_workwithothers, integration_applyingknowledge),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic, attitudes_workwithothers, integration_applyingknowledge))

# T-test for CONFIDENCE
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_confidentunderstanding, understanding_ecology, understanding_fertilization),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_confidentunderstanding, understanding_ecology, understanding_fertilization))



# QUESTION FOR LISA: Not part of our original research question, but if we want to test: do different genders respond the same way to the intervention?

# QUESTION FOR LISA: What about this type of code?
# OLD CODE:
# To constrain all the latent means (the omnibus test), you can just add "means" to the vector of parameters to constrain in the group.equal argument.  
compare <- measEq.syntax(configural.model = model,
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



