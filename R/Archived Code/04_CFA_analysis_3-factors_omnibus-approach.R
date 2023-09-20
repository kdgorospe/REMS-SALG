# "Omnibus" approach based on Vandenberg and Lance 2000

rm(list=ls())

library(lavaan)
library(tidyverse)
library(googledrive)

# Restarting point
# Two datasets should be the same (10/08 is from last analysis run on MacBook and 12/24 is from Dell), but keeping both for now
#load("2022-10-08_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 
load("2022-12-24_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 


######################################################################################################
# QUESTION FOR LISA: CONFIRM THAT FIRST STEP IS TO IDENTIFY AN EXPLORATORY MODEL USING EFA and confirm on the POST data
# STEP 1: Since EFA was done on just the PRE data, do a CFA on just the POST data to make sure the EFA was not overfitting the data

# Define model:
# Only allow one item to load onto each latent variable (assumption with CFA is that each items loads equally)
# Use EFA (data) and expected model (theory) along with model fit measures and anova to arrive at final model

# EFA Results for FOUR factors: https://docs.google.com/spreadsheets/d/1PV5B4tzEFQUa1_8KEfHZAFZoYlGPVUG-rbk7Wqgf6b0/edit#gid=1486559735
model <- '
process =~ skills_developH0 + skills_evalH0 + skills_testH0
interest =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
integration =~ integration_applyingknowledge + integration_connectingknowledge
content =~ understanding_ecology + understanding_fertilization
 '

# NOTE: See FOUR factor results with more permissive cutoff = 0.4: https://docs.google.com/spreadsheets/d/1y5w1OhuB2VbnYgcQ5W9d5iI8qeC2xQKIQVf-LetNMmc/edit#gid=1486559735 
# Only gain one more variable and not in any of the factors that could use more measured variables
# Lowering cutoff further to 0.3 makes it more difficult to interpret factors (e.g., work effectively with others starts to load with scientific process factor)
# Since lowering the cutoff doesn't gain us much, keep stricter 0.7 cutoff so that we can say we chose a model with strong loadings

# Fit indices (results) are the same for parameterization = "delta" (same setting as measurement invariance) or "theta"
# Use MLR estimator for continuous-ish variables (e.g., often used when there are 5 or more response options)
# MLR = maximum likelihood estimation with robust (Huber-White) standard errors and a scaled test statistic that is (asymptotically) equal to the Yuan-Bentler test statistic.
# https://lavaan.ugent.be/tutorial/est.html
# Original code, dropping missing data as per https://lavaan.ugent.be/tutorial/est.html
# fit <- cfa(model, data = tidy_dat_post_final, std.lv = TRUE, estimator = "MLR") 

# Implement ML estimation of missing values - Note: NO difference in model fit when running without missing = "ML"
# https://lavaan.ugent.be/tutorial/est.html
fit <- cfa(model, data = tidy_dat_post_final, std.lv = TRUE, estimator = "MLR", missing = "ML") 

# fit_3 <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
# fit_3_theta <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = FALSE, parameterization = "theta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
summary(fit, fit.measures = TRUE)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post_fit <- matrix(NA, nrow = 1, ncol = 7)
colnames(post_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr")
rownames(post_fit) <- c("model_delta-parameter")
post_fit[1,] <- round(data.matrix(fitmeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr"))), digits=3)

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
# From : CFI and TLI values are close to .95 or greater.
# RMSEA however is < 0.08 is marginal
# From Leandre R ,et al. "Evaluating the use of exploratory factor analysis in psychological research." Psychological methods 4, no. 3 (1999): 272:
# "It has been suggested that RMSEA values less than 0.05 are good, values between 0.05 and 0.08 are acceptable, 
# values between 0.08 and 0.1 are marginal, and values greater than 0.1 are poor [8]. 

######################################################################################################
# STEP 2: OMNIBUS APPROACH TO MEASUREMENT INVARIANCE ANALYSIS based on Vandenberg and Lance 2000
# Estimate the null model. The null model does not include any differences across groups

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

# Original code, dropping missing data as per https://lavaan.ugent.be/tutorial/est.html
# no_groups <-cfa(model, tidy_dat_all, estimator = "MLR", meanstructure = T)

# Attempt ML estimation of missing data 
no_groups <-cfa(model, tidy_dat_all, estimator = "MLR", meanstructure = T, missing = "ML")
summary(no_groups, standardized = T, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
no_groups_fit <- matrix(NA, nrow = 1, ncol = 7)
colnames(no_groups_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr")
rownames(no_groups_fit) <- c("model_delta-parameter")
no_groups_fit[1,] <- round(data.matrix(fitmeasures(no_groups, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr"))), digits=3)

no_groups_fit

# Get RMSEA confidence intervals
rmsea_base <- round(fitmeasures(no_groups, fit.measures = c("rmsea.scaled")), digits = 3)
rmsea_ci_base <- paste(round(rmsea_base, digits = 3),
                       " (", 
                       round(fitmeasures(no_groups, fit.measures = c("rmsea.ci.lower.scaled")), digits = 3),
                       "-",
                       round(fitmeasures(no_groups, fit.measures = c("rmsea.ci.upper.scaled")), digits = 3),
                       ")",
                       sep = "", collapse = "")

rmsea_ci_base


######################################################################################################
# STEP 3 - Multiple Group comparison using test groups (pre vs post)

tidy_dat_all$test <- as.factor(tidy_dat_all$test)

# Original code, dropping missing data as per https://lavaan.ugent.be/tutorial/est.html
# test_mod <- cfa(model, tidy_dat_all, estimator = "MLR", group = "test")

# Attempt ML estimation of missing data
test_mod <- cfa(model, tidy_dat_all, estimator = "MLR", group = "test", missing = "ML")
summary(test_mod, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
test_mod_fit <- matrix(NA, nrow = 1, ncol = 7)
colnames(test_mod_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr")
rownames(test_mod_fit) <- c("model_delta-parameter")
test_mod_fit[1,] <- round(data.matrix(fitmeasures(test_mod, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled", "srmr"))), digits=3)

test_mod_fit

# Get RMSEA confidence intervals
rmsea_test <- round(fitmeasures(test_mod, fit.measures = c("rmsea.scaled")), digits = 3)
rmsea_ci_test <- paste(round(rmsea_test, digits = 3),
                       " (", 
                       round(fitmeasures(test_mod, fit.measures = c("rmsea.ci.lower.scaled")), digits = 3),
                       "-",
                       round(fitmeasures(test_mod, fit.measures = c("rmsea.ci.upper.scaled")), digits = 3),
                       ")",
                       sep = "", collapse = "")

rmsea_ci_test


# Compare model fits:
no_groups_fit
test_mod_fit

# CFA is a pretty good fit for "real life" data and 
# no groups model is a better fit which suggests that we have measurement invariance

######################################################################################################
# STEP 4 - Calculate means in SALG responses for the three latent factors pre vs post and do T-test for significance

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


# NOTE: this collapses all variables for a shared factor into a single vector; one for pre and one for post data and performs a t-test on the two vectors
# i.e. - individual variables are lost in that the t-test is not taking a mean of the means for each of the three variables in a factor, rather a single mean for all the data points
# T-Test for PROCESS
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0))

# T-test for INTEREST
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic))

# T-test for INTEGRATION
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge))

# T-test for CONTENT
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization))

# All significant differences between pre and post
# Students were increased their:
# perceived understanding of the scientific process (PROCESS)
# interest in marine science (INTEREST)
# their perceived ability to connect what they learn in classes to other areas
# their perceived knowledge of scientific content (CONTENT)