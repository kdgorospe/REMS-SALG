# Use multiple-group confirmatory factor analysis (MG-CFA) to establish measurement equivalence or invariance (ME/I)
# Purpose of this is to demonstrate that the SALG's ability to measure latent factors was equivalent across groups (i.e., the EFA model holds over time pre vs post)
# Once this is established, can make comparisons about the means of latent groups
# kdgorospe@gmail.com
# Based on: HIRSCHFELD and VON BRACHEL 2014
# and SVETINA et al 2019 (which implements Wu and Estabrook 2016) in "Resources" folder 

rm(list=ls())

library(lavaan)
library(semPlot)
library(semTools)
library(tidyverse)
library(googledrive)

# Restarting point
load("2021-07-13_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 
# REMINDER: dropping pooled variables because when analyzing these as ORDINAL variables, categories must be present in both the pre and post groups
# Pooled variables range from 1 to 6 in increments of 0.5 because responses were averaged
# Example: compare tidy_dat_all %>% filter(test == "pre") %>% select(understanding_sound_pooled) %>% table() versus
# tidy_dat_all %>% filter(test == "post") %>% select(understanding_sound_pooled) %>% table()
# Requires complex collapsing of responses

# Old dataset where pooled variables are retained
# load("2021-02-13_all-data-prior-to-CFA_all-vars.RData") 
######################################################################################################
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
# fit_3_theta <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "theta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
#summary(fit_3)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post.results <- matrix(NA, nrow = 1, ncol = 6)
colnames(post.results) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(post.results) <- c("model3_delta-parameter")
post.results[1,] <- round(data.matrix(fitmeasures(fit_3, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
#post.results[2,] <- round(data.matrix(fitmeasures(fit_3_theta, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

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
# STEP 2: MEASUREMENT INVARIANCE ANALYSIS OF ORDINAL VARIABLES following Svetina et al. 2019

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

# BEFORE PASSING TO ORDINAL FRAMEWORK - NEED TO COLLAPSE RESPONSES THAT ARE NOT PRESENT IN ONE GROUP VS THE OTHER
# Example: table(tidy_dat_all %>% filter(test == "pre") %>% select(attitudes_discussing))
# vs: table(tidy_dat_all %>% filter(test == "post") %>% select(attitudes_discussing))
# i.e., need to collapse the "2" response in the pre-test to be "3"
# Otherwise, will get error message: lavaan ERROR: some categories of variable `attitudes_confidentresearch' are empty in group 2
dat_collapse_responses <- tidy_dat_all %>%
  mutate(attitudes_confidentunderstanding = if_else(attitudes_confidentunderstanding == 2, true = 3, false = attitudes_confidentunderstanding),
         attitudes_discussing = if_else(attitudes_discussing %in% c(2, 3), true = 4, false = attitudes_discussing),
         skills_developH0 = if_else(skills_developH0 == 2, true = 3, false = skills_developH0),
         skills_evalH0 = if_else(skills_evalH0 == 2, true = 3, false = skills_evalH0),
         skills_testH0 = if_else(skills_testH0 == 2, true = 3, false = skills_testH0),
         understanding_ecology = if_else(understanding_ecology %in% c(1, 2, 3), true = 4, false = understanding_ecology),
         understanding_fertilization = if_else(understanding_fertilization %in% c(1, 2), true = 3, false = understanding_fertilization),
         understanding_sciprocess = if_else(understanding_sciprocess %in% c(2, 3), true = 4, false = understanding_sciprocess))

# Create matrix for storing results (6 fit indices across three different models)
all.results <- matrix(NA, nrow = 4, ncol = 6)
colnames(all.results) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(all.results) <- c("configural", "weak", "strong", "strict")

# Specify BASELINE model: no constraints across groups or repeated measures
# See ?measEq.syntax (new function in semTools, replaces measurementInvariance functions)
# Following Svetina et al 2019 (which implements Wu and Estabrook 2016) - Measurement invariance for categorical variables
baseline <- measEq.syntax(configural.model = model_3,
                          data = dat_collapse_responses,
                          ordered = TRUE, # ie all variables are ordinal
                          parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                          ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                          ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                          group = "test", # column name defining groups
                          group.equal = "configural")

# orientation to the model:
summary(baseline)

# print list of all constraints in the model:
constraints_baseline <- "meas-invar_constraints_baseline-model.txt"
sink(constraints_baseline)
print(cat(as.character(baseline)))
sink()

#drive_upload(constraints_baseline, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1NLQqME6SN1Ix1EhfaRaBRlKasXTQLeQT"), media = constraints_baseline)  
file.remove(constraints_baseline)

# Fit baseline model
# Warning messages about eigenvalues that are close to zero can be ignored: https://groups.google.com/g/lavaan/c/4y5pmqRz4nk
# specify as.character to submit to lavaan
model.baseline <- as.character(baseline)
fit.baseline <- cfa(model.baseline, data = dat_collapse_responses, group = "test", ordered = TRUE)
#summary(fit.baseline) # Results of baseline model

# Extract just the fit indices:
all.results[1,] <- round(data.matrix(fitmeasures(fit.baseline, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

######################################################################################################
# THRESHOLD INVARIANCE MODEL FOR ORDINAL VARIABLES following Hirschfield and von Brachel 2014
# aka "Proposition 4" in Wu and Estabrook's 2016, equivalent to "WEAK INVARIANCE" for Continuous variables
# NOTE: 
# Threshold are the gaps that separate the different Likert scale categories (e.g., Somewhat agree vs Strongly agree)

prop4 <- measEq.syntax(configural.model = model_3,
                       data = dat_collapse_responses,
                       ordered = TRUE, # ie all variables are ordinal
                       parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                       ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                       ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                       group = "test", # column name defining groups
                       group.equal = "thresholds")

# Fit threshold invariance model
model.prop4 <- as.character(prop4)
fit.prop4 <- cfa(model.prop4, data = dat_collapse_responses, group = "test", ordered = TRUE)
# summary(fit.prop4)

# print list of all constraints in the model:
constraints_prop4 <- "meas-invar_constraints_prop-4-model.txt"
sink(constraints_prop4)
print(cat(as.character(prop4)))
sink()

#drive_upload(constraints_prop4, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1BlK2TrdaG-OxMrBgcFDabYGr-oujz7gD"), media = constraints_prop4)  
file.remove(constraints_prop4)

# Extract just the fit indices
all.results[2,] <- round(data.matrix(fitmeasures(fit.prop4, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

# Use chi-square test to test for difference in model fit between baseline model and model with threshold equality constraints
# p > 0.05 means no difference between the two models fits, despite higher constraints in the latter
# lavTestLRT(fit.baseline, fit.prop4) # Note: lavTestLRT equivalent to anova(fit.baseline, fit.prop4)

# JUst test all models together at the end - i.e., lavTestLRT(model1, model2, model3, model4)
# output results of chi-square test:
# baseline_v_prop4 <- "meas-invar_chi-sq_baseline-v-prop4.txt"
# sink(baseline_v_prop4)
# print(lavTestLRT(fit.baseline, fit.prop4))
# sink()

#drive_upload(baseline_v_prop4, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
# drive_update(file = as_id("1WCcCtEFyvSUD9z0GKpJoUoiBphGEwm_J"), media = baseline_v_prop4)  
# file.remove(baseline_v_prop4)

######################################################################################################
# THRESHOLD AND LOADING INVARIANCE MODEL
# aka "Proposition 7" in Wu and Estabrook's 2016, equivalent to "Strict invariance" for continuous variables
prop7 <- measEq.syntax(configural.model = model_3,
                       data = dat_collapse_responses,
                       ordered = TRUE, # ie all variables are ordinal
                       parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                       ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                       ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                       group = "test", # column name defining groups
                       group.equal = c("thresholds", "loadings"))

# Fit threshold invariance model
model.prop7 <- as.character(prop7)
fit.prop7 <- cfa(model.prop7, data = dat_collapse_responses, group = "test", ordered = TRUE)
# summary(fit.prop7)

# print list of all constraints in the model:
constraints_prop7 <- "meas-invar_constraints_prop-7-model.txt"
sink(constraints_prop7)
print(cat(as.character(prop7)))
sink()

#drive_upload(constraints_prop7, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1LTq_DQJFLVSj7vGVgFtXJYX6sGuwQ74z"), media = constraints_prop7)  
file.remove(constraints_prop7)

# Extract just the fit indices
all.results[3,] <- round(data.matrix(fitmeasures(fit.prop7, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

# Use chi-square test to test for difference in model fit between model with threshold equality constraints and model with threshold AND loading equality constraints
# p > 0.05 means no difference between the two models fits, despite higher constraints in the latter
# lavTestLRT(fit.prop4, fit.prop7) 

# output results of chi-square test:
# prop4_v_prop7 <- "meas-invar_chi-sq_prop4-v-prop7.txt"
# sink(prop4_v_prop7)
# print(lavTestLRT(fit.prop4, fit.prop7))
# sink()
# 
# #drive_upload(prop4_v_prop7, path = as_dribble("REMS_SALG/Results")) # for initial upload
# # Use drive_update to update specific file based on ID number
# drive_update(file = as_id("1Sral6rcecFkidXVg0DdU5cw5Z7FWwqhz"), media = prop4_v_prop7)  
# file.remove(prop4_v_prop7)

######################################################################################################
# THRESHOLD, LOADING, and VARIANCE INVARIANCE MODEL aka "strict invariance" 
# as per Liu et al 2017, strict invariance must be demonstrated to show 
# in group.equal, add "lv.variances", i.e., residual variance of the latent variables

strict <- measEq.syntax(configural.model = model_3,
                        data = dat_collapse_responses,
                        ordered = TRUE, # ie all variables are ordinal
                        parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                        ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                        ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                        group = "test", # column name defining groups
                        group.equal = c("thresholds", "loadings", "lv.variances"))

# Fit strict invariance model
model.strict <- as.character(strict)
fit.strict <- cfa(model.strict, data = dat_collapse_responses, group = "test", ordered = TRUE)
# summary(fit.prop7)

# print list of all constraints in the model:
constraints_strict <- "meas-invar_constraints_scalar-invar-model.txt"
sink(constraints_strict)
print(cat(as.character(strict)))
sink()

#drive_upload(constraints_strict, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("19HljBfL_zjJTdgxJu3sFGGJkovEcWse7"), media = constraints_strict)  
file.remove(constraints_strict)

# Extract just the fit indices
all.results[4,] <- round(data.matrix(fitmeasures(fit.strict, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

######################################################################################################
# output fit indices for all models
fit_indices <- "meas-invar_all-fit-indices.txt"
sink(fit_indices)
print(all.results)
sink()

#drive_upload(fit_indices, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1SYyed8ySlRw7qEs7WhG0ntf9UcU9iPWx"), media = fit_indices)  
file.remove(fit_indices)

# Interpreting fit indices for INDIVIDUAL models, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# Low chi square means better model fit
# Since chi-sq is dependent on sample size, one way useful benchmark is that chi-sq / df < 5 means good model fit
# Chi-square p-value of model fit are all significant
# RMSEA is > 0.05 (not a good fit)
# Comparative fit index CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RESULTS: Model fits for individual models are all good except for RMSEA

######################################################################################################
# Use chi-square test to test for difference in model fit between each successively stricter model
# p > 0.05 means no difference between the two models fits, despite higher constraints in the latter
lavTestLRT(fit.baseline, fit.prop4, fit.prop7, fit.strict) 

# output results of chi-square test:
chi_sq_all <- "meas-invar_chi-sq-test-for-all-models.txt"
sink(chi_sq_all)
print(lavTestLRT(fit.baseline, fit.prop4, fit.prop7, fit.strict) )
sink()

#drive_upload(chi_sq_all, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("14SBbiV9UsAY46QNGfSkHGG9PmfEEDVeX"), media = chi_sq_all)  
file.remove(chi_sq_all)

# Interpreting fit indices ACROSS models
# CFI should decrease (model fit should decrease) with more constrained models 
# Convention is Delta CFI < 0.01 (decreased fit, but not too much) indicates measurement invariance (in our case, we actually see slight increases in CFI - i.e., negative Delta CFI which is allowed)
# Convention is that Delta RMSEA < 0.015 (decreased fit, but not too much) indicates measurement invariance

# RESULTS: 
# Comparing baseline to prop4 did not show a significant decrease in model fit, so ordinal variable thresholds are equivalent
# Comparing prop4 to prop7 did not show a significant decrease in model fit, so variable loadings onto latent factors are equivalent
# p < 0.05 chi-sq test when comparing prop 7 with strict model meaning that strict invariance fails

# REMINDER: Attempted re-doing entire analysis with parameterization = "theta" and estimator = "DWLS" (as per Liu et al 2017) and overall result was the same (strict invariance fails)
# REMINDER: Attempted re-doing strict invariance test by contraining the "intercepts" instead of "lv.variances" (as per Liu et al 2017) and overall result was the same (strict invariance fails)

######################################################################################################
# Since strict invariance fails, move to PARTIAL INVARIANCE ANALYSIS


# OLD: Use highest "mi" scores to decide which parameters should be "freed"
strict.indices <- modindices(fit.strict, free.remove = FALSE)
strict.indices <- modindices(fit.strict, epc)
strict.indices[strict.indices$op == "=~",] %>%
   arrange(desc(mi))

# Try lavTestScore
lavTestScore(fit.strict, epc = TRUE)

# Modification index: the improvement in model fit in terms of the chi-square statistic IF we refit the model but allow this parameter (i.e., column LHS) to be free
# process =~ attitudes_workwithothers
# identity =~ attitudes_confidentunderstanding
# identity =~ skills_withothers
# confidence =~ understanding_relatetolife

# partial.strict <- measEq.syntax(configural.model = model_3,
#                         data = dat_collapse_responses,
#                         ordered = TRUE, # ie all variables are ordinal
#                         parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
#                         ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
#                         ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
#                         group = "test", # column name defining groups
#                         group.equal = c("thresholds", "loadings", "lv.variances"),
#                         group.partial = c("process =~ attitudes_workwithothers",
#                                           "identity =~ attitudes_confidentunderstanding"))

# Fit partial.strict invariance model
# model.partial.strict <- as.character(partial.strict)
# fit.partial.strict <- cfa(model.partial.strict, data = dat_collapse_responses, group = "test", ordered = TRUE)
# lavTestLRT(fit.baseline, fit.prop4, fit.prop7, fit.partial.strict)
# anova(fit.prop7, fit.partial.strict)
###############################################################################################
# Continued: PARTIAL INVARIANCE ANALYSIS
# Use highest epc scores (all epc > 0.1) to decide which parameters to be "freed"
strict.indices[strict.indices$op == "=~",] %>%
  arrange(desc(epc))

# EPC > 0.1
# lhs op                              rhs block group level     mi    epc sepc.lv sepc.all sepc.nox
# 1    identity =~ attitudes_confidentunderstanding     1     1     1 26.498  1.015   1.015    1.015    1.015
# 2     process =~         attitudes_workwithothers     2     2     1 33.354  0.757   0.757    1.011    1.011
# 3  confidence =~         attitudes_workwithothers     2     2     1 16.439  0.657   0.657    0.877    0.877
# 4    identity =~                skills_withothers     2     2     1 25.205  0.536   0.536    0.794    0.794
# 5  confidence =~                skills_withothers     2     2     1 18.105  0.534   0.534    0.792    0.792
# 6     process =~    integration_applyingknowledge     2     2     1 10.508  0.361   0.361    0.442    0.442
# 7     process =~         attitudes_workwithothers     1     1     1 15.529  0.341   0.341    0.341    0.341
# 8  confidence =~    integration_applyingknowledge     2     2     1  6.454  0.318   0.318    0.390    0.390
# 9  confidence =~       understanding_relatetolife     2     2     1 12.194  0.313   0.313    0.363    0.363
# 10   identity =~       understanding_relatetolife     2     2     1 11.754  0.296   0.296    0.343    0.343
# 11 confidence =~         understanding_sciprocess     2     2     1  3.830  0.291   0.291    0.446    0.446
# 12    process =~      understanding_fertilization     1     1     1  5.865  0.238   0.238    0.238    0.238
# 13   identity =~                skills_withothers     1     1     1  8.330  0.234   0.234    0.234    0.234
# 14    process =~      understanding_fertilization     2     2     1  1.927  0.193   0.193    0.284    0.284
# 15 confidence =~                    skills_evalH0     2     2     1  1.096  0.157   0.157    0.164    0.164
# 16    process =~    integration_applyingknowledge     1     1     1  3.434  0.149   0.149    0.149    0.149
# 17   identity =~         understanding_sciprocess     2     2     1  0.992  0.129   0.129    0.197    0.197
# 18 confidence =~                 attitudes_career     2     2     1  0.864  0.117   0.117    0.111    0.111
# 19 confidence =~                skills_withothers     1     1     1  1.096  0.105   0.105    0.105    0.105

partial.strict <- measEq.syntax(configural.model = model_3,
                        data = dat_collapse_responses,
                        ordered = TRUE, # ie all variables are ordinal
                        parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                        ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                        ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                        group = "test", # column name defining groups
                        group.equal = c("thresholds", "loadings", "lv.variances"),
                        group.partial = c("identity =~ attitudes_confidentunderstanding"))

# Fit partial.strict invariance model
model.partial.strict <- as.character(partial.strict)
fit.partial.strict <- cfa(model.partial.strict, data = dat_collapse_responses, group = "test", ordered = TRUE)

partial.strict.indices <- modindices(fit.partial.strict, free.remove = FALSE)
partial.strict.indices[partial.strict.indices$op == "=~",] %>%
  arrange(desc(epc))

lavTestLRT(fit.baseline, fit.prop4, fit.prop7, fit.partial.strict)
anova(fit.prop7, fit.partial.strict)
###############################################################################################



# ONLY PROCEED WITH TESTS BELOW AFTER MEASUREMENT INVARIANCE IS ESTABLISHED
######################################################################################################
# STEP 3: Calculate means in SALG responses for the three latent factors pre vs post and do T-test for significance

# CHECK IF THIS IS NECESSARY
# After establishing invariance, then your next step is to constrain the latent means across groups and conduct a chi-sq difference test to see whether that set of constraints is plausible.
# To constrain all the latent means (the omnibus test), you can just add "means" to the vector of parameters to constrain in the group.equal argument.  

compare <- measEq.syntax(configural.model = model_3,
                       data = dat_collapse_responses,
                       ordered = TRUE, # ie all variables are ordinal
                       parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                       ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                       ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                       group = "test", # column name defining groups
                       group.equal = c("thresholds", "loadings", "means"))

model.compare <- as.character(compare)
fit.compare <- cfa(model.compare, data = dat_collapse_responses, group = "test", ordered = TRUE, meanstructure = TRUE)
summary(fit.compare) 

# Does this mean there are significant differences between the mean (in at least one of the three latent factors)?
lavTestLRT(fit.prop7, model.compare) 

# FIX IT - store results in a holder data.frame
round(data.matrix(fitmeasures(fit.compare, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

# To constrain one at a time, you can either add parameters to the group.partial argument, or you can leave "means" out of the group.equal argument and manual constrain a latent mean using labels in the model syntax (e.g., "com ~ c(mean1, mean1)*com" )
# Need to test the three latent factors separately: process, identity, confidence?
# compare_process <- measEq.syntax(configural.model = model_3,
#                              data = dat_collapse_responses,
#                              ordered = TRUE, # ie all variables are ordinal
#                              parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
#                              ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
#                              ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
#                              group = "test", # column name defining groups
#                              group.equal = c("thresholds", "loadings", "means"))



