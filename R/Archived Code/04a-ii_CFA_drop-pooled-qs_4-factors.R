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

# Restarting point
load("2021-02-13_all-data-prior-to-CFA_pooled-vars-removed.RData") 
# REMINDER: dropping pooled variables because when analyzing these as ORDINAL variables, categories must be present in both the pre and post groups
# Pooled variables range from 1 to 6 in increments of 0.5 because responses were averaged
# Example: compare tidy_dat_all %>% filter(test == "pre") %>% select(understanding_sound_pooled) %>% table() versus
# tidy_dat_all %>% filter(test == "post") %>% select(understanding_sound_pooled) %>% table()
# Requires complex collapsing of responses

# Old dataset where pooled variables are retained
# load("2021-02-13_all-data-prior-to-CFA_all-vars.RData") 
######################################################################################################
# Since EFA was done on just the PRE data, possible to make sure that model wasn't overfitting by doing a CFA on just the POST data
# Can use this test different models for variables that were cross loading onto multiple factors

# Define model:
# Only allow one item to load onto each latent variable (assumption with CFA is that each items loads equally)
# See EFA Results for FOUR factor loadings: https://docs.google.com/spreadsheets/d/1OUFQo1BtBokK45lmwFNyCxteK-cMJzVh_cLG1l1HwCo/edit#gid=1649019614
# model_4a: attitudes_confidentunderstanding loads onto "knowledge"
model_4a <- '
hypothesis =~ skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_sciprocess
integration =~ attitudes_workwithothers + integration_applyingknowledge + integration_connectingknowledge
identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
knowledge =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
 '

# model_4b: attitudes_confidentunderstanding loads onto "identity" (PROBLEM here is that now knowledge only has two items)
model_4b <- '
hypothesis =~ skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_sciprocess
integration =~ attitudes_workwithothers + integration_applyingknowledge + integration_connectingknowledge
identity =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic 
knowledge =~ understanding_ecology + understanding_fertilization
 '

# FIX IT should this be parameterization = "delta" (same as measurement invariance?) or "theta"
fit_4a <- cfa(model_4a, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
fit_4b <- cfa(model_4b, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
#summary(fit_4a) # Results of baseline model
#summary(fit_4b) # Results of baseline model

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post.results <- matrix(NA, nrow = 2, ncol = 6)
colnames(post.results) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(post.results) <- c("model4a", "model4b")
post.results[1,] <- round(data.matrix(fitmeasures(fit_4a, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
post.results[2,] <- round(data.matrix(fitmeasures(fit_4b, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)


# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is > 0.05 (not a good fit)

# LEFT OFF HERE: how to determine based on test statistics which of the two models are a better fit: model 4a or 4b?


######################################################################################################
# MEASUREMENT INVARIANCE ANALYSIS OF ORDINAL VARIABLES following Svetina et al. 2019

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

# BEFORE PASSING TO ORDINAL FRAMEWORK - NEED TO COLLAPSE RESPONSES THAT ARE NOT PRESENT IN ONE GROUP VS THE OTHER
# Example: table(tidy_dat_all %>% filter(test == "pre") %>% select(attitudes_confidentresearch))
# vs: table(tidy_dat_all %>% filter(test == "post") %>% select(attitudes_confidentresearch))
# i.e., need to collapse the "2" response in the pre-test to be "3"
# Otherwise, will get error message: lavaan ERROR: some categories of variable `attitudes_confidentresearch' are empty in group 2
dat_collapse_responses <- tidy_dat_all %>%
  mutate(attitudes_confidentresearch = if_else(attitudes_confidentresearch == 2, true = 3, false = attitudes_confidentresearch),
         skills_developH0 = if_else(skills_developH0 == 2, true = 3, false = skills_developH0),
         skills_evalH0 = if_else(skills_evalH0 == 2, true = 3, false = skills_evalH0),
         skills_testH0 = if_else(skills_testH0 == 2, true = 3, false = skills_testH0),
         understanding_sciprocess = if_else(understanding_sciprocess %in% c(1, 2, 3), true = 4, false = understanding_sciprocess),
         attitudes_career = if_else(attitudes_career == 1, true = 2, false = attitudes_career),
         attitudes_discussing = if_else(attitudes_discussing %in% c(2, 3), true = 4, false = attitudes_discussing),
         attitudes_confidentunderstanding = if_else(attitudes_confidentunderstanding == 2, true = 3, false = attitudes_confidentunderstanding),
         understanding_ecology = if_else(understanding_ecology %in% c(1, 2, 3), true = 4, false = understanding_ecology),
         understanding_fertilization = if_else(understanding_fertilization %in% c(1, 2), true = 3, false = understanding_fertilization))

# Create matrix for storing results (6 fit indices across three different models)
all.results <- matrix(NA, nrow = 3, ncol = 6)
colnames(all.results) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(all.results) <- c("baseline", "prop4", "prop7")

######################################################################################################
# Specify BASELINE model: no constraints across groups or repeated measures
# See ?measEq.syntax (new function in semTools, replaces measurementInvariance functions)
# Following Svetina et al 2019 (which implements Wu and Estabrook 2016) - Measurement invariance for categorical variables

baseline <- measEq.syntax(configural.model = model_5,
                          data = dat_collapse_responses,
                          ordered = TRUE, # ie all variables are ordinal
                          parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                          ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                          ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                          group = "test", # column name defining groups
                          group.equal = "configural")

# orientation to the model:
summary(baseline)

# to see list of all constraints in the model:
cat(as.character(baseline))

# Fit baseline model
# Warning messages about eigenvalues that are close to zero can be ignored: https://groups.google.com/g/lavaan/c/4y5pmqRz4nk
# specify as.character to submit to lavaan
model.baseline <- as.character(baseline)
fit.baseline <- cfa(model.baseline, data = dat_collapse_responses, group = "test", ordered = TRUE)
#summary(fit.baseline) # Results of baseline model

# Extract just the fit indices:
all.results[1,] <- round(data.matrix(fitmeasures(fit.baseline, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

######################################################################################################
# THRESHOLD INVARIANCE MODEL (aka "Proposition 4" in Wu and Estabrook's 2016)
# Threshold are the gaps that separate the different Likert scale categories (e.g., Somewhat agree vs Strongly agree)

prop4 <- measEq.syntax(configural.model = model_5,
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

# Extract just the fit indices
all.results[2,] <- round(data.matrix(fitmeasures(fit.prop4, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

# Use chi-square test to test for difference in model fit between baseline model and model with threshold equality constraints
# p > 0.05 means no difference between the two models fits, despite higher constraints in the latter
lavTestLRT(fit.baseline, fit.prop4) # p > 0.05 means no difference between the two models, despite higher constraints in the latter

######################################################################################################
# THRESHOLD AND LOADING INVARIANCE MODEL (aka "Proposition 7" in Wu and Estabrook's 2016)
prop7 <- measEq.syntax(configural.model = model_5,
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

# Extract just the fit indices
all.results[3,] <- round(data.matrix(fitmeasures(fit.prop7, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

# Use chi-square test to test for difference in model fit between model with threshold equality constraints and model with threshold AND loading equality constraints
# p > 0.05 means no difference between the two models fits, despite higher constraints in the latter
lavTestLRT(fit.prop4, fit.prop7) 

all.results
# Interpreting fit indices for INDIVIDUAL models, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# Low chi square means better model fit
# Since chi-sq is dependent on sample size, one way useful benchmark is that chi-sq / df < 5 means good model fit
# Chi-square p-value of model fit are all significant
# RMSEA is > 0.05 (not a good fit)
# Comparative fit index CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RESULTS: Model fits for individual models are all good except for RMSEA

# Interpreting fit indices ACROSS models
# CFI should decrease (model fit should decrease) with more constrained models 
# Convention is Delta CFI < 0.01 (decreased fit, but not too much) indicates measurement invariance (in our case, we actually see slight increases in CFI - i.e., negative Delta CFI which is allowed)
# Convention is that Delta RMSEA < 0.015 (decreased fit, but not too much) indicates measurement invariance
# RESULTS: Comparing baseline to prop4 did not show a significant decrease in model fit, so ordinal variable thresholds are equivalent
# Comparing prop4 to prop7 did not show a significant decrease in model fit, so variable loadings onto latent factors are equivalent
# CONCLUSION - SALG is measurement invariant, and it is acceptable to compare means in latent variables


# Return to fit.prop7 framework, since we are able to establish invariance at this highest level: summary(fit.prop7)
# My understanding is that the values we are interested in are the "Intercepts" which are the means of each variable (i.e., question response)
# These are set to "zero" in gorup 1
# So the values under group 2 represent how much of an increase was observed between pre vs post. 

######################################################################################################

# LEFT OFF HERE:
# NEXT STEP - Compare means in latent variables between pre and post? Or I think we can just stop by showing 

# One potential route forward is to test for differences in the means as part of the measurement invariance framework:
# i.e., add "means" to the list of groups.equal constraints - and by process of elimination determine which factor(s) are causing the difference
# CONCERN: MY UNDERSTANDING IS THAT METHODS ARE SO UNDEVELOPED/NON-STANDARDIZED THAT I WON'T KNOW WHAT IS ACCEPTABLE

# After establishing invariance, then your next step is to constrain the latent means across groups and conduct a chi-sq difference test to see whether that set of constraints is plausible.
# To constrain all the latent means (the omnibus test), you can just add "means" to the vector of parameters to constrain in the group.equal argument.  
# If that test fails, then there are significant group differences in at least one of the latent means.  
# Instead of constraining all 3 latent means, you can fix one at a time and compare that model to the partial strong invariance model using a chi-sq difference test. 
# To constrain one at a time, you can either add parameters to the group.partial argument, or you can leave "means" out of the group.equal argument and manual constrain a latent mean using labels in the model syntax (e.g., "com ~ c(mean1, mean1)*com" )

compare <- measEq.syntax(configural.model = model_5,
                       data = dat_collapse_responses,
                       ordered = TRUE, # ie all variables are ordinal
                       parameterization = "delta", # recommended by Svetina et al for baseline model specification of ordinal variables
                       ID.cat = "Wu.Estabrook.2016", # method for identifying residual variance for ordinal variables
                       ID.fac = "std.lv", # std.lv = standardize latent variables to have a mean of 0 and a variance of 1 (can now interpret these as CORRELATIONS)
                       group = "test", # column name defining groups
                       group.equal = c("thresholds", "loadings", "means"))

model.compare <- as.character(compare)
fit.compare <- cfa(model.prop7, data = dat_collapse_responses, group = "test", ordered = TRUE, meanstructure = TRUE)
summary(fit.compare) 












######################################################################################################
######################################################################################################
# OLD CODE FOR CONTINUOUS VARIABLES:
# NOTE: after dropping pooled questions, understanding_relatetolife doesn't load strongly to any latent variable - dropped from CFA
model_5 <- '
hypothesis =~ attitudes_confidentresearch + skills_developH0 + skills_evalH0 + skills_testH0 + understanding_sciprocess
identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
practice =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
knowledge =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
peers =~ attitudes_workwithothers + skills_withothers
 '

# Tried only using the largest loading per variable (i.e., remove double loadings for attitudes_confidentresearch and attitudes_confidentunderstanding)
# Note: doesn't change results significantly compared to when allowing multiple loadings per variable
# model_6 <- '
# hypothesis =~ skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + understanding_sciprocess
# marine_sci =~ understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled
# identity =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic
# practice =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
# peers =~ attitudes_workwithothers + skills_withothers
# society =~ understanding_relatetolife + understanding_society_pooled
#  '

# Note: lavaan automatically adds covariances between latent variables automatically

######################################################################################################
# INSPECT BASELINE MODEL

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

fit <- cfa(model_6, data = tidy_dat_all, std.lv = TRUE) 
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)

# option to do multiple imputation of missing data gives similar results, so just keep it simple (no imputation)
# fit <- cfa(model_6, data = tidy_dat_all, std.lv = TRUE, missing = "fiml")

summary(fit, standardized = TRUE, fit.measures = TRUE)
# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is > 0.05 (not a good fit)

# Visualize pathways
semPaths(fit, "std")

# Parameter estiamtes
parameterEstimates(fit, standardized=TRUE)
# also note CORRELATIONS (not covariance, because they are standardized) between latent variables
# for example: parameterEstimates(fit, standardized=TRUE) %>% filter((lhs == "identity" | rhs == "identity") & op == "~~") %>% arrange(desc(est))
# Identity most highly correlated with practicing science, working with peers, and hypothesis-testing; as abilities in one increases, identity also increases (but not causation)

# Question should this initial test be done on the full dataset or just the pre data?
# Doesn't change results significantly so just stick with the full dataset
# fit_pre <- cfa(model_6, data = tidy_dat_pre_final, std.lv = TRUE, missing = "fiml") 
# summary(fit_pre, standardized = TRUE, fit.measures = TRUE)
# semPaths(fit_pre, "std")
# parameterEstimates(fit_pre, standardized=TRUE)


######################################################################################################
# MEASUREMENT INVARIANCE FRAMEWORK FOR CONTINUOUS VARIABLES:
# First test the baseline configuration model, then test for weak, strong, strict measurement invariance
config <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test")
weak <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = "loadings")
strong <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts"))
strict <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts", "residuals"))
anova(config, weak, strong, strict)

measurementInvariance(model = model_6, data = tidy_dat_all, group = "test")

######################################################################################################
# NEXT, REPEAT ANALYSIS FROM ABOVE BUT SPECIFY THAT THESE ARE ORDINAL VARIABLES: may be more appropriate since these are Likert scale data

ordinal_model_fit <- lavaan::cfa(model_5, 
                                 ordered = TRUE,
                                 data = tidy_dat_all, std.lv = TRUE)


summary(ordinal_model_fit, standardized = TRUE, fit.measures = TRUE)
# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is > 0.05 (not a good fit)

# Visualize pathways
semPaths(ordinal_model_fit, "std")

# Parameter estiamtes
parameterEstimates(ordinal_model_fit, standardized=TRUE)
# also note CORRELATIONS (not covariance, because they are standardized) between latent variables
# for example: parameterEstimates(ordinal_model_fit, standardized=TRUE) %>% filter(op == "~~") %>% arrange(desc(est))

# FIX IT - make correlation matrix of just the latent variables


######################################################################################################
# MEASUREMENT INVARIANCE FRAMEWORK FOR ORDINAL VARIABLES:
# First test the baseline configuration model, then test for weak, strong, strict measurement invariance
# The difference is that the comparison between models must be done in pairs

# BEFORE PASSING TO ORDINAL FRAMEWORK - NEED TO COLLAPSE RESPONSES THAT ARE NOT PRESENT IN ONE GROUP VS THE OTHER
# Example: table(tidy_dat_all %>% filter(test == "pre") %>% select(attitudes_confidentresearch))
# vs: table(tidy_dat_all %>% filter(test == "post") %>% select(attitudes_confidentresearch))
# i.e., need to collapse the "2" response in the pre-test to be "3"
dat_collapse_responses <- tidy_dat_all %>%
  mutate(attitudes_confidentresearch = if_else(attitudes_confidentresearch == 2, true = 3, false = attitudes_confidentresearch),
         skills_developH0 = if_else(skills_developH0 == 2, true = 3, false = skills_developH0),
         skills_evalH0 = if_else(skills_evalH0 == 2, true = 3, false = skills_evalH0),
         skills_testH0 = if_else(skills_testH0 == 2, true = 3, false = skills_testH0),
         understanding_sciprocess = if_else(understanding_sciprocess %in% c(1, 2, 3), true = 4, false = understanding_sciprocess),
         attitudes_career = if_else(attitudes_career == 1, true = 2, false = attitudes_career),
         attitudes_discussing = if_else(attitudes_discussing %in% c(2, 3), true = 4, false = attitudes_discussing),
         attitudes_confidentunderstanding = if_else(attitudes_confidentunderstanding == 2, true = 3, false = attitudes_confidentunderstanding),
         understanding_ecology = if_else(understanding_ecology %in% c(1, 2, 3), true = 4, false = understanding_ecology),
         understanding_fertilization = if_else(understanding_fertilization %in% c(1, 2), true = 3, false = understanding_fertilization))

# FIX IT - should these be std.lv = TRUE as in continuous data framework?

ordinal_config <- lavaan::cfa(model_5, ordered = TRUE, data = dat_collapse_responses, group = "test")
ordinal_weak <- lavaan::cfa(model_5, 
                            ordered = TRUE,
                            data = dat_collapse_responses, 
                            group = "test", 
                            group.equal = "loadings")

# More recent guide that also covers measurement invariance: 
# Liu, Y., Millsap, R. E., West, S. G., Tein, J.-Y., Tanaka, R., & Grimm, K. J. (2017). Testing measurement invariance in longitudinal data with ordered-categorical measures. Psychological Methods, 22(3), 486–506. https://doi.org/10.1037/met0000075 
semTools::difftest(ordinal_config,
                    ordinal_weak)


ordinal_strong <- lavaan::cfa(model_5, data = dat_collapse_responses, group = "test", group.equal = c("loadings", "intercepts"))
ordinal_strict <- lavaan::cfa(model_5, data = dat_collapse_responses, group = "test", group.equal = c("loadings", "intercepts", "residuals"))

######################################################################################################
# OLD ANALYSIS WITH DROPPED VARIABLES: (attitudes_workwithothers, understanding_relatetolife, understanding_society_pooled)
# Using the 4-factor model 
# model_4 <- '
# xi_1 =~ skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_sciprocess
# xi_2 =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled + understanding_sciprocess
# xi_3 =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic 
# xi_4 =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
#  '
# # Calculate model fit for pre data and post data: tidy_dat_pre_final vs tidy_dat_post_final
# fit_pre_4 <- lavaan::cfa(model_4, data = tidy_dat_pre_final)
# fit_post_4 <- lavaan::cfa(model_4, data = tidy_dat_post_final)
# 
# fit_pre_4
# fit_post_4
# 
# # Using the 3-factor model 
# model_3 <- '
# xi_1 =~ attitudes_confidentresearch + skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_sciprocess
# xi_2 =~ attitudes_confidentresearch + attitudes_confidentunderstanding + integration_connectingknowledge + understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled + understanding_sciprocess 
# xi_3 =~ attitudes_career + attitudes_confidentresearch + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic + integration_applyingknowledge + integration_connectingknowledge
#  '
# 
# # Calculate model fit for pre data and post data: tidy_dat_pre_final vs tidy_dat_post_final
# fit_pre_3 <- lavaan::cfa(model_3, data = tidy_dat_pre_final)
# fit_post_3 <- lavaan::cfa(model_3, data = tidy_dat_post_final)
# 
# fit_pre_3
# fit_post_3
# 
# ######################################################################################################
# # After removing ALL cross-loading variables: 
# 
# # Using the 4-factor model 
# model_4_simple <- '
# xi_1 =~ skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers
# xi_2 =~ understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled
# xi_3 =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
# xi_4 =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
#  '
# # Calculate model fit for pre data and post data: tidy_dat_pre_final vs tidy_dat_post_final
# fit_pre_4 <- lavaan::cfa(model_4_simple, data = tidy_dat_pre_final)
# fit_post_4 <- lavaan::cfa(model_4_simple, data = tidy_dat_post_final)
# 
# fit_pre_4
# fit_post_4
# 
# # Using the 3-factor model 
# model_3_simple <- '
# xi_1 =~ skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers
# xi_2 =~ understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled
# xi_3 =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic + integration_applyingknowledge
#  '
# 
# # Calculate model fit for pre data and post data: tidy_dat_pre_final vs tidy_dat_post_final
# fit_pre_3 <- lavaan::cfa(model_3_simple, data = tidy_dat_pre_final)
# fit_post_3 <- lavaan::cfa(model_3_simple, data = tidy_dat_post_final)
# 
# fit_pre_3
# fit_post_3


######################################################################################################
# THEN:
# Use SEMtools to test for measurement invariance between pre and post - i.e., demonstrate statistical significance of difference between the two time points
# Student responses that vary between groups are removed
# The data are then re-tested to confirm that there is no longer a difference, confirming that those removed responses are responsible for causing the difference
