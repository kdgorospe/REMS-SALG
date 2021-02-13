# Do confirmatory factor analysis of pre vs. post using the results from EFA on pre-data as the confirmed model
# kdgorospe@gmail.com
# See: HIRSCHFELD and VON BRACHEL 2014 in "Resources" folder

rm(list=ls())

library(lavaan)
library(semPlot)
library(semTools)
library(tidyverse)

# Restarting point
load("2021-02-13_all-data-prior-to-CFA_pooled-vars-removed.RData") 

######################################################################################################
# NOTE: after dropping pooled questions, understanding_relatetolife doesn't load strongly to any latent variable - dropped from CFA
model_5 <- '
hypothesis =~ attitudes_confidentresearch + skills_developH0 + skills_evalH0 + skills_testH0 + understanding_sciprocess
identity =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
practice =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
knowledge =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization
peers =~ attitudes_workwithothers + skills_withothers
 '

# Tried only using the largest loading per variable (i.e., remove double loadings for attitudes_confidentresearch and attitudes_confidentunderstanding)
# Note: doesn't change results significantly
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

#### LEFT OFF HERE - apaprently semTools::difftest no longer being used (Hirschfield 2014 is outdated)
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
