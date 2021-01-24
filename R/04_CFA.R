# Do confirmatory factor analysis of pre vs. post using the results from EFA on pre-data as the confirmed model
# kdgorospe@gmail.com
# See: HIRSCHFELD and VON BRACHEL 2014 in "Resources" folder

library(lavaan)
library(semPlot)
library(semTools)
# Use lavaan to do CFA on pre vs post
# Run the model on pre-data to see if there is adequate fit
# Run the model on post-data to see if there is a decrease in fit

######################################################################################################
# RETAINING ALL VARIABLES (including cross-loading variables)
model_5 <- '
xi_1 =~ attitudes_confidentresearch + skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + skills_withothers + understanding_sciprocess
xi_2 =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sciprocess + understanding_sound_pooled
xi_3 =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic + skills_withothers
xi_4 =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
xi_5 =~ understanding_relatetolife + understanding_society_pooled
 '

######################################################################################################
# MEASUREMENT INVARIANCE FRAMEWORK FOR CONTINUOUS VARIABLES:

# FIX IT - before doing config, inspect overall model fit on PRE EFA?

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

# First test the baseline configuration model (the pre data EFA model), then test for weak, strong, strict measurement invariance
config <- lavaan::cfa(model_5, data = tidy_dat_all, group = "test")

# See "RMSEA" in summary output for measure for model fit
summary(config, standardized = TRUE, fit.measures = TRUE)
moreFitIndices(config)

# For graphical representations of the model:
semPaths(config, "std")

weak <- lavaan::cfa(model_5, data = tidy_dat_all, group = "test", group.equal = "loadings")
strong <- lavaan::cfa(model_5, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts"))
strict <- lavaan::cfa(model_5, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts", "residuals"))
anova(config, weak, strong, strict)

measurementInvariance(model = model_5, data = tidy_dat_all, group = "test")

######################################################################################################
# MEASUREMENT INVARIANCE FRAMEWORK FOR ORDINAL VARIABLES: may be more appropriate since these are Likert scale data

ordinal_model_fit <- lavaan::cfa(model_5, 
                                 ordered = c("attitudes_career", "attitudes_confidentresearch", "attitudes_confidentunderstanding",
                                                      "attitudes_discussing", "attitudes_enthusiastic", "attitudes_workwithothers", 
                                                      "integration_applyingknowledge", "integration_connectingknowledge", "skills_communicate_pooled",
                                                      "skills_developH0", "skills_evalH0", "skills_testH0", "skills_withothers", "understanding_ecology",
                                                      "understanding_fertilization", "understanding_oceanacid_pooled", "understanding_relatetolife",
                                                      "understanding_sciprocess", "understanding_society_pooled", "understanding_sound_pooled"),
                                 data = tidy_dat_all)

######################################################################################################
# Consider repeating analysis after removing cross-loading variables?

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
