# Do preliminary analysis (correlation and scree plots) and exploratory factor analysis
# kdgorospe@gmail.com

### NOTE: Focus on EFA results on the pre-data, plan is to use this as the "confirmed" model
### Then use CFA on the pre and post in a factorial invariance framework to test for changes between the two time points
### Screeplot on the pre-data (after filtering out variables that fail normality test) suggests there are three latent factors

library(psych)
library(corrplot)
library(GPArotation) # needed for promax rotation in fa function

### REFERENCE: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
# Arrange data into tidy format (each row is a single student)
tidy_dat_all <- coded_and_standardized_dat %>%
  select(Number, pooled_answer, year, test, concept) %>%
  pivot_wider(values_from = pooled_answer, names_from = concept, id_cols = c(Number, year, test)) %>% # use values_fn = list(val = length) to identify coding duplicates
  arrange(year, test, Number) %>%
  ungroup() 

# Create subsets of pre and post datasets - filter both subsets to the same questions, so they are comparable
tidy_dat_pre <- tidy_dat_all %>%
  filter(test=="pre") %>%
  # REMOVE ALL EVALUATION QUESTIONS (only asked in the post test, and not part of our pre vs post framework)
  select(!starts_with("evaluation")) %>% # note: for some reason starts_with("evaluation") == FALSE does not work (not tidy?)
  # REMOVE VARIABLES WITH LIMITED PAIRWISE COMPARISONS (these labs were run only a few times)
  select(-c(understanding_ethology, understanding_aquaculture, understanding_coralclimate, understanding_coralskel, understanding_diversity)) 

tidy_dat_post <- tidy_dat_all %>%
  filter(test=="post") %>% 
  # REMOVE ALL EVALUATION QUESTIONS (only asked in the post test, and not part of our pre vs post framework)
  select(!starts_with("evaluation")) %>% 
  # REMOVE VARIABLES WITH LIMITED PAIRWISE COMPARISONS (these labs were run only a few times)
  select(-c(understanding_ethology, understanding_aquaculture, understanding_coralclimate, understanding_coralskel, understanding_diversity)) 
  
  
###################################################################################################################
# Calculate and plot correlations
# NOTE ON CORRELATION RESULTS: Scaling vs not scaling the variables does not change results of correlation matrix

time_point <- c("pre", "post")
for (i in time_point){
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
  cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")
  cor_csv <- paste("cormatrix_", i, ".csv", sep="")
  write.csv(cor_matrix, cor_csv, row.names = TRUE)
  # drive_upload(cor_csv, path = as_dribble("REMS_SALG/")) # for initial upload
  
  # NOTE: option use = "pairwise.complete.obs" computes correlations for each pair of columns using vectors formed by omitting rows with missing values on a pairwise basis
  # Caveat: this means each column vector varies depending on its pairing - and for small datasets can lead to weird results
  # See: http://bwlewis.github.io/covar/missing.html
  
  # Calculate significance tests
  p_95 <- cor.mtest(cor_dat, conf.level = 0.95)
  p_csv <- paste("cor_pmatrix_", i, ".csv", sep = "")
  write.csv(p_95$p, p_csv, row.names = TRUE)
  # drive_upload(p_csv, path = as_dribble("REMS_SALG/")) # for initial upload
  
  
  cor_pdf_plain <- paste("corrplot_", i, "_plain.pdf", sep="")
  pdf(cor_pdf_plain)
  corrplot(cor_matrix, tl.col="black", tl.cex=0.75) # insig = "label_sig" means label the significant p-values; otherwise option is to label the insignificant values 
  dev.off()
  #drive_upload(cor_pdf_plain, path = as_dribble("REMS_SALG/")) # for initial upload
  
  cor_pdf_x_insig <- paste("corrplot_", i, "_x_insig.pdf", sep="")
  pdf(cor_pdf_x_insig)
  corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, tl.col = "black", tl.cex = 0.75, insig = "blank")
  dev.off()
  
  # ALTERNATIVELY, place an "X" wherever values are insiginficant
  #corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, pch.cex = 0.8, tl.col="black", tl.cex=0.75) 
  
  #drive_upload(cor_pdf_x_insig, path = as_dribble("REMS_SALG/")) # for initial upload

  
  if (i=="pre"){ 
    drive_update(file = as_id("1mDIq5JfDSuiZIAehyy6UZX37l-5cgU-t36pM87Iix8s"), media = cor_csv)
    drive_update(file = as_id("1FMLHbE7sbnLbqHm6eeiWtU-O_xVks66-"), media = cor_pdf_plain)
    drive_update(file = as_id("1p0JbznqjciYAZlI4DEJ9SxOYQtlXoKEM"), media = cor_pdf_x_insig)
    drive_update(file = as_id("1UPWKsR3ijvEPRHN4JgbBc-CMhzKsfdtd"), media = p_csv)
  }
  if (i=="post"){ 
    drive_update(file = as_id("1yNpDJoBgd_taaJ8eO1klt4mKwEbEoj5b2UhTfsbpXYM"), media = cor_csv)
    drive_update(file = as_id("10qsyqLTg4GqHFrGIARd6m1wconWu3jpT"), media = cor_pdf_plain)
    drive_update(file = as_id("1CZHbdvW1UF6Lc-lwhXfDxjsgPa4FalqL"), media = cor_pdf_x_insig)
    drive_update(file = as_id("1QgpTdt7NaBWE6qdHdnhmb25fOnyAw6-B"), media = p_csv)
  }
  file.remove(cor_csv)
  file.remove(cor_pdf_plain)
  file.remove(cor_pdf_x_insig)
  file.remove(p_csv)
  
}

###################################################################################################################
# TEST FOR NORMALITY
## Note: scale() function only shifts data to mean = 0 and standard deviation = 1; i.e., does nothing to affect how normally distributed it is; keep raw data so we see results in terms of the Likert scale (e.g., mean response for each variable)
## From Godwin 2013 for EFA: The skew and kurtosis were evaluated for each item to ensure that the assumptions of multivariate normality were not severely violated 
## Do this separately for pre and post
## Use describe() to examine normality of each variable ()
## caution: describe() shares namespace with other packages (e.g., Hmisc); use psych::describe to specify

time_point <- c("pre", "post")
for (i in time_point){
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  
  dat_describe <- psych::describe(tidy_dat[,4:dim(tidy_dat)[2]])
  
  
  # According to Goodwin 2016, these variables should be removed because they violate assumption of normality:
  skew_fail <- row.names(dat_describe[abs(dat_describe$skew) >= 2,])
  if (length(skew_fail) > 0){
    skew_list <- paste(row.names(dat_describe[abs(dat_describe$skew) >= 2,]), collapse = ", ")
    message <- paste(i, "dataset variables that fail skewness test:\n", skew_list, "\n")
    cat(message) # unlike print(), cat will print \n as newline in console
  }
  
  kurtosis_fail <- row.names(dat_describe[abs(dat_describe$kurtosis) >= 7,])
  if (length(kurtosis_fail) > 0){
    kurtosis_list <- paste(row.names(dat_describe[abs(dat_describe$kurtosis) >= 7,]), collapse = ", ")
    message <- paste(i, "dataset variables that fail kurtosis test:\n", kurtosis_list, "\n")
    cat(message)
  }

  describe_csv <- paste("dat_describe_", i, ".csv", sep = "")
  write.csv(dat_describe, describe_csv, row.names = TRUE) # write out row.names because these list the different "questions"
  #drive_upload(describe_csv, path = as_dribble("REMS_SALG/")) # for initial upload
  # FILE ID for dat_describe_pre.csv: 1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ
  if (i == "pre"){
    drive_update(file = as_id("1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ"), media = describe_csv)
  }
  # FILE ID for dat_describe_post.csv: 1OcP2sO6kgu6-W0wxYVnLFzrbHjcJxVY5
  if (i == "post"){
    drive_update(file = as_id("1OcP2sO6kgu6-W0wxYVnLFzrbHjcJxVY5"), media = describe_csv)
  }
  file.remove(describe_csv)
  
}

###################################################################################################################
# Finalize variable list for further analysis:

# NOTES on TEST FOR NORMALITY: 
# in pre data: "major_unsurecollege_yesno" violates skewness test for normality
# in post data: "attitudes_workwithothers", "major_unsurecollege_yesno", "understanding_relatetolife", and "understanding_society_pooled" violate skewness

# MOVING FORWARD: 
# remove all binary responses from analysis (i.e., major_marinesci_yesno, major_notscience_yesno, etc) - EFA should be done on continuous or continuous-like data (e.g., Likert scale) but not binary data
# Keep the following variables because they only failed the skewness test for the post data (attitudes_workwithothers, understanding_relatetolife, understanding_society_pooled)

# FIX IT: Add to end of analysis: Compare binary responses pre vs post with t-tests

# FILTER THESE OUT FOR NOW:

variables_to_filter <- c("major_marinesci_yesno", "major_notscience_yesno", "major_science_yesno", "major_undecided_yesno", "major_unsurecollege_yesno")
# Also can try removing questions that were split and then pooled in different years
# variables_to_filter <- c("major_marinesci_yesno", "major_notscience_yesno", "major_science_yesno", "major_undecided_yesno", "major_unsurecollege_yesno", 
#                          "skills_communicate_pooled", "understanding_oceanacid_pooled", "understanding_society_pooled", "understanding_sound_pooled")

tidy_dat_pre_final <- tidy_dat_pre %>%
  select(-(all_of(variables_to_filter))) # Use all_of to fix ambiguity of selecting columns, see: https://tidyselect.r-lib.org/reference/faq-external-vector.html
tidy_dat_post_final <- tidy_dat_post %>%
  select(-(all_of(variables_to_filter)))

# Additional EFA data inspection tests:

# 1 - Bartlett's test of sphericity - tests whether observed correlation matrix is an identity matrix;
# Tests whether or not the correlation matrix exhibits any relationships or if has a complete lack of relationships (i.e., the identity matrix)
# Just perform this on the pre data since that's the only being used in the EFA

pre_cor_matrix <- cor(tidy_dat_pre_final[,-c(1:3)], use="pairwise.complete.obs")

# RESULTS: Test shows results are significantly different from the identity matrix
cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE) 

# SAVE RESULTS:
bartlett_name <- "pre_cor_matrix_Bartlett's_test.txt"
sink(bartlett_name)
print(cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE))
sink()

drive_upload(bartlett_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Only use drive_update for files that we want to be overwritten
#drive_update(file = as_id("1BYZtJo2aHaYPeqQdcgXDkWp1QknHo-ja"), media = bartlett_name)  
file.remove(bartlett_name)

# 2 - Kaiser, Meyer, Olkin measure of Sampling Adequacy

KMO(r = pre_cor_matrix)
# RESULS: Test shows all variables are above cutoff of 0.6
# This indicates that common variance (and thus latent factors) are present in the data

# SAVE RESULTS:
KMO_name <- "pre_cor_matrix_KMO_test.txt"
sink(KMO_name)
print(KMO(r = pre_cor_matrix))
sink()

drive_upload(KMO_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
#drive_update(file = as_id("10A_EWtLwvLIm8MvhSbFLreR5MVp8zFUE"), media = KMO_name)  
file.remove(KMO_name)

###################################################################################################################
# # SCREE PLOTS (version 1: retaining all variables; using unfiltered tidy_dat_pre and tidy_dat_post)
# # RESULT: use 6 factors for PRE data; use 3 factors for POST data
# # At this point, can go ahead and center and scale data using scale() function
# 
# time_point <- c("pre", "post")
# for (i in time_point){
#   
#   tidy_dat <- get(paste("tidy_dat_", i, sep=""))
#   
#   dat_scaled <- scale(tidy_dat[,4:dim(tidy_dat)[2]], center=TRUE, scale=TRUE)
#   
#   # SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
#   # FIX IT - in addition to scree plot, do parallel analysis?
#   # See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
#   cor_matrix <- cor(dat_scaled, use = "pairwise.complete.obs")
#   
#   # Print to console:
#   cat("For", i, "data:\n")
#   
#   scree_name <- paste("screeplot_", i, ".pdf", sep = "")
#   pdf(file = scree_name)
#   fa.parallel(x = cor_matrix, fm = "ml", fa = "fa", n.obs = nrow(dat_scaled)) # "ml" is the maximum likelihood method for "well-behaved" data
#   dev.off()
#   #drive_upload(scree_name, path = as_dribble("REMS_SALG/")) # for initial upload
#   
#   if (i == "pre"){
#     drive_update(file = as_id("1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT"), media = scree_name)  
#   }
#   if (i == "post"){
#     drive_update(file = as_id("1GXAaVmQoO5tjxb6PNISzwsxFz3FM8JH5"), media = scree_name)  
#   }
#   
#   file.remove(scree_name)
#   
# }

###################################################################################################################
# SCREE PLOTS (version 2: after filtering problematic variables; using tidy_dat_pre_final and tidy_dat_post_final)
# RESULT: use 3 factors for PRE data; use 2 factors for POST data
# Although code below does this for PRE and POST, only interested in PRE since this is what's used for the EFA

# NOTES on SCREE PLOTS: (from Howard 2016)
# plots each eigenvalue on a graph and determine when decreases in successive eigenvalues start to asymptote
# only include the number of factors up until the asymptote; these represent common variance better than the factors after the asymptote

time_point <- c("pre", "post")
for (i in time_point){
  
  tidy_dat <- get(paste("tidy_dat_", i, "_final", sep=""))
  
  dat_scaled <- scale(tidy_dat[,4:dim(tidy_dat)[2]], center=TRUE, scale=TRUE)
  
  # SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
  # FIX IT - in addition to scree plot, do parallel analysis?
  # See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
  cor_matrix <- cor(dat_scaled, use = "pairwise.complete.obs")
  
  # Print to console:
  cat("For", i, "data:\n")
  
  scree_name <- paste("screeplot_", i, "_final.pdf", sep = "")
  pdf(file = scree_name)
  fa.parallel(x = cor_matrix, fm = "ml", fa = "fa", n.obs = nrow(dat_scaled)) # "ml" is the maximum likelihood method for "well-behaved" data
  dev.off()
  drive_upload(scree_name, path = as_dribble("REMS_SALG/Results")) # for initial upload

  # Only use drive_update if we want files to be overwritten
  # if (i == "pre"){
  #   drive_update(file = as_id("1MwShJ2OoAf-DGWNIT7ntxaaHN1xl8hTC"), media = scree_name)
  # }
  # if (i == "post"){
  #   drive_update(file = as_id("1ZZPEDbvscidbT9L4xqIIna4XlkZPbKYj"), media = scree_name)
  # }
  
  file.remove(scree_name)
  
}

###################################################################################################################
# FACTOR ANALYSIS (version 1: retaining all variables; using unfiltered tidy_dat_pre and tidy_dat_post)
# Uses nfactors 3 and nfactors 6 based on screeplots of unfiltered data

# NOTE: set nfactors to results from Scree plot
# NOTE: rotate = "promax" is what Goodwin 2016 used
# NOTE: fm (factor method, aka factor extraction method): 
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal).
# 
# time_point <- c("pre", "post")
# for (i in time_point){
#   tidy_dat <- get(paste("tidy_dat_", i, sep=""))
#   cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
#   cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")
#   
#   for (f in c(3, 6)){ # Set numbers of factors here; IMPORTANT: if decide to change number of factors here, remember conditionals for drive_update below
#     EFA_results <- fa(r = cor_matrix, nfactors = f, rotate = "promax", fm = "ml") 
#     
#     # Write model outputs to textfile
#     efa_file <- paste("EFA_", i, "_", f, "factors.txt", sep = "")
#     sink(efa_file)
#     print(EFA_results)
#     sink()
#     
#     # Simplify model outputs: Write just factor loadings to textfile:
#     #efa_loadings_file <- paste("EFA_", i, "_", f, "factors_loadingsONLY.txt", sep = "")
#     #sink(efa_loadings_file)
#     #print(EFA_results$loadings)
#     #sink()
#     
#     # Simplify model outputs: Write factor loadings to csv file
#     efa_loadings_csv <- paste("EFA_", i, "_", f, "factors_loadingsONLY.csv", sep = "")
#     EFA_loadings_only <- EFA_results$loadings[1:nrow(EFA_results$loadings), 1:ncol(EFA_results$loadings)]
#     write.csv(EFA_loadings_only, file = efa_loadings_csv)
# 
#     #drive_upload(efa_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
#     #drive_upload(efa_loadings_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
#     #drive_upload(efa_loadings_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
#     if (i == "pre" & f == 3){
#       drive_update(file = as_id("1KmYFQYaNqNxtKCfkqPf9oEOzipse7WLi"), media = efa_file)  
#       drive_update(file = as_id("1sxk_70W4MZnYG4LoOq0dbwOJ0-gaKiEB"), media = efa_loadings_csv) 
#     }
#     if (i == "post" & f == 3){
#       drive_update(file = as_id("1WtknxTkcCc1dcBrOAd0VzvzsrbLQtef1"), media = efa_file)  
#       drive_update(file = as_id("1AAn-gdBbpQyfthKemDLuUinha1PM3h7y"), media = efa_loadings_csv) 
#     }
#     if (i == "pre" & f == 6){
#       drive_update(file = as_id("1o-BnY6VDmBCO9AH5F-ajy7nrFDFYH9rK"), media = efa_file)  
#       drive_update(file = as_id("1mtbVNt6eoimoRFeNrKuko7CbRzpqtb0g"), media = efa_loadings_csv) 
#     }
#     if (i == "post" & f == 6){
#       drive_update(file = as_id("18QF8at0WBq0yIjwCX44KKknDpZnNqufU"), media = efa_file)  
#       drive_update(file = as_id("1FsrAdaf_ukXs391sHC4q-kzVBf1CTVVy"), media = efa_loadings_csv) 
#     }
#     file.remove(efa_file)
#     file.remove(efa_loadings_csv)
#   } # END loop through different number of factors
# 
#   # To output how much variance is accounted for by the factors:
#   # Or just inspect the table of "Cumulative Var"
#   #EFA_results$Vaccounted
#   #EFA_results$Vaccounted[3, f] # i.e., last column of the "cumulative variance" row
#   
#   # Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#   #EFA_rawdat_results <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#   #EFA_rawdat_results$scores
#   
# } # END loop through pre and post

###################################################################################################################
# FACTOR ANALYSIS (version 2: after filtering problematic variables; using tidy_dat_pre_final and tidy_dat_post_final)
# Uses nfactors 2, 3, and 4 based on screeplot of final (i.e., filtered) data
# Note: script produces results for pre and post data, but only the pre results are uploaded to Google Drive since this is the only one we care about for EFA

# set nfactors to results from Scree plot
# rotate = "promax" what Goodwin 2016 used:
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal)

time_point <- c("pre", "post")
for (i in time_point){
  
  tidy_dat <- get(paste("tidy_dat_", i, "_final", sep=""))
  cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
  cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")
  
  for (f in c(5, 6, 7)){ # Set numbers of factors here; IMPORTANT: if decide to change number of factors here, remember conditionals for drive_update below
    EFA_results <- fa(r = cor_matrix, nfactors = f, rotate = "promax", fm = "ml") 
    
    # Write model outputs to textfile
    efa_file <- paste("EFA_", i, "_final_", f, "factors.txt", sep = "")
    sink(efa_file)
    print(EFA_results)
    sink()
    
    # Simplify model outputs: Write just factor loadings to textfile:
    #efa_loadings_file <- paste("EFA_", i, "_final_", f, "factors_loadingsONLY.txt", sep = "")
    #sink(efa_loadings_file)
    #print(EFA_results$loadings)
    #sink()
    
    # Simplify model outputs: Write factor loadings to csv file
    efa_loadings_csv <- paste("EFA_", i, "_final_", f, "factors_loadingsONLY.csv", sep = "")
    EFA_loadings_only <- EFA_results$loadings[1:nrow(EFA_results$loadings), 1:ncol(EFA_results$loadings)]
    write.csv(EFA_loadings_only, file = efa_loadings_csv)
    
    drive_upload(efa_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
    drive_upload(efa_loadings_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
    
    # Only use drive_update to overwrite files
    # if (i == "pre" & f == 5){
    #   drive_update(file = as_id("1M48aiuwL1cTeSdDUn4y6zpCa41xLyA7s"), media = efa_file)  # https://drive.google.com/file/d/1M48aiuwL1cTeSdDUn4y6zpCa41xLyA7s/view?usp=sharing
    #   drive_update(file = as_id("11IG2BAaRi4FtKDDaBcbDXY8UFp_D5e4x"), media = efa_loadings_csv) # https://drive.google.com/file/d/11IG2BAaRi4FtKDDaBcbDXY8UFp_D5e4x/view?usp=sharing
    # }
    # if (i == "pre" & f == 6){
    #   drive_update(file = as_id("18mTQMQrcj0ta87XUOkxIO5QCh5K6M0-x"), media = efa_file)  # https://drive.google.com/file/d/18mTQMQrcj0ta87XUOkxIO5QCh5K6M0-x/view?usp=sharing
    #   drive_update(file = as_id("1sXz29Qvy8k854Nvl_raFOonNCTZRl6om"), media = efa_loadings_csv) # https://drive.google.com/file/d/1sXz29Qvy8k854Nvl_raFOonNCTZRl6om/view?usp=sharing
    # }
    # if (i == "pre" & f == 7){
    #   drive_update(file = as_id("130dXp_ar8pzPxJ8DJfnkWZpGEp8dw6Dc"), media = efa_file) # https://drive.google.com/file/d/130dXp_ar8pzPxJ8DJfnkWZpGEp8dw6Dc/view?usp=sharing
    #   drive_update(file = as_id("1i8vanJc2h5jh1mz40m1JwNGBRaOFOP1q"), media = efa_loadings_csv) # https://drive.google.com/file/d/1i8vanJc2h5jh1mz40m1JwNGBRaOFOP1q/view?usp=sharing
    # }
    
    file.remove(efa_file)
    file.remove(efa_loadings_csv)
    
  } # END loop through different number of factors
  
} # END loop through pre and post


# To output how much variance is accounted for by the factors:
# Or just inspect the table of "Cumulative Var"
#EFA_results$Vaccounted
#EFA_results$Vaccounted[3, f] # i.e., last column of the "cumulative variance" row

# Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#EFA_rawdat_results <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#EFA_rawdat_results$scores

