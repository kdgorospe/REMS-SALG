# Do preliminary analysis (correlation and scree plots) and exploratory factor analysis
# kdgorospe@gmail.com

# No need to re-run correlation plots since variables were not dropped yet

###################################################################################################################
# Finalize variable list for further analysis:

# Try removing questions that were split and then pooled in different years
variables_to_filter <- c("major_marinesci_yesno", "major_notscience_yesno", "major_science_yesno", "major_undecided_yesno", "major_unsurecollege_yesno",
                         "skills_communicate_pooled", "understanding_oceanacid_pooled", "understanding_society_pooled", "understanding_sound_pooled")

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
# Use drive_update to update specific file based on ID number
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
# SCREE PLOTS 
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

  # Use drive_update to update specific file based on ID number
  # if (i == "pre"){
  #   drive_update(file = as_id("1MwShJ2OoAf-DGWNIT7ntxaaHN1xl8hTC"), media = scree_name)
  # }
  # if (i == "post"){
  #   drive_update(file = as_id("1ZZPEDbvscidbT9L4xqIIna4XlkZPbKYj"), media = scree_name)
  # }
  
  file.remove(scree_name)
  
}

###################################################################################################################
# FACTOR ANALYSIS
# Uses nfactors 5, 6, and 7 based on screeplot of final (i.e., filtered) data
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
    
    # Use drive_update to update specific file based on ID number
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

rdata_file <- paste(Sys.Date(), "_all-data-prior-to-CFA_", insert_description, ".RData", sep = "")
save.image(file = rdata_file)
drive_upload(rdata_file, path = as_dribble("REMS_SALG/Results")) # for initial upload

# To output how much variance is accounted for by the factors:
# Or just inspect the table of "Cumulative Var"
#EFA_results$Vaccounted
#EFA_results$Vaccounted[3, f] # i.e., last column of the "cumulative variance" row

# Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#EFA_rawdat_results <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#EFA_rawdat_results$scores

