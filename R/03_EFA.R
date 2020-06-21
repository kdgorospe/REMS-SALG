# Do preliminary analysis (correlation and scree plots) and exploratory factor analysis
# kdgorospe@gmail.com

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

  ### NOTE: in pre data: "major_unsurecollege_yesno" violates skewness test for normality
  # in post data: "attitudes_workwithothers", "major_unsurecollege_yesno", "understanding_relatetolife", and "understanding_society_pooled" violate skewness

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
# SCREE PLOTS


time_point <- c("pre", "post")
for (i in time_point){
  
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  
  dat_scaled <- scale(tidy_dat[,4:dim(tidy_dat)[2]], center=TRUE, scale=TRUE)
  
  # SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
  # FIX IT - in addition to scree plot, do parallel analysis?
  # See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
  cor_matrix <- cor(dat_scaled, use = "pairwise.complete.obs")
  
  # Print to console:
  cat("For", i, "data:\n")
  
  scree_name <- paste("screeplot_", i, ".pdf", sep = "")
  pdf(file = scree_name)
  fa.parallel(x = cor_matrix, fm = "ml", fa = "fa", n.obs = nrow(dat_scaled)) # "ml" is the maximum likelihood method for "well-behaved" data
  dev.off()
  #drive_upload(scree_name, path = as_dribble("REMS_SALG/")) # for initial upload
  
  if (i == "pre"){
    drive_update(file = as_id("1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT"), media = scree_name)  
  }
  if (i == "post"){
    drive_update(file = as_id("1GXAaVmQoO5tjxb6PNISzwsxFz3FM8JH5"), media = scree_name)  
  }
  
  file.remove(scree_name)
  
}

# LEFT OFF HERE: continue splitting analysis for pre vs post
###################################################################################################################
# FACTOR ANALYSIS



# NOTE: set nfactors to results from Scree plot
# NOTE: rotate = "promax" is what Goodwin 2016 used
# NOTE: fm (factor method, aka factor extraction method): 
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal).
EFA_pre_cordat_5factor <- fa(r = cor_dat_pre, nfactors = 5, rotate = "promax", fm = "ml") 

# Write model outputs to textfile
sink("EFA_pre_cordat_5factor.txt")
EFA_pre_cordat_5factor
sink()

#drive_upload("EFA_pre_cordat_5factor.txt", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for EFA_pre_cordat_5factor.txt: 1xBtWz9dBAap1kjjh00H3liHr1Don_Niu
drive_update(file = as_id("1xBtWz9dBAap1kjjh00H3liHr1Don_Niu"), media = "EFA_pre_cordat_5factor.txt")
file.remove("EFA_pre_cordat_5factor.txt")

# Simplify model outputs: Write just factor loadings to textfile:
sink("EFA_pre_cordat_5factor_loadings.txt")
EFA_pre_cordat_5factor$loadings
sink()

#drive_upload("EFA_pre_cordat_5factor_loadings.txt", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for EFA_pre_cordat_5factor_loadings.txt: 1JUY5F8OWKNTKgpdN2HMtpTXAUDw2XUFZ
drive_update(file = as_id("1JUY5F8OWKNTKgpdN2HMtpTXAUDw2XUFZ"), media = "EFA_pre_cordat_5factor_loadings.txt")
file.remove("EFA_pre_cordat_5factor_loadings.txt")


# To find out how much variance is accounted for by the factors:
EFA_pre_cordat_5factor$Vaccounted
EFA_pre_cordat_5factor$Vaccounted[3, 5]


# Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#EFA_pre_rawdat_5factor <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#EFA_pre_rawdat_5factor$scores



