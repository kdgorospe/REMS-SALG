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
  # REMOVE ALL EVALUATION QUESTIONS (only run in the post test, and not part of our pre vs post framework)
  select(!starts_with("evaluation")) %>% # note: for some reason starts_with("evaluation") == FALSE does not work (not tidy?)
  # REMOVE VARIABLES WITH LIMITED PAIRWISE COMPARISONS (these labs were run only a few times)
  select(-c("understanding_ethology", "understanding_aquaculture"))

tidy_dat_post <- tidy_dat_all %>%
  filter(test=="post") %>% 
  # REMOVE ALL EVALUATION QUESTIONS (only run in the post test, and not part of our pre vs post framework)
  select(!starts_with("evaluation")) %>% 
  # REMOVE VARIABLES WITH LIMITED PAIRWISE COMPARISONS (these labs were run only a few times)
  select(-c("understanding_ethology", "understanding_aquaculture"))
  
  

time_point <- c("pre", "post")

for (i in time_point){
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  cor_matrix <- cor(tidy_dat[,4:dim(tidy_dat)[2]], use="pairwise.complete.obs")
  cor_csv <- paste("cormatrix_", i, ".csv", sep="")
  write.csv(cor_matrix, cor_csv, row.names = TRUE)
  # drive_upload(cor_csv, path = as_dribble("REMS_SALG/")) # for initial upload
  
  cor_pdf <- paste("corrplot_", i, ".pdf", sep="")
  pdf(cor_pdf)
  corrplot(cor_matrix, tl.col="black", tl.cex=0.75)
  dev.off()
  #drive_upload(cor_pdf, path = as_dribble("REMS_SALG/")) # for initial upload
  
  if (i=="pre"){ 
    drive_update(file = as_id("1mDIq5JfDSuiZIAehyy6UZX37l-5cgU-t36pM87Iix8s"), media = cor_csv)
    drive_update(file = as_id("1FMLHbE7sbnLbqHm6eeiWtU-O_xVks66-"), media = cor_pdf)
  }
  if (i=="post"){ 
    drive_update(file = as_id("1yNpDJoBgd_taaJ8eO1klt4mKwEbEoj5b2UhTfsbpXYM"), media = cor_csv)
    drive_update(file = as_id("10qsyqLTg4GqHFrGIARd6m1wconWu3jpT"), media = cor_pdf)
  }
  file.remove(cor_csv)
  file.remove(cor_pdf)
  
}



## LEFT OFF HERE - need to continue splitting analyses for pre and post

## From Godwin 2013: Test for normality: The skew and kurtosis were evaluated for each item to ensure that the assumptions of multivariate normality were not severely violated 
## Use describe() to examine normality of each variable ()
## specify that we should use the describe function from psych package (shares namespace with other packages, e.g., Hmisc)
dat_describe <- psych::describe(dat_analysis[,4:dim(dat_analysis)[2]])

# According to Goodwin 2016, these variables should be removed because they violate assumption of normality: 
dat_describe[abs(dat_describe$skew) >= 2,]
dat_describe[abs(dat_describe$kurtosis) >= 7,]

write.csv(dat_describe, "dat_describe.csv", row.names = TRUE)
#drive_upload("dat_describe.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for dat_describe.csv: 1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ
drive_update(file = as_id("1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ"), media = "dat_describe.csv")
file.remove("dat_describe.csv")



dat_scaled <- scale(dat_analysis[,4:dim(dat_analysis)[2]], center=TRUE, scale=TRUE)
dat_scaled <- cbind(dat_analysis[,1:3], dat_scaled)

# FIX IT - should i rerun describe and corrplot on separate pre and post dataframes?
# Split pre and post, do separate analyses for each
scaled_pre <- dat_scaled %>%
  filter(test == "pre") %>%
  # For PRE data, remove "evaluation" questions, these are only part of the POST data
  select_if(grepl("evaluation", names(.))==FALSE) %>%
  # FIX IT - how is analysis affected by NAs? Still runs even if we keep this in, but removing it for now
  # For preliminary analysis of PRE data, remove "understanding_coralclimate, coralskel, and diversity" questions which only showed up in 2017 and 2018
  select(-c(understanding_coralclimate, understanding_coralskel, understanding_diversity))
  
scaled_post <- dat_scaled %>%
  filter(test == "post")

# FIX IT - Analyze just the pre questions for now:

# SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
# FIX IT - in addition to scree plot, do parallel analysis 
# See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
cor_dat_pre <- cor(scaled_pre[,4:dim(scaled_pre)[2]], use = "pairwise.complete.obs")

pdf(file="pre_screeplot.pdf")
fa.parallel(x = cor_dat_pre, fm = "ml", fa = "fa") # "ml" is the maximum likelihood method for "well-behaved" data
dev.off()

#drive_upload("pre_screeplot.pdf", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for pre_screeplot.pdf: 1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT
drive_update(file = as_id("1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT"), media = "pre_screeplot.pdf")
file.remove("pre_screeplot.pdf")

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



