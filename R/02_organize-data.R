# Import REMS data (student responses) and assign "concept" labels to more easily identify questions across years
# Example: Data about student attitudes towards a career in marine science are from question 3.3 in 2013-2016 and from question 4.3 in 2017-2018 is given the label "attitudes_career"
# kdgorospe@gmail.com

library(tidyverse)
library(googledrive)

### The following creates a list of question IDs to be used for specifying the data structure
question_id_list = list()
# Create list of questions and ids
# start with i=2 because i=1 is ID column
for (i in 2:length(names(question_dt))){
  question_id_set <- tibble(id = question_dt$id,
                            question = question_dt[,i]) %>%
    drop_na() %>%
    mutate(q_number = names(question_dt[i])) %>%
    mutate(id = str_replace(id, pattern="q_", replacement = "")) %>%
    separate(id, into = c("year", "test"))
  question_id_list[[i]] <- question_id_set
}

question_id_dt <- rbindlist(question_id_list)



# note: file IDs saved as text file in google drive
# post data:
post_ids <- c("1Rg82pkzL6lOMi1QuSeOLBda6Gox4-BSk", 
              "1TeSyc8oH2ElQvwuBZMke60h5w0mhdZJz",
              "1xYknTNdXLhCEHSamQtnB9ksk1_Sm6N8_",
              "199SByfuEhAp0tatoU3iLcYuiRyg2fmoC",
              "1-hIf5bHKwz2-kmzur18h_Qhs6Q-c_lxj",
              "15bde4B0mFTlI1Hu6EwSW5_z3bNR5HAzx")

# pre data:
pre_ids <- c("1BMd7zVU2NPK6SC8FTs29vcCzABZKZHc0",
             "1NZbFXSr0xiQCzhydtQsfnm3TWCvcPf-D",
             "1c2VJv8W8JbEiDQX_C2oPRUSJ37jsl_Yf",
             "1TrXDAZwVeaOXUSVLpYwctwze0tCin7DE",
             "1kX_Bl9E9xj8Zs4SfmvPl8ZH1-42XP2mg",
             "1Qz1tUDXELybr81liRP8KvQglve_n57A9")

# Download files to local computer
for (i in 1:length(post_ids)) {
  drive_download(as_id(post_ids[i]), overwrite=TRUE) 
}
for (i in 1:length(pre_ids)) {
  drive_download(as_id(pre_ids[i]), overwrite=TRUE) 
}

# Read post data files into R
all_filenames <- list.files()
post_grep <- grep("POST", all_filenames)

for (i in post_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("a", dat_year, "post", sep = "_")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Read pre data files into R
pre_grep <- grep("PRE", all_filenames)

for (i in pre_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("a", dat_year, "pre", sep = "_")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Remove files from local computer
for (i in post_grep) {
  file.remove(all_filenames[i])
}
for (i in pre_grep) {
  file.remove(all_filenames[i])
}

# Combine and reshape answer data long format (each row is a single response by a single student)
answer_objs <- apropos("a_20") # find all the "answer" objects
answer_list = list()
for (i in answer_objs){
  answer_raw_i <- get(i)
  answer_tidy_i <- answer_raw_i %>% 
    select(-Elapsed) %>%
    select_if(is.numeric) %>%
    pivot_longer(cols = -Number,
                 names_to = "q_number", 
                 values_to = "answer") %>%
    mutate(id = i) %>%
    mutate(id = str_replace(id, pattern="a_", replacement = "")) %>%
    separate(id, into = c("year", "test")) %>%
    mutate(q_number = str_replace(q_number, pattern="X", replacement=""))
  answer_list[[i]] <- answer_tidy_i
}

answer_dt <- rbindlist(answer_list)


# Bind answer data to questions
full_dat <- answer_dt %>%
  inner_join(question_id_dt, by = c("year", "test", "q_number")) %>% # inner_join, check that dim(full_dat) == dim(answer_dt)
  arrange(q_number, year, test)

# START ASSIGNING "CONCEPTS" to each question based on grepping wording
coded_dat <- full_dat %>% 
  mutate(concept = case_when(# question heading: "UNDERSTANDING"
                             question=="The ecology of coral reefs" ~ "understanding_ecology",
                             question=="The scientific process" ~ "understanding_sciprocess",
                             question=="How increasing carbon dioxide in the atmosphere contributes to ocean acidification and how this might affect the health of corals and many marine organisms" ~ "understanding_oceanacid_pooled",
                             question=="The effects of ocean acidification on marine organisms" ~ "understanding_oceanacid_split_organisms",
                             question=="The effects of atmospheric carbon dioxide on ocean chemistry" ~ "understanding_oceanacid_split_chemistry",
                             question=="The challenges of growing and harvesting fish in captivity, also known as aquaculture" ~ "understanding_aquaculture",
                             question=="How an ethologist records data about animal behavior and how the form of an animal and where it lives can influence its behavior" ~ "understanding_ethology",
                             question=="The effects of water quality on the fertilization processes of marine organisms, particularly sea urchins" ~ "understanding_fertilization",
                             question=="The physics of sound and how many marine organisms, such as snapping shrimp, use sound in the ocean for different purposes" ~ "understanding_sound_pooled",
                             question=="The general physics of sound" ~ "understanding_sound_split_general",
                             question=="How many marine organisms, such as snapping shrimp, use sound in the ocean" ~ "understanding_sound_split_organisms",
                             question=="The significance of organism diversity in ecosystem health" ~ "understanding_diversity",
                             question=="The role of coral skeletal structure in reef ecology" ~ "understanding_coralskel",
                             question=="The effects of climate change on coral growth and survival" ~ "understanding_coralclimate",
                             question=="How ideas we will explore in this class relate to your own everyday life" | question=="Marine science concepts relate to my own everyday life" ~ "understanding_relatetolife",
                             question=="How studying this subject is important to society and helps people address real world issues" | question=="How studying marine science is important to society and helps people address real world issues" ~ "understanding_society_pooled",
                             question=="The study of marine science helps people address real world issues" ~ "understanding_society_split_realissues",
                             question=="The study of marine science is important to society" ~ "understanding_society_split_society",
                             
                             # question heading: "SKILLS"
                             question=="Use the scientific process to ask a question and develop a hypothesis" ~ "skills_developH0",
                             question=="Develop an experiment to test a hypothesis" ~ "skills_testH0",
                             question=="Analyze and interpret experimental data to evaluate a hypothesis" ~ "skills_evalH0",
                             question=="Communicate the results of a research project in written and/or oral format" ~ "skills_communicate_pooled",
                             question=="Communicate the results of a research project in written format" ~ "skills_communicate_split_written",
                             question=="Communicate the results of a research project in oral format" ~ "skills_communicate_split_oral",
                             question=="Work effectively with others" ~ "skills_withothers",
                             
                             # question heading: "ATTITUDES"
                             question=="Enthusiastic about the marine science" | question=="Enthusiastic about marine science" ~ "attitudes_enthusiastic",
                             question=="Interested in discussing marine science with friends or family" ~ "attitudes_discussing",
                             question=="Interested in taking or planning to pursue a career in marine science" | question=="Interested in a career in marine science" ~ "attitudes_career",
                             question=="Confident that I understand marine science" ~ "attitudes_confidentunderstanding",
                             question=="Confident that I can use the scientific process to execute a research project" ~ "attitudes_confidentresearch",
                             question=="Willing to work with others to accomplish a research project" ~ "attitudes_workwithothers",
                             
                             # question heading: "INTEGRATION"
                             question=="Connecting key ideas I learn in my classes with other knowledge" ~ "integration_connectingknowledge",
                             question=="Applying what I learn in classes to other situations" ~ "integration_applyingknowledge",
                             
                             # question heading: "MAJOR"
                             question=="Marine science" ~ "major_marinesci_yesno",
                             question=="Something in the sciences" ~ "major_science_yesno",
                             question=="A major not in the sciences" ~ "major_notscience_yesno",
                             question=="Undecided as of now" ~ "major_undecided_yesno",
                             question=="Not sure I want to attend college" ~ "major_unsurecollege_yesno",
                             question=="What do you want to major in college?" ~ "major_multiplechoice",
                             
                             # question heading: "EVALUATION"
                             question=="Coral reef ecology lecture/ photoquad reef monitoring activity" ~ "evaluation_coralreefeco",
                             question=="Ocean acidification lecture/lab activity" ~ "evaluation_oceanacid",
                             question=="Plankton ecology lecture/field collecting/lab activity" ~ "evaluation_planktoneco",
                             question=="Hawaiian bobtail squid ethology lecture/lab activity" ~ "evaluation_ethology",
                             question=="The bioacoustics of snapping shrimp lecture/lab activity" ~ "evaluation_bioacoustics",
                             question=="Opakapaka spawning lecture/field collecting/lab activity" ~ "evaluation_aquaculture",
                             question=="Sea urchin fertilization lecture/field collecting/ lab activity" ~ "evaluation_fertilization",
                             question=="Scientific method lecture and scientific paper homework assignment and in class discussion" | question=="Scientific method lecture and guided inquiry activity" | question=="Scientific method lecture and guided inquiry group activity" ~ "evaluation_scimethod",
                             question=="My participation in the group research project was enjoyable" ~ "evaluation_groupresearch_enjoyable",
                             question=="As a result of my group research project experience, I feel confident that I can execute a research project on my own" ~ "evaluation_groupresearch_confident",
                             
                             # question heading: "EVALUATION (Guest lecturers and career day)"
                             question=="Guest researcher lecture Ariana Snow and her presentation on coral reef monitoring" ~  "evaluation_guestspeaker_coraleco_as",
                             question=="Guest researcher lecture Kelvin Gorospe and his presentation on coral reef ecology" ~ "evaluation_guestspeaker_coraleco_kg",
                             question=="Guest researcher lecture Keisha Bahr and her presentation on ocean acidification and reef growth" ~ "evaluation_guestspeaker_oceanacid_kb",
                             question=="Guest researcher lecture Ku'ulei Rogers and her presentation on ocean acidification and reef growth" ~ "evaluation_guestspeaker_oceanacid_kr",
                             question=="Guest researcher lecture Christine Carrier and her presentation on ethology and shark sensory" ~ "evaluation_guestspeaker_ethology_cc", 
                             question=="Guest researcher lecture Jenny Fung and her presentation on sea urchin fertilzation and water quality" ~ "evaluation_guestspeaker_fertilization_jf",
                             question=="Guest researcher lecture Adam Smith and his presentation on the bioacoustics of marine mammals" ~ "evaluation_guestspeaker_bioacoustics_as",
                             question=="Guest researcher lecture Clyde Tamaru and his presentation on aquaponics and aquaculture" ~ "evaluation_guestspeaker_aquaculture_ct",
                             question=="Guest researcher lecture Aude Pacini and her presentation on the bioacoustics of marine mammals" ~ "evaluation_guestspeaker_bioacoustics_ap",
                             grepl(pattern="Career day in class presentations*", full_dat$question) ~ "evaluation_careerday")) %>%
  arrange(concept, test, q_number, year)



write.csv(coded_dat, "coded_dat.csv", row.names = FALSE)
#drive_upload("coded_dat.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for coded_dat: 1ztPIrSmNdy0o-0yJpL_oDybcEUEj1vYEy2zrLm0PhyE
drive_update(file = as_id("1ztPIrSmNdy0o-0yJpL_oDybcEUEj1vYEy2zrLm0PhyE"), media = "coded_dat.csv")
file.remove("coded_dat.csv")

### Before doing any analysis, clean data: 

# 1 - Standardize scale and replace numbers with NA's: For some questions the true Likert scale is 1 to 5, and 9 = NA; For other questions the true Likert scale is 2 to 6, and 1 = NA

# SOLUTION: set all scales to be 2 to 6 with 1 as NA; i.e., for questions where 9 = NA, reset "1 to 5" as "2 to 6" and change 9 to NA
# For other questions, change 1 to NA
# From Christine's email 2021 Apr 02
# All the items are 5-point scale Likert questions with an additional “not applicable” choice. The 'N/A' either comes first and is scored 1 on the CSV file (the rest of the scale being 2-6), or comes last and is scored as a 9 (the rest of the scale being 1-5). 
# If you look at an instrument page on one of the SALG CSV’s you can see what I mean. For my analyses, I just transformed all the scales to have the ’N/A’ option as a 1, with the rest of the scale 2-6. But now I’m going to delete the ’N/A’ option entirely and focus on the “true” 5-point Likert scale.

# Here is the list of questions with the ’N/A’ last (and scored as a 9):
# 2013 - POST 6.1-11
# 2014 - POST 6.1-11, 7.1-2
# 2015 - Student POST 6.1-11, 7.1-2; Mentor PRE and POST 6.1-5
# 2016 - Student POST 6.1-8, 7.1-2; Mentor PRE 6.1-7, Mentor POST 6.1-8, 7.1-2, 8.1-7
# 2017/18 - Student PRE 2.1-3, Student POST 2.1-3, 7.1-6, 8.1-2; Mentor PRE 2.1-3, 7.1-7, Mentor POST 2.1-3, 7.1-6, 8.1-2, 9.1-7

# In addition, remove iClicker ID numbers that correspond to mentors (only want to analyze students): only applies to year 2014
pre_mentor <- c(25861, 23860, 51148)
post_mentor <- c(54142, 14983, 38861)
coded_and_standardized_dat <- coded_dat %>%
  #mutate_all(~na_if(., 9)) #%>%
  mutate_if(is.numeric, ~ na_if(., 9)) %>%
  mutate(answer = as.numeric(answer)) %>%
  # Using Christine's list above, for all questions with NA scored as 9, move answers up by 1 - i.e., ultimately, want all scales to be 2 to 6
  mutate(answer = case_when(q_number %in% c("6.1", "6.2", "6.3", "6.4", "6.4", "6.5", "6.6", "6.7", "6.8", "6.9", "6.10", "6.11") & test == "post" & year %in% c("2013", "2014", "2015") ~ answer + 1,
                            q_number %in% c("6.1", "6.2", "6.3", "6.4", "6.4", "6.5", "6.6", "6.7", "6.8") & test == "post" & year == "2016" ~ answer + 1,
                            q_number %in% c("7.1", "7.2") & test == "post" & year %in% c("2014", "2015", "2016") ~ answer + 1,
                            q_number %in% c("2.1", "2.2", "2.3") & year %in% c("2017", "2018") ~ answer + 1, # both pre and post
                            q_number %in% c("7.1", "7.2", "7.3", "7.4", "7.5", "7.6", "8.1", "8.2") ~ answer + 1,
                            TRUE ~ answer)) %>%
  # After moving all relevant scales to be 2 to 6, all "1"s are NAs EXCEPT for concepts concerning their major (e.g., "major_marinesci_yesno", "major_undecided_yes_no", "major_multiplechoice", etc)
  # Convert all questions about college major to NAs
  mutate(answer = if_else(str_detect(concept, "major"), true = NaN, false = answer)) %>%
  # After converting all questions about college major to NAs, NOW **ALL** "1"s are NAs
  #mutate_all(~na_if(., 1)) %>%
  mutate_if(is.numeric, ~ na_if(., 1)) %>%
  # Remove iClicker ID numbers that correspond to mentors (only want to analyze students): only applies to year 2014
  filter((Number %in% pre_mentor & year == 2014 & test == "pre")==FALSE) %>%
  filter((Number %in% post_mentor & year == 2014 & test == "post")==FALSE)
  

# DATA CLEANING CHECKS: 
# no more "1"s because they've all been conerted to NAs
table(coded_and_standardized_dat$concept, coded_and_standardized_dat$answer)
coded_and_standardized_dat %>%
  filter(answer == 1)
# Also no more "9"s because they've all been converted to NAs
coded_and_standardized_dat %>%
  filter(answer == 9)

# RE-SCALE DATA 
# Now that data is standardized as 2 to 6, re-scale to be 1 to 5 since this is a more "standard" Likert scale
coded_and_standardized_dat <- coded_and_standardized_dat %>%
  mutate(answer = answer - 1)


write.csv(coded_and_standardized_dat, "coded_and_standardized_dat.csv", row.names = FALSE)
#drive_upload("coded_and_standardized_dat.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for coded_dat: 
drive_update(file = as_id("1-AUf5ojqAEHQ5RSDitKVYAOo29xJDdx4"), media = "coded_and_standardized_dat.csv")
file.remove("coded_and_standardized_dat.csv")
