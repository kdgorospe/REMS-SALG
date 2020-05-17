# Import REMS data and assign "concept" labels to easily identify data across years
# Example: Data about student attitudes towards a career in marine science are from question 3.3 in 2013-2016 and from question 4.3 in 2017-2018 is given the label "attitudes_career"
# kdgorospe@gmail.com


require(tidyverse)
require(googledrive)

### THe following creates a list of question IDs to be used for specifying the data structure
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
                             question=="The scientific process" ~ "understandng_sciprocess",
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

# 1 - Response value 9 = NA
coded_and_standardized_dat <- coded_dat %>%
  mutate_all(~na_if(., 9))

# 2 - Deal with questions with changing Likert Scales
# For 2017 and 2018, these questions changed to Likert scale ONE to SIX: understanding_relatetolife, understanding_society_split_realissues, understanding_society_split_society (additionally, the latter two were "pooled" but we'll deal with that in the next cleaning step)
# Scale them to be between ONE and SIX by multiplying all responses by 6/5
# View the problem:
#coded_and_standardized_dat %>%
#  filter(str_detect(concept, pattern = "understanding_relatetolife|understanding_society_pooled|understanding_society_split")) %>%
#  group_by(concept, year) %>%
#  summarise(max = max(answer, na.rm = TRUE))

# Scale 2017 and 2018 data to be ONE to SIX (2017 and 2018 data Likert scale was 1 to 5; but prior to 2017, Likert scale was 1 to 6)
coded_and_standardized_dat <- coded_and_standardized_dat %>%
  mutate(answer = case_when((str_detect(concept, "understanding_relatetolife") & year>=2017) ~ answer * 6/5,
                            (str_detect(concept, "understanding_society_split_realissues") & year>=2017) ~ answer * 6/5,
                            (str_detect(concept, "understanding_society_split_society") & year>=2017) ~ answer * 6/5,
                            TRUE ~ as.numeric(answer)))

# 3 - DEAL WITH inconsistent questions:
# SPLIT vs POOLED questions: In some years, the question wording is: "Communicate the results of a research project in written and/or oral format skills" (POOLED) 
# while in other years the wording is split: "Communicate the results of a research project in written format" and "Communicate the results of a research project in oral format"

# View pooled vs split data:
#coded_dat %>%
#  filter(str_detect(coded_dat$concept, "pooled"))
#coded_dat %>% 
#  filter(str_detect(coded_dat$concept, "split"))

# Mutate the column "concept" so that corresponding "split" questions have the same label, allowing them to be grouped together, and for their answers to be averaged
# i.e., mutate "skills_communicate_split_written" and "skills_communicate_split_oral" to "skills_communicate_split"

coded_and_standardized_dat <- coded_and_standardized_dat %>%
  mutate(concept = case_when(str_detect(concept, "split") ~ str_replace(concept, pattern="(split_.*)", replacement = "split"),
                             TRUE ~ concept)) %>%
  group_by(Number, year, test, concept) %>%
  summarize(pooled_answer = mean(answer), n_question = n()) 

# Now that answers have been pooled, mutate column "concept" so that "split" questions now say "pooled" (indicating that they can be analyzed together downstream)
coded_and_standardized_dat <- coded_and_standardized_dat %>%
  mutate(concept = case_when(str_detect(concept, "split") ~ str_replace(concept, pattern="split", replacement = "pooled"),
                             TRUE ~ concept))

# 4 - Deal with questions about Major and College plans which changed from Yes/NO (2013 - 2017) to multiple choice in 2018
# CAVEAT: when the question was YES/NO, students were allowed to indicate multiple majors (they could say yes multiple times as opposed to just selecting one major)
# View the problem:
#coded_and_standardized_dat %>%
#  filter(concept %in% c("major_marinesci_yesno", "major_notscience_yesno", "major_science_yesno", "major_undecided_yesno", "major_unsurecollege_yesno")) %>%
#  arrange(year, test, Number) %>%
#  print(n=dim(.)[1])


# Code below partially addresses this by converting their response to the MULTIPLE CHOICE question into YES answers
# But doesn't assume whether the student would answer YES/NO to any of the other multiple choices
# Example: if student said their preferred major was Marine Science (multiple choice), this is converted to a "YES" to the question, Are you interested in majoring in Marine science? 
# But leaves BLANK all the other questions (Are you interested in majoring in a science? non-science? undecided? unsure about college?)
multichoice_to_yesno <- coded_and_standardized_dat %>%
  filter(concept == "major_multiplechoice") %>%
  # Use their answer to the multiple choice to assign corresponding YES/NO concept
  mutate(concept = case_when(pooled_answer == 1 ~ "major_marinesci_yesno",
                             pooled_answer == 2 ~ "major_science_yesno",
                             pooled_answer == 3 ~ "major_notscience_yesno",
                             pooled_answer == 4 ~ "major_undecided_yesno",
                             pooled_answer == 5 ~ "major_unsurecollege_yesno",
                             TRUE ~ concept)) %>%
  # Now convert all answers to YES = 1
  mutate(pooled_answer = 1)


# Now filter out old answers (multiple choice) and replace them with the new YES/NO versions:
coded_and_standardized_dat <- coded_and_standardized_dat %>% 
  filter(concept != "major_multiplechoice") %>%
  rbind(multichoice_to_yesno) %>%
  arrange(concept, test, year, Number)


write.csv(coded_and_standardized_dat, "coded_and_standardized_dat.csv", row.names = FALSE)
#drive_upload("coded_and_standardized_dat.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for coded_dat: 1NfWblXuSW_gwPqfteP9tzxe5A96kI94uuJb-7PGyCcE
drive_update(file = as_id("1NfWblXuSW_gwPqfteP9tzxe5A96kI94uuJb-7PGyCcE"), media = "coded_and_standardized_dat.csv")
file.remove("coded_and_standardized_dat.csv")

  
