# kdgorospe@gmail.com
# Clean REMS data
rm(list=ls())
library(tidyverse)
library(googledrive)

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
  dat_object <- paste("post", dat_year, sep = "")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Read pre data files into R
pre_grep <- grep("PRE", all_filenames)

for (i in pre_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("pre", dat_year, sep = "")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Remove files from local computer
for (i in post_grep) {
  file.remove(all_filenames[i])
}
for (i in pre_grep) {
  file.remove(all_filenames[i])
}


