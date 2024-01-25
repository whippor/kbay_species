#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Species list for Kachemak Bay, AK, USA                                      ##
# Script created yyyy-mm-dd                                                   ##
# Data source: NAME/ORG                                                       ##
# R code prepared by NAME                                                     ##
# Last updated yyyy-mm-dd                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# FILE.csv

# Associated Scripts:
# NONE

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# RECENT CHANGES TO SCRIPT                                                     +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(worrms)
library(janitor)
library(tidystringdist)
library(fuzzyjoin)

# fuction for "%notin%
`%notin%` <- Negate(`%in%`)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# import full species list
raw_spp <- read_csv("data/KBL_Species_List-InProgress - Raw_Species_List.csv")

# import plant/fungus/lichen list for comparison
plant_list <- read_csv("data/Comprehensive_Checklist.csv")

# harmonize with other column names and reduce
reduced_plants <- plant_list %>%
  select("Name",
         "Status",
         "Accepted Name",
         "Family",
         "Name Source",
         "Level") %>%
  rename("Scientific Name" = "Name",
         "status" = "Status",
         "valid_name" = "Accepted Name",
         "family" = "Family",
         "source" = "Name Source",
         "rank" = "Level")
  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SCRAPE WORMS DATA                                                         ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# remove non-marine species
raw_marine <- raw_spp %>%
  filter(SubGroup %notin% c('Terrestrial Mammal',
                            'Terrestrial Plant',
                            'Freshwater Amphibian',
                            'Freshwater Fish',
                            'Terrestrial Amphibian'))

# create unique raw species list
raw_list <- unique(raw_marine$`Scientific Name`)

# break into smaller lists
raw_list_1 <- raw_list[1:125]
raw_list_2 <- raw_list[126:250]
raw_list_3 <- raw_list[251:375]
raw_list_4 <- raw_list[376:500]
raw_list_5 <- raw_list[501:600]
raw_list_6 <- raw_list[601:725]
raw_list_7 <- raw_list[726:825]
raw_list_8 <- raw_list[826:925]
raw_list_9 <- raw_list[926:1025]
raw_list_10 <- raw_list[1026:1100]
raw_list_11 <- raw_list[1101:1138]

# scrape WORMS for species list
TaxWorms_1 <- wm_records_names(name = c(raw_list_1), fuzzy = TRUE)
TaxWormsTib_1 <- data.table::rbindlist(TaxWorms_1)

TaxWorms_2 <- wm_records_names(name = c(raw_list_2), fuzzy = TRUE)
TaxWormsTib_2 <- data.table::rbindlist(TaxWorms_2)

TaxWorms_3 <- wm_records_names(name = c(raw_list_3), fuzzy = TRUE)
TaxWormsTib_3 <- data.table::rbindlist(TaxWorms_3)

TaxWorms_4 <- wm_records_names(name = c(raw_list_4), fuzzy = TRUE)
TaxWormsTib_4 <- data.table::rbindlist(TaxWorms_4)

TaxWorms_5 <- wm_records_names(name = c(raw_list_5))
TaxWormsTib_5 <- data.table::rbindlist(TaxWorms_5)

TaxWorms_6 <- wm_records_names(name = c(raw_list_6))
TaxWormsTib_6 <- data.table::rbindlist(TaxWorms_6)

TaxWorms_7 <- wm_records_names(name = c(raw_list_7))
TaxWormsTib_7 <- data.table::rbindlist(TaxWorms_7)

TaxWorms_8 <- wm_records_names(name = c(raw_list_8))
TaxWormsTib_8 <- data.table::rbindlist(TaxWorms_8)

TaxWorms_9 <- wm_records_names(name = c(raw_list_9))
TaxWormsTib_9 <- data.table::rbindlist(TaxWorms_9)

TaxWorms_10 <- wm_records_names(name = c(raw_list_10))
TaxWormsTib_10 <- data.table::rbindlist(TaxWorms_10)

TaxWorms_11 <- wm_records_names(name = c(raw_list_11))
TaxWormsTib_11 <- data.table::rbindlist(TaxWorms_11)

# combine into single table

all_taxa <- bind_rows(TaxWormsTib_1,
                      TaxWormsTib_2,
                      TaxWormsTib_3,
                      TaxWormsTib_4,
                      TaxWormsTib_5,
                      TaxWormsTib_6,
                      TaxWormsTib_7,
                      TaxWormsTib_8,
                      TaxWormsTib_9,
                      TaxWormsTib_10,
                      TaxWormsTib_11) %>%
  rename(`Scientific Name` = scientificname)

# get list of accepted duplicates to keep
dupes <- all_taxa %>%
   get_dupes(`Scientific Name`) %>%
  select(!dupe_count) %>%
  group_by(`Scientific Name`) %>%
  slice(which.max(as.Date(modified)))

# remove all existing duplicates in main table
minus_dupes <- all_taxa %>%
  filter(`Scientific Name` %notin% dupes$`Scientific Name`)

# join reduced duplicates to main dataset
updated_taxa <- bind_rows(dupes,
                          minus_dupes)

# add timestamp column
updated_taxa$`Date Taxonomy Updated` <- Sys.Date()

# add source column
updated_taxa$source <- "WORMS"

# add columns back into raw_spp data table
worms_taxa <- raw_spp %>%
  left_join(updated_taxa, by =  "Scientific Name") %>%
  mutate_all(funs(str_replace(., "â€™", "'")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CHECK PLANT TAXA                                                          ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plant_match <- reduced_plants %>%
  stringdist_inner_join((raw_spp %>%
                           filter(!is.na(`Scientific Name`))), 
                       by = "Scientific Name",
                       method = "soundex")












#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WRITE CSV                                                                 ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write csv 
write_csv(worms_taxa, "UpdatedTaxa.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####





