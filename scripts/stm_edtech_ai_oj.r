################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < create_cb_geo_hs_visits.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/22/2025
## [ DESC ] < load data on HS characteristics, visits to HS, and geomarkets>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
#options(width = 160)

### LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(labelled)
library(stm)
library(tidytext)

### DIRECTORY PATHS
data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

df <- read_excel(
  path = file.path(data_dir,'PitchBook_Search_Result_Columns_2025_09_17_09_38_28.xlsx'),
  sheet = "Data",
  skip = 7,
  col_names = TRUE
) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%   # replace spaces with underscores
  rename_with(tolower)                    # lowercase

df %>% glimpse()

# investigate units of observation

df %>% group_by(company_id) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group)
df %>% group_by(deal_id) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group)

