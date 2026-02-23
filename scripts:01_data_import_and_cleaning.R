# ============================================
# Temiscouata Project – Data Import & Cleaning
# MSc Thesis
# Author: Sarah Minyoung Song
# ============================================
# This script:
# 1. Imports raw phenotypic measurements from Excel
# 2. Cleans column names and data types
# 3. Combines historical (CMN) and contemporary field data
# 4. Saves a processed dataset for downstream analyses

install.packages(c("tidyverse", "readxl", "dplyr", "purrr"))
install.packages("janitor", dependencies = TRUE)

library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(janitor)

# Path to raw data
data_path <- "data/temiscouata_measurements.xlsx"

# Read individual sheets
data_21_23 <- read_excel(data_path, sheet = "2021-23") |> 
  mutate(year_group = "2021_2023")

data_2025 <- read_excel(data_path, sheet = "2025") |> 
  mutate(year_group = "2025")

data_cmn <- read_excel(data_path, sheet = "CMN 1980-1981") |> 
  mutate(year_group = "1980_1981")

# Fix all numeric columns and apply to each dataset
numeric_cols <- c(
  "sl", "body_depth", "ap_length", "ap_width", 
  "jaw_length", "ppl", "ppw", "dorsal_1",
  "dorsal_2", "pspine_r", "pspine_l", "pspine_num",
  "pscore_r", "pscore_l"
)

fix_numeric <- function(df) {
  df |> 
    mutate(across(
      all_of(numeric_cols),
      ~ as.numeric(.x)
    ))
}

data_21_23 <- fix_numeric(data_21_23)
data_2025  <- fix_numeric(data_2025)
data_cmn   <- fix_numeric(data_cmn)

# Combine everything into one master dataset
stickleback_all <- bind_rows(
  data_21_23,
  data_2025,
  data_cmn
)

# Quick checks:
glimpse(stickleback_all)
summary(stickleback_all$year)
table(stickleback_all$year_group)

# Remove empty note columns
stickleback_all <- stickleback_all |> 
  select(-starts_with("unnamed"))

# Make sure numeric traits are numeric
traits <- c(
  "sl", "body_depth", "ap_length", "ap_width", 
  "jaw_length", "ppl", "ppw", "dorsal_1",
  "dorsal_2", "pspine_r", "pspine_l", "pspine_num",
  "pscore_r", "pscore_l"
)

stickleback_all <- stickleback_all |> 
  mutate(across(all_of(traits), as.numeric))

# Save a clean analysis-ready dataset
write_csv(
  stickleback_all,
  "data/stickleback_all_measurements.csv"
)

# From now on, later scripts can just load this: 
stickleback_all <- read_csv("data/stickleback_all_measurements.csv")

# End of Script 01
# No data values are altered in this script.