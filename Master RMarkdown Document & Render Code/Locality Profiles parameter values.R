##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(knitr)
library(rmarkdown)
library(here)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Source in functions code
source(here("Master RMarkdown Document & Render Code/Global Script.R"))


# Set file path
data_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
lp_path <- "/conf/LIST_analytics/West Dunbartonshire/Locality Profiles Combined/"
output_dir <- path(lp_path, "Master RMarkdown Document & Render Code", "Output")


# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# or create a vector for multiple e.g. `c("Angus", "West Lothian")`
# For a larger test, use the below to produce profiles for HSCPs likely to cause issues.
# source("Master RMarkdown Document & Render Code/find_hscp_outliers.R")
# hscp_list <- outlier_hscps
HSCP <- "West Dunbartonshire"


# NOTE - This checks that it exactly matches the lookup
stopifnot(all(HSCP %in% unique(lookup[["hscp2019name"]])))

# list of localities 
  locality_list <- lookup |>
    filter(hscp2019name == HSCP) |>
    pull(hscp_locality)
  
# HB name
  HB <- lookup |> 
    filter(hscp2019name == HSCP) |> 
    distinct(hb2019name) |> 
    pull()

  